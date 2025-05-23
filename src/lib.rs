use std::collections::BTreeMap;

use camino::Utf8PathBuf;
use color_eyre::eyre::Context;
use dashmap::DashMap;
use ordered_float::OrderedFloat;
use rnix::{
    NodeOrToken, SyntaxNode, TextRange,
    ast::{Expr, HasEntry},
};
use rowan::{GreenNode, ast::AstNode};
use salsa::{Accumulator, Storage};

#[salsa::db]
pub trait Db: salsa::Database {
    fn input(&self, path: Utf8PathBuf) -> color_eyre::Result<NixInputFile>;
}

#[salsa::db]
#[derive(Clone)]
pub struct NixEvalDatabase {
    storage: salsa::Storage<Self>,
    files: DashMap<Utf8PathBuf, NixInputFile>,
}

impl Default for NixEvalDatabase {
    fn default() -> Self {
        Self::new()
    }
}

impl NixEvalDatabase {
    pub fn new() -> Self {
        Self {
            storage: Storage::default(),
            files: DashMap::new(),
        }
    }
}

#[salsa::db]
impl salsa::Database for NixEvalDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
}

#[salsa::db]
impl Db for NixEvalDatabase {
    fn input(&self, path: Utf8PathBuf) -> color_eyre::Result<NixInputFile> {
        let path = path.canonicalize_utf8()?;

        let file = match self.files.entry(path.clone()) {
            dashmap::Entry::Occupied(entry) => *entry.get(),
            dashmap::Entry::Vacant(entry) => {
                let content = std::fs::read_to_string(&path)
                    .wrap_err_with(|| format!("Failed to read {path}"))?;

                *entry.insert(NixInputFile::new(self, path, content))
            }
        };

        Ok(file)
    }
}

#[salsa::input]
pub struct NixInputFile {
    pub path: Utf8PathBuf,
    #[returns(ref)]
    pub contents: String,
}

#[salsa::accumulator]
#[derive(Debug)]
pub struct ParseErrors(pub rnix::parser::ParseError);

#[salsa::tracked]
#[derive(Debug)]
pub struct NixFile<'db> {
    #[tracked]
    #[returns(ref)]
    pub root_node: GreenNode,

    #[returns(ref)]
    links: Vec<NixFile<'db>>,
}

impl<'db> NixFile<'db> {
    fn get_root(&self, db: &dyn Db) -> rnix::Root {
        let root_node = SyntaxNode::new_root(self.root_node(db));
        rnix::Root::cast(root_node).unwrap()
    }
}

#[salsa::tracked]
pub fn parse_file(db: &dyn Db, file: NixInputFile) -> NixFile<'_> {
    let contents = file.contents(db);
    let tokenize = rnix::tokenize(&contents);

    let (parsed, errors) = rnix::parser::parse(tokenize.into_iter());

    for error in errors {
        ParseErrors(error).accumulate(db);
    }

    NixFile::new(db, parsed, vec![])
}

#[salsa::tracked]
#[derive(Debug)]
pub struct NixEvalResult<'db> {
    pub value: NixValue<'db>,
}

#[salsa::accumulator]
#[derive(Debug)]
pub struct EvalError(String);

impl AsRef<str> for EvalError {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Value<'db> {
    Number(i64),
    Float(OrderedFloat<f64>),
    Bool(bool),
    String(StringWithContext<'db>),
    Function(Function<'db>),
    Null,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Function<'db> {
    expr: NixExprId<'db>,
    ctx: NixVarContext<'db>,
}

impl<'db> Function<'db> {
    fn evaluate(&self, db: &'db dyn Db, value: NixValue<'db>) -> NixValue<'db> {
        NixValue::new(db, Value::Null, value.addr(db))
    }
}

impl<'db> Value<'db> {
    pub fn as_number(&self) -> Option<&i64> {
        if let Self::Number(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_float(&self) -> Option<&OrderedFloat<f64>> {
        if let Self::Float(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_bool(&self) -> Option<&bool> {
        if let Self::Bool(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_string(&self) -> Option<&StringWithContext<'db>> {
        if let Self::String(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the value is [`Null`].
    ///
    /// [`Null`]: Value::Null
    #[must_use]
    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct StringWithContext<'db> {
    string: String,
    context: Vec<Utf8PathBuf>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct NixAddr<'db> {
    offset: TextRange,
    file: NixFile<'db>,
}

impl NixAddr<'_> {
    fn to_syntax_node(self, db: &dyn Db) -> SyntaxNode {
        let root = self.file(db).get_root(db);

        let not = root.syntax().covering_element(self.offset(db));
        match not {
            NodeOrToken::Node(n) => n,
            NodeOrToken::Token(t) => t.parent().unwrap(),
        }
    }

    fn with<'db>(db: &'db dyn Db, addr: NixAddr<'db>, node: SyntaxNode) -> NixAddr<'db> {
        let file = addr.file(db);

        NixAddr::new(db, node.text_range(), file)
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct NixValue<'db> {
    pub value: Value<'db>,
    addr: NixAddr<'db>,
}

pub fn get_value<'db>(db: &'db dyn Db, val: NixValue<'db>) -> Value<'db> {
    val.value(db)
}

#[salsa::tracked]
#[derive(Debug)]
pub struct NixVarContext<'db> {
    addr: NixAddr<'db>,
    own_vals: BTreeMap<String, NixAddr<'db>>,
    parent_ctx: Vec<NixVarContext<'db>>,
}

impl<'db> NixVarContext<'db> {
    fn add(
        &self,
        db: &'db dyn Db,
        addr: NixAddr<'db>,
        entries: impl Iterator<Item = rnix::ast::Entry>,
    ) -> NixVarContext<'db> {
        let own_vals = entries.map(|entry| match entry {
            rnix::ast::Entry::Inherit(_inherit) => todo!(),
            rnix::ast::Entry::AttrpathValue(attrpath_value) => {
                let path = attrpath_value
                    .attrpath()
                    .unwrap()
                    .syntax()
                    .text()
                    .to_string();
                let value =
                    NixAddr::with(db, addr, attrpath_value.value().unwrap().syntax().clone());

                (path, value)
            }
        });

        NixVarContext::new(db, addr, own_vals.collect(), vec![*self])
    }

    fn resolve(&self, db: &'db dyn Db, name: &'_ str) -> NixExprId<'db> {
        let addr = *self.own_vals(db).get(name).unwrap_or_else(|| panic!());

        NixExprId::new(db, addr, *self)
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct NixExprId<'db> {
    addr: NixAddr<'db>,
    ctx: NixVarContext<'db>,
}

impl NixExprId<'_> {
    fn as_expr_node(&self, db: &dyn Db) -> Expr {
        Expr::cast(self.addr(db).to_syntax_node(db)).unwrap()
    }
}

#[salsa::tracked]
pub fn find_expr<'db>(
    db: &'db dyn Db,
    addr: NixAddr<'db>,
    ctx: NixVarContext<'db>,
) -> NixExprId<'db> {
    let expr = addr.to_syntax_node(db);
    if !Expr::can_cast(expr.kind()) {
        EvalError("Not an expression".to_string()).accumulate(db);
    }

    NixExprId::new(db, addr, ctx)
}

struct ExprEvaluation<'db> {
    expr: Expr,
    addr: NixAddr<'db>,
    ctx: NixVarContext<'db>,
}

impl<'db> ExprEvaluation<'db> {
    fn evaluate(&self, db: &'db dyn Db) -> NixEvalResult<'db> {
        let val = match &self.expr {
            Expr::Literal(lit) => match lit.kind() {
                rnix::ast::LiteralKind::Integer(int) => Value::Number(int.value().unwrap()),
                e => panic!("Not supported: {:?}", e),
            },
            Expr::BinOp(bin_op) => {
                let left = NixAddr::with(db, self.addr, bin_op.lhs().unwrap().syntax().clone());
                let right = NixAddr::with(db, self.addr, bin_op.rhs().unwrap().syntax().clone());

                let left = find_expr(db, left, self.ctx);
                let right = find_expr(db, right, self.ctx);

                let left = eval_expression(db, left);
                let right = eval_expression(db, right);

                let left = left
                    .value(db)
                    .value(db)
                    .as_number()
                    .copied()
                    .unwrap_or_default();
                let right = right
                    .value(db)
                    .value(db)
                    .as_number()
                    .copied()
                    .unwrap_or_default();

                match bin_op.operator().unwrap() {
                    rnix::ast::BinOpKind::Add => Value::Number(left + right),
                    rnix::ast::BinOpKind::Mul => Value::Number(left * right),
                    e => panic!("Not supported: {:?}", e),
                }
            }
            Expr::LetIn(let_in) => {
                let body = NixAddr::with(db, self.addr, let_in.body().unwrap().syntax().clone());

                let ctx = self.ctx.add(db, self.addr, let_in.entries());
                return eval_expression(db, NixExprId::new(db, body, ctx));
            }
            Expr::Ident(ident) => {
                let syntax_token = ident.ident_token().unwrap();
                let name = syntax_token.text();

                let expr_id: NixExprId<'db> = self.ctx.resolve(db, name);

                return eval_expression(db, expr_id);
            }
            Expr::Paren(paren) => {
                let addr = NixAddr::with(db, self.addr, paren.expr().unwrap().syntax().clone());
                let expr_id = NixExprId::new(db, addr, self.ctx);

                return eval_expression(db, expr_id);
            }
            Expr::Lambda(lambda) => {}
            Expr::Apply(apply) => {
                let lambda = NixAddr::with(db, self.addr, apply.lambda().unwrap().syntax().clone());
                let argument =
                    NixAddr::with(db, self.addr, apply.argument().unwrap().syntax().clone());

                let lambda = find_expr(db, lambda, self.ctx);
                let argument = find_expr(db, argument, self.ctx);

                let lambda = eval_expression(db, lambda);
                let argument = eval_expression(db, argument);

                let lambda = lambda.value(db).value(db);

                match lambda {
                    Value::Function(func) => {
                        return NixEvalResult::new(db, func.evaluate(db, argument.value(db)));
                    }
                    _ => panic!("Not a function..."),
                }
            }
            e => panic!("Not supported: {:?}", e),
        };

        NixEvalResult::new(db, NixValue::new(db, val, self.addr))
    }
}

#[salsa::tracked(cycle_fn=eval_expression_cycle, cycle_initial=eval_expression_initial)]
pub fn eval_expression<'db>(db: &'db dyn Db, expr_id: NixExprId<'db>) -> NixEvalResult<'db> {
    let expr = expr_id.as_expr_node(db);

    let evaluation = ExprEvaluation {
        expr,
        addr: expr_id.addr(db),
        ctx: expr_id.ctx(db),
    };
    evaluation.evaluate(db)
}

fn eval_expression_cycle<'db>(
    _db: &'db dyn Db,
    _value: &NixEvalResult<'db>,
    _count: u32,
    _expr_id: NixExprId<'db>,
) -> salsa::CycleRecoveryAction<NixEvalResult<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

fn eval_expression_initial<'db>(db: &'db dyn Db, expr_id: NixExprId<'db>) -> NixEvalResult<'db> {
    NixEvalResult::new(db, NixValue::new(db, Value::Null, expr_id.addr(db)))
}

#[salsa::tracked]
pub fn eval_file<'db>(db: &'db dyn Db, file: NixFile<'db>) -> NixEvalResult<'db> {
    let root_node = SyntaxNode::new_root(file.root_node(db));
    let root = rnix::Root::cast(root_node).unwrap();
    let root_addr = NixAddr::new(db, root.expr().unwrap().syntax().text_range(), file);

    let root_expr = find_expr(
        db,
        root_addr,
        NixVarContext::new(db, root_addr, BTreeMap::default(), vec![]),
    );
    eval_expression(db, root_expr)
}
