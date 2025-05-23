use camino::Utf8PathBuf;
use nir::{Db, EvalError, NixEvalDatabase, ParseErrors, eval_file, get_value, parse_file};

fn main() -> color_eyre::Result<()> {
    let db = NixEvalDatabase::new();

    let file = db.input(Utf8PathBuf::from("./test.nix"))?;

    let root = parse_file(&db, file);

    let errors = parse_file::accumulated::<ParseErrors>(&db, file);

    println!("Errors while parsing: {errors:?}");

    let evaluation = eval_file(&db, root);

    let errors = eval_file::accumulated::<EvalError>(&db, root);

    println!("Errors while evaluating: {errors:?}");

    println!("{:?}", get_value(&db, evaluation.value(&db)));

    Ok(())
}
