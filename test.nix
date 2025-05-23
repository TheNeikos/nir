let
  bar = 3;
  mul = x: x * 3;
  square = x: x * x;
in
mul (square bar) 2
