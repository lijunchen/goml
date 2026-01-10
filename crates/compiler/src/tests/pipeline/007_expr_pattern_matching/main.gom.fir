enum Expr {
    Zero,
    Succ(Expr),
    Add(Expr, Expr),
    Mul(Expr, Expr)
}

fn main() {
  let local/0 = ctor/3(ctor/2(ctor/0, ctor/0), ctor/0);
  match a/0 {
      ctor/2(ctor/0, ctor/0) => string_print(int32_to_string(0)),
      ctor/3(ctor/0, local/1) => string_print(int32_to_string(1)),
      ctor/2(ctor/1(local/2), local/3) => string_print(int32_to_string(2)),
      ctor/3(local/4, ctor/0) => string_print(int32_to_string(3)),
      ctor/3(ctor/2(local/5, local/6), local/7) => string_print(int32_to_string(4)),
      ctor/2(local/8, ctor/0) => string_print(int32_to_string(5)),
      local/9 => string_print(int32_to_string(6)),
  };
}

