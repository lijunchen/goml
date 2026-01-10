enum Expr {
    Zero,
    Succ(Expr),
    Add(Expr, Expr),
    Mul(Expr, Expr)
}

fn main() {
  let local/0 = ctor(def/0::v3)(ctor(def/0::v2)(ctor(def/0::v0), ctor(def/0::v0)), ctor(def/0::v0));
  match a/0 {
      ctor(def/0::v2)(ctor(def/0::v0), ctor(def/0::v0)) => string_print(int32_to_string(0)),
      ctor(def/0::v3)(ctor(def/0::v0), local/1) => string_print(int32_to_string(1)),
      ctor(def/0::v2)(ctor(def/0::v1)(local/2), local/3) => string_print(int32_to_string(2)),
      ctor(def/0::v3)(local/4, ctor(def/0::v0)) => string_print(int32_to_string(3)),
      ctor(def/0::v3)(ctor(def/0::v2)(local/5, local/6), local/7) => string_print(int32_to_string(4)),
      ctor(def/0::v2)(local/8, ctor(def/0::v0)) => string_print(int32_to_string(5)),
      local/9 => string_print(int32_to_string(6)),
  };
}

