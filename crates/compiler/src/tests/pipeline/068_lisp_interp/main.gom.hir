package Main
file main.gom
  enum Token {
        LParen,
        RParen,
        Sym(string),
        Int(int32),
        Bool(bool)
    }
    
    struct Binding {
        name: string,
        value: Value,
    }
    
    struct Lambda {
        params: Vec[string],
        body: SExpr,
        env: Vec[Binding],
        global: Ref[Vec[Binding]],
    }
    
    enum Value {
        Int(int32),
        Bool(bool),
        Func(Lambda),
        Nil
    }
    
    enum SExpr {
        Int(int32),
        Bool(bool),
        Sym(string),
        List(Vec[SExpr])
    }
    
    fn is_digit(local/1/0: string) -> bool {
      match ch/0 {
          "0" => true,
          "1" => true,
          "2" => true,
          "3" => true,
          "4" => true,
          "5" => true,
          "6" => true,
          "7" => true,
          "8" => true,
          "9" => true,
          _ => false,
      };
    }
    
    fn digit_value(local/1/1: string) -> int32 {
      match ch/1 {
          "0" => 0,
          "1" => 1,
          "2" => 2,
          "3" => 3,
          "4" => 4,
          "5" => 5,
          "6" => 6,
          "7" => 7,
          "8" => 8,
          "9" => 9,
          _ => 0,
      };
    }
    
    fn is_int_text(local/1/2: string) -> bool {
      let local/1/3 = string_len(text/2);
      match len/3 == 0 {
          true => false,
          false => {
                let local/1/4 = ref(0);
                let local/1/5 = ref(false);
                let local/1/6 = ref(true);
                let local/1/7 = ref(false);
                let _ = while ref_get(ok/6) && ref_get(i/4) < len/3 {
                    {
                        let local/1/8 = string_get(text/2, ref_get(i/4));
                        match (ref_get(started/7), ch/8) {
                            (false, "-") => {
                                  let _ = ref_set(started/7, true);
                                  ref_set(i/4, ref_get(i/4) + 1);
                              },
                            _ => match def/1/5(ch/8) {
                                  true => {
                                        let _ = ref_set(started/7, true);
                                        let _ = ref_set(saw_digit/5, true);
                                        ref_set(i/4, ref_get(i/4) + 1);
                                    },
                                  false => ref_set(ok/6, false),
                              },
                        };
                    }
                };
                ref_get(ok/6) && ref_get(saw_digit/5);
            },
      };
    }
    
    fn parse_int32(local/1/9: string) -> int32 {
      let local/1/10 = string_len(text/9);
      let local/1/11 = ref(0);
      let local/1/12 = ref(false);
      let local/1/13 = ref(false);
      let local/1/14 = ref(0);
      let _ = while ref_get(i/11) < len/10 {
          {
              let local/1/15 = string_get(text/9, ref_get(i/11));
              match (ref_get(started/13), ch/15) {
                  (false, "-") => {
                        let _ = ref_set(started/13, true);
                        let _ = ref_set(negative/12, true);
                        ref_set(i/11, ref_get(i/11) + 1);
                    },
                  _ => {
                        let _ = ref_set(started/13, true);
                        let local/1/16 = def/1/6(ch/15);
                        let _ = ref_set(acc/14, ref_get(acc/14) * 10 + d/16);
                        ref_set(i/11, ref_get(i/11) + 1);
                    },
              };
          }
      };
      match ref_get(negative/12) {
          true => 0 - ref_get(acc/14),
          false => ref_get(acc/14),
      };
    }
    
    fn is_delim(local/1/17: string) -> bool {
      match ch/17 {
          "(" => true,
          ")" => true,
          " " => true,
          _ => false,
      };
    }
    
    fn lex_atom(local/1/18: string, local/1/19: int32) -> (Token, int32) {
      let local/1/20 = string_len(source/18);
      let local/1/21 = ref("");
      let local/1/22 = ref(start/19);
      let local/1/23 = ref(false);
      let _ = while !ref_get(done/23) && ref_get(i/22) < len/20 {
          {
              let local/1/24 = string_get(source/18, ref_get(i/22));
              match def/1/9(ch/24) {
                  true => ref_set(done/23, true),
                  false => {
                        let _ = ref_set(text/21, ref_get(text/21) + ch/24);
                        ref_set(i/22, ref_get(i/22) + 1);
                    },
              };
          }
      };
      let local/1/25 = ref_get(text/21);
      let local/1/26 = match atom/25 {
          "true" => ctor(def/1/0::v4)(true),
          "false" => ctor(def/1/0::v4)(false),
          _ => match def/1/7(atom/25) {
                true => ctor(def/1/0::v3)(def/1/8(atom/25)),
                false => ctor(def/1/0::v2)(atom/25),
            },
      };
      (token/26, ref_get(i/22));
    }
    
    fn lex(local/1/27: string) -> Vec[Token] {
      let local/1/28 = string_len(source/27);
      let local/1/29: Vec[Token] = vec_new();
      let local/1/30 = ref(toks0/29);
      let local/1/31 = ref(0);
      let _ = while ref_get(i/31) < len/28 {
          {
              let local/1/32 = string_get(source/27, ref_get(i/31));
              match ch/32 {
                  "(" => {
                        let _ = ref_set(toks/30, vec_push(ref_get(toks/30), ctor(def/1/0::v0)));
                        ref_set(i/31, ref_get(i/31) + 1);
                    },
                  ")" => {
                        let _ = ref_set(toks/30, vec_push(ref_get(toks/30), ctor(def/1/0::v1)));
                        ref_set(i/31, ref_get(i/31) + 1);
                    },
                  " " => ref_set(i/31, ref_get(i/31) + 1),
                  _ => {
                        let (local/1/33, local/1/34) = def/1/10(source/27, ref_get(i/31));
                        let _ = ref_set(toks/30, vec_push(ref_get(toks/30), tok/33));
                        ref_set(i/31, next/34);
                    },
              };
          }
      };
      ref_get(toks/30);
    }
    
    fn env_lookup(local/1/35: Vec[Binding], local/1/36: string) -> Value {
      let local/1/37 = ref(vec_len(env/35) - 1);
      let local/1/38 = ref(ctor(def/1/3::v3));
      let local/1/39 = ref(false);
      let _ = while !ref_get(done/39) && ref_get(i/37) >= 0 {
          {
              let local/1/40 = vec_get(env/35, ref_get(i/37));
              if binding/40.name == name/36 {
                  let _ = ref_set(result/38, binding/40.value);
                  ref_set(done/39, true);
              } else {
                  ref_set(i/37, ref_get(i/37) - 1);
              };
          }
      };
      ref_get(result/38);
    }
    
    fn lookup(local/1/41: Vec[Binding], local/1/42: Vec[Binding], local/1/43: string) -> Value {
      match def/1/12(local/41, name/43) {
          ctor(def/1/3::v3) => def/1/12(global/42, name/43),
          local/1/44 => other/44,
      };
    }
    
    fn parse_list(local/1/45: Vec[Token], local/1/46: int32) -> (Vec[SExpr], int32) {
      let local/1/47: Vec[SExpr] = vec_new();
      let local/1/48 = ref(acc/47);
      let local/1/49 = ref(start/46);
      let local/1/50 = ref(false);
      let _ = while !ref_get(done/50) && ref_get(i/49) < vec_len(tokens/45) {
          {
              match vec_get(tokens/45, ref_get(i/49)) {
                  ctor(def/1/0::v1) => {
                        let _ = ref_set(done/50, true);
                        ref_set(i/49, ref_get(i/49) + 1);
                    },
                  _ => {
                        let (local/1/51, local/1/52) = def/1/15(tokens/45, ref_get(i/49));
                        let _ = ref_set(exprs/48, vec_push(ref_get(exprs/48), expr/51));
                        ref_set(i/49, next/52);
                    },
              };
          }
      };
      (ref_get(exprs/48), ref_get(i/49));
    }
    
    fn parse_expr(local/1/53: Vec[Token], local/1/54: int32) -> (SExpr, int32) {
      match vec_get(tokens/53, start/54) {
          ctor(def/1/0::v0) => {
                let (local/1/55, local/1/56) = def/1/14(tokens/53, start/54 + 1);
                (ctor(def/1/4::v3)(items/55), next/56);
            },
          ctor(def/1/0::v1) => (ctor(def/1/4::v2)(")"), start/54 + 1),
          ctor(def/1/0::v4)(local/1/57) => (ctor(def/1/4::v1)(b/57), start/54 + 1),
          ctor(def/1/0::v3)(local/1/58) => (ctor(def/1/4::v0)(n/58), start/54 + 1),
          ctor(def/1/0::v2)(local/1/59) => (ctor(def/1/4::v2)(name/59), start/54 + 1),
      };
    }
    
    fn parse_program(local/1/60: Vec[Token]) -> Vec[SExpr] {
      let local/1/61 = ref(0);
      let local/1/62: Vec[SExpr] = vec_new();
      let local/1/63 = ref(acc/62);
      let _ = while ref_get(i/61) < vec_len(tokens/60) {
          {
              let (local/1/64, local/1/65) = def/1/15(tokens/60, ref_get(i/61));
              let _ = ref_set(exprs/63, vec_push(ref_get(exprs/63), expr/64));
              ref_set(i/61, next/65);
          }
      };
      ref_get(exprs/63);
    }
    
    fn value_to_string(local/1/66: Value) -> string {
      match value/66 {
          ctor(def/1/3::v0)(local/1/67) => int32_to_string(n/67),
          ctor(def/1/3::v1)(local/1/68) => bool_to_string(b/68),
          ctor(def/1/3::v2)(_) => "<lambda>",
          ctor(def/1/3::v3) => "nil",
      };
    }
    
    fn truthy(local/1/69: Value) -> bool {
      match value/69 {
          ctor(def/1/3::v1)(local/1/70) => b/70,
          ctor(def/1/3::v0)(local/1/71) => n/71 != 0,
          ctor(def/1/3::v2)(_) => true,
          ctor(def/1/3::v3) => false,
      };
    }
    
    fn eval(local/1/72: SExpr, local/1/73: Vec[Binding], local/1/74: Ref[Vec[Binding]]) -> Value {
      match expr/72 {
          ctor(def/1/4::v0)(local/1/75) => ctor(def/1/3::v0)(n/75),
          ctor(def/1/4::v1)(local/1/76) => ctor(def/1/3::v1)(b/76),
          ctor(def/1/4::v2)(local/1/77) => def/1/13(local/73, ref_get(global/74), name/77),
          ctor(def/1/4::v3)(local/1/78) => def/1/20(items/78, local/73, global/74),
      };
    }
    
    fn eval_list(local/1/79: Vec[SExpr], local/1/80: Vec[Binding], local/1/81: Ref[Vec[Binding]]) -> Value {
      if vec_len(items/79) == 0 {
          ctor(def/1/3::v3);
      } else {
          let local/1/82 = vec_get(items/79, 0);
          match head/82 {
              ctor(def/1/4::v2)(local/1/83) => def/1/21(name/83, items/79, local/80, global/81),
              _ => {
                    let local/1/84 = def/1/19(head/82, local/80, global/81);
                    let local/1/85 = def/1/24(items/79, 1, local/80, global/81);
                    def/1/26(f/84, args/85, global/81);
                },
          };
      };
    }
    
    fn eval_list_sym(local/1/86: string, local/1/87: Vec[SExpr], local/1/88: Vec[Binding], local/1/89: Ref[Vec[Binding]]) -> Value {
      match name/86 {
          "begin" => def/1/22(items/87, 1, local/88, global/89),
          "define" => {
                match vec_len(items/87) == 3 {
                    true => match vec_get(items/87, 1) {
                          ctor(def/1/4::v2)(local/1/90) => {
                                let local/1/91 = def/1/19(vec_get(items/87, 2), local/88, global/89);
                                let local/1/92 = ref_get(global/89);
                                let local/1/93 = vec_push(env/92, Binding {
                                      name: var/90,
                                      value: value/91
                                  });
                                let _ = ref_set(global/89, updated/93);
                                value/91;
                            },
                          _ => ctor(def/1/3::v3),
                      },
                    false => ctor(def/1/3::v3),
                };
            },
          "if" => {
                match vec_len(items/87) == 4 {
                    true => {
                          let local/1/94 = def/1/19(vec_get(items/87, 1), local/88, global/89);
                          match def/1/18(cond/94) {
                              true => def/1/19(vec_get(items/87, 2), local/88, global/89),
                              false => def/1/19(vec_get(items/87, 3), local/88, global/89),
                          };
                      },
                    false => ctor(def/1/3::v3),
                };
            },
          "lambda" => {
                match vec_len(items/87) == 3 {
                    true => match vec_get(items/87, 1) {
                          ctor(def/1/4::v3)(local/1/95) => {
                                let local/1/96 = def/1/23(params_exprs/95);
                                let local/1/97 = vec_get(items/87, 2);
                                ctor(def/1/3::v2)(Lambda {
                                    params: params/96,
                                    body: body/97,
                                    env: local/88,
                                    global: global/89
                                });
                            },
                          _ => ctor(def/1/3::v3),
                      },
                    false => ctor(def/1/3::v3),
                };
            },
          "+" => def/1/25("+", def/1/24(items/87, 1, local/88, global/89)),
          "-" => def/1/25("-", def/1/24(items/87, 1, local/88, global/89)),
          "*" => def/1/25("*", def/1/24(items/87, 1, local/88, global/89)),
          "/" => def/1/25("/", def/1/24(items/87, 1, local/88, global/89)),
          "=" => def/1/25("=", def/1/24(items/87, 1, local/88, global/89)),
          _ => {
                let local/1/98 = def/1/19(ctor(def/1/4::v2)(name/86), local/88, global/89);
                let local/1/99 = def/1/24(items/87, 1, local/88, global/89);
                def/1/26(f/98, args/99, global/89);
            },
      };
    }
    
    fn eval_begin(local/1/100: Vec[SExpr], local/1/101: int32, local/1/102: Vec[Binding], local/1/103: Ref[Vec[Binding]]) -> Value {
      let local/1/104 = ref(start/101);
      let local/1/105 = ref(ctor(def/1/3::v3));
      let _ = while ref_get(i/104) < vec_len(items/100) {
          {
              let local/1/106 = def/1/19(vec_get(items/100, ref_get(i/104)), local/102, global/103);
              let _ = ref_set(last/105, v/106);
              ref_set(i/104, ref_get(i/104) + 1);
          }
      };
      ref_get(last/105);
    }
    
    fn params_from_sexprs(local/1/107: Vec[SExpr]) -> Vec[string] {
      let local/1/108 = ref(0);
      let local/1/109: Vec[string] = vec_new();
      let local/1/110 = ref(acc/109);
      let _ = while ref_get(i/108) < vec_len(items/107) {
          {
              match vec_get(items/107, ref_get(i/108)) {
                  ctor(def/1/4::v2)(local/1/111) => {
                        let _ = ref_set(params/110, vec_push(ref_get(params/110), name/111));
                        ref_set(i/108, ref_get(i/108) + 1);
                    },
                  _ => ref_set(i/108, ref_get(i/108) + 1),
              };
          }
      };
      ref_get(params/110);
    }
    
    fn eval_args(local/1/112: Vec[SExpr], local/1/113: int32, local/1/114: Vec[Binding], local/1/115: Ref[Vec[Binding]]) -> Vec[Value] {
      let local/1/116 = ref(start/113);
      let local/1/117: Vec[Value] = vec_new();
      let local/1/118 = ref(acc/117);
      let _ = while ref_get(i/116) < vec_len(items/112) {
          {
              let local/1/119 = def/1/19(vec_get(items/112, ref_get(i/116)), local/114, global/115);
              let _ = ref_set(args/118, vec_push(ref_get(args/118), v/119));
              ref_set(i/116, ref_get(i/116) + 1);
          }
      };
      ref_get(args/118);
    }
    
    fn apply_builtin(local/1/120: string, local/1/121: Vec[Value]) -> Value {
      match name/120 {
          "=" => match vec_len(args/121) == 2 {
                true => match (vec_get(args/121, 0), vec_get(args/121, 1)) {
                      (ctor(def/1/3::v0)(local/1/122), ctor(def/1/3::v0)(local/1/123)) => ctor(def/1/3::v1)(a/122 == b/123),
                      (ctor(def/1/3::v1)(local/1/124), ctor(def/1/3::v1)(local/1/125)) => ctor(def/1/3::v1)(a/124 == b/125),
                      _ => ctor(def/1/3::v1)(false),
                  },
                false => ctor(def/1/3::v1)(false),
            },
          "+" => {
                let local/1/126 = ref(0);
                let local/1/127 = ref(0);
                let _ = while ref_get(i/126) < vec_len(args/121) {
                    {
                        match vec_get(args/121, ref_get(i/126)) {
                            ctor(def/1/3::v0)(local/1/128) => {
                                  let _ = ref_set(acc/127, ref_get(acc/127) + n/128);
                                  ref_set(i/126, ref_get(i/126) + 1);
                              },
                            _ => ref_set(i/126, ref_get(i/126) + 1),
                        };
                    }
                };
                ctor(def/1/3::v0)(ref_get(acc/127));
            },
          "*" => {
                let local/1/129 = ref(0);
                let local/1/130 = ref(1);
                let _ = while ref_get(i/129) < vec_len(args/121) {
                    {
                        match vec_get(args/121, ref_get(i/129)) {
                            ctor(def/1/3::v0)(local/1/131) => {
                                  let _ = ref_set(acc/130, ref_get(acc/130) * n/131);
                                  ref_set(i/129, ref_get(i/129) + 1);
                              },
                            _ => ref_set(i/129, ref_get(i/129) + 1),
                        };
                    }
                };
                ctor(def/1/3::v0)(ref_get(acc/130));
            },
          "-" => match vec_len(args/121) {
                1 => match vec_get(args/121, 0) {
                      ctor(def/1/3::v0)(local/1/132) => ctor(def/1/3::v0)(0 - n/132),
                      _ => ctor(def/1/3::v3),
                  },
                2 => match (vec_get(args/121, 0), vec_get(args/121, 1)) {
                      (ctor(def/1/3::v0)(local/1/133), ctor(def/1/3::v0)(local/1/134)) => ctor(def/1/3::v0)(a/133 - b/134),
                      _ => ctor(def/1/3::v3),
                  },
                _ => ctor(def/1/3::v3),
            },
          "/" => match vec_len(args/121) == 2 {
                true => match (vec_get(args/121, 0), vec_get(args/121, 1)) {
                      (ctor(def/1/3::v0)(local/1/135), ctor(def/1/3::v0)(local/1/136)) => ctor(def/1/3::v0)(a/135 / b/136),
                      _ => ctor(def/1/3::v3),
                  },
                false => ctor(def/1/3::v3),
            },
          _ => ctor(def/1/3::v3),
      };
    }
    
    fn apply(local/1/137: Value, local/1/138: Vec[Value], local/1/139: Ref[Vec[Binding]]) -> Value {
      match func/137 {
          ctor(def/1/3::v2)(local/1/140) => def/1/27(fun/140, args/138),
          _ => ctor(def/1/3::v3),
      };
    }
    
    fn apply_lambda(local/1/141: Lambda, local/1/142: Vec[Value]) -> Value {
      let local/1/143 = ref(lambda/141.env);
      let local/1/144 = ref(0);
      let _ = while ref_get(i/144) < vec_len(lambda/141.params) && ref_get(i/144) < vec_len(args/142) {
          {
              let local/1/145 = vec_get(lambda/141.params, ref_get(i/144));
              let local/1/146 = vec_get(args/142, ref_get(i/144));
              let local/1/147 = vec_push(ref_get(env/143), Binding {
                    name: name/145,
                    value: value/146
                });
              let _ = ref_set(env/143, updated/147);
              ref_set(i/144, ref_get(i/144) + 1);
          }
      };
      def/1/19(lambda/141.body, ref_get(env/143), lambda/141.global);
    }
    
    fn main() -> unit {
      let local/1/148: Ref[Vec[Binding]] = ref(vec_new());
      let local/1/149 = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))";
      let local/1/150 = def/1/16(def/1/11(program/149));
      let local/1/151 = def/1/19(vec_get(exprs/150, 0), vec_new(), global/148);
      let _ = string_println(def/1/17(result/151));
      let local/1/152 = def/1/16(def/1/11("(add3 10 20 30)"));
      let local/1/153 = def/1/19(vec_get(exprs2/152, 0), vec_new(), global/148);
      let _ = string_println(def/1/17(result2/153));
      ();
    }