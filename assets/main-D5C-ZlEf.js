const e = `enum Token {
    LParen,
    RParen,
    Sym(string),
    Int(int32),
    Bool(bool),
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
    Nil,
}

enum SExpr {
    Int(int32),
    Bool(bool),
    Sym(string),
    List(Vec[SExpr]),
}

fn is_digit(ch: string) -> bool {
    match ch {
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
    }
}

fn digit_value(ch: string) -> int32 {
    match ch {
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
    }
}

fn is_int_text(text: string) -> bool {
    let len = string_len(text);
    match len == 0 {
        true => false,
        false => {
            let i = ref(0);
            let saw_digit = ref(false);
            let ok = ref(true);
            let started = ref(false);
            let _ = (while ref_get(ok) && ref_get(i) < len {
                let ch = string_get(text, ref_get(i));
                match (ref_get(started), ch) {
                    (false, "-") => {
                        let _ = ref_set(started, true);
                        ref_set(i, ref_get(i) + 1)
                    }
                    _ => match is_digit(ch) {
                        true => {
                            let _ = ref_set(started, true);
                            let _ = ref_set(saw_digit, true);
                            ref_set(i, ref_get(i) + 1)
                        }
                        false => ref_set(ok, false),
                    },
                }
            });
            ref_get(ok) && ref_get(saw_digit)
        }
    }
}

fn parse_int32(text: string) -> int32 {
    let len = string_len(text);
    let i = ref(0);
    let negative = ref(false);
    let started = ref(false);
    let acc = ref(0);
    let _ = (while ref_get(i) < len {
        let ch = string_get(text, ref_get(i));
        match (ref_get(started), ch) {
            (false, "-") => {
                let _ = ref_set(started, true);
                let _ = ref_set(negative, true);
                ref_set(i, ref_get(i) + 1)
            }
            _ => {
                let _ = ref_set(started, true);
                let d = digit_value(ch);
                let _ = ref_set(acc, ref_get(acc) * 10 + d);
                ref_set(i, ref_get(i) + 1)
            }
        }
    });
    match ref_get(negative) {
        true => 0 - ref_get(acc),
        false => ref_get(acc),
    }
}

fn is_delim(ch: string) -> bool {
    match ch {
        "(" => true,
        ")" => true,
        " " => true,
        _ => false,
    }
}

fn lex_atom(source: string, start: int32) -> (Token, int32) {
    let len = string_len(source);
    let text = ref("");
    let i = ref(start);
    let done = ref(false);
    let _ = (while !ref_get(done) && ref_get(i) < len {
        let ch = string_get(source, ref_get(i));
        match is_delim(ch) {
            true => ref_set(done, true),
            false => {
                let _ = ref_set(text, ref_get(text) + ch);
                ref_set(i, ref_get(i) + 1)
            }
        }
    });
    let atom = ref_get(text);
    let token = match atom {
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        _ => match is_int_text(atom) {
            true => Token::Int(parse_int32(atom)),
            false => Token::Sym(atom),
        },
    };
    (token, ref_get(i))
}

fn lex(source: string) -> Vec[Token] {
    let len = string_len(source);
    let toks0: Vec[Token] = vec_new();
    let toks = ref(toks0);
    let i = ref(0);
    let _ = (while ref_get(i) < len {
        let ch = string_get(source, ref_get(i));
        match ch {
            "(" => {
                let _ = ref_set(toks, vec_push(ref_get(toks), Token::LParen));
                ref_set(i, ref_get(i) + 1)
            }
            ")" => {
                let _ = ref_set(toks, vec_push(ref_get(toks), Token::RParen));
                ref_set(i, ref_get(i) + 1)
            }
            " " => ref_set(i, ref_get(i) + 1),
            _ => {
                let (tok, next) = lex_atom(source, ref_get(i));
                let _ = ref_set(toks, vec_push(ref_get(toks), tok));
                ref_set(i, next)
            }
        }
    });
    ref_get(toks)
}

fn env_lookup(env: Vec[Binding], name: string) -> Value {
    let i = ref(vec_len(env) - 1);
    let result = ref(Value::Nil);
    let done = ref(false);
    let _ = (while !ref_get(done) && ref_get(i) >= 0 {
        let binding = vec_get(env, ref_get(i));
        if binding.name == name {
            let _ = ref_set(result, binding.value);
            ref_set(done, true)
        } else {
            ref_set(i, ref_get(i) - 1)
        }
    });
    ref_get(result)
}

fn lookup(local: Vec[Binding], global: Vec[Binding], name: string) -> Value {
    match env_lookup(local, name) {
        Value::Nil => env_lookup(global, name),
        other => other,
    }
}

fn parse_list(tokens: Vec[Token], start: int32) -> (Vec[SExpr], int32) {
    let acc: Vec[SExpr] = vec_new();
    let exprs = ref(acc);
    let i = ref(start);
    let done = ref(false);
    let _ = (while !ref_get(done) && ref_get(i) < vec_len(tokens) {
        match vec_get(tokens, ref_get(i)) {
            Token::RParen => {
                let _ = ref_set(done, true);
                ref_set(i, ref_get(i) + 1)
            }
            _ => {
                let (expr, next) = parse_expr(tokens, ref_get(i));
                let _ = ref_set(exprs, vec_push(ref_get(exprs), expr));
                ref_set(i, next)
            }
        }
    });
    (ref_get(exprs), ref_get(i))
}

fn parse_expr(tokens: Vec[Token], start: int32) -> (SExpr, int32) {
    match vec_get(tokens, start) {
        Token::LParen => {
            let (items, next) = parse_list(tokens, start + 1);
            (SExpr::List(items), next)
        }
        Token::RParen => (SExpr::Sym(")"), start + 1),
        Token::Bool(b) => (SExpr::Bool(b), start + 1),
        Token::Int(n) => (SExpr::Int(n), start + 1),
        Token::Sym(name) => (SExpr::Sym(name), start + 1),
    }
}

fn parse_program(tokens: Vec[Token]) -> Vec[SExpr] {
    let i = ref(0);
    let acc: Vec[SExpr] = vec_new();
    let exprs = ref(acc);
    let _ = (while ref_get(i) < vec_len(tokens) {
        let (expr, next) = parse_expr(tokens, ref_get(i));
        let _ = ref_set(exprs, vec_push(ref_get(exprs), expr));
        ref_set(i, next)
    });
    ref_get(exprs)
}

fn value_to_string(value: Value) -> string {
    match value {
        Value::Int(n) => int32_to_string(n),
        Value::Bool(b) => bool_to_string(b),
        Value::Func(_) => "<lambda>",
        Value::Nil => "nil",
    }
}

fn truthy(value: Value) -> bool {
    match value {
        Value::Bool(b) => b,
        Value::Int(n) => n != 0,
        Value::Func(_) => true,
        Value::Nil => false,
    }
}

fn eval(expr: SExpr, local: Vec[Binding], global: Ref[Vec[Binding]]) -> Value {
    match expr {
        SExpr::Int(n) => Value::Int(n),
        SExpr::Bool(b) => Value::Bool(b),
        SExpr::Sym(name) => lookup(local, ref_get(global), name),
        SExpr::List(items) => eval_list(items, local, global),
    }
}

fn eval_list(items: Vec[SExpr], local: Vec[Binding], global: Ref[Vec[Binding]]) -> Value {
    if vec_len(items) == 0 {
        Value::Nil
    } else {
        let head = vec_get(items, 0);
        match head {
            SExpr::Sym(name) => eval_list_sym(name, items, local, global),
            _ => {
                let f = eval(head, local, global);
                let args = eval_args(items, 1, local, global);
                apply(f, args, global)
            }
        }
    }
}

fn eval_list_sym(name: string, items: Vec[SExpr], local: Vec[Binding], global: Ref[Vec[Binding]]) -> Value {
    match name {
        "begin" => eval_begin(items, 1, local, global),
        "define" => {
            match vec_len(items) == 3 {
                true => match vec_get(items, 1) {
                    SExpr::Sym(var) => {
                        let value = eval(vec_get(items, 2), local, global);
                        let env = ref_get(global);
                        let updated = vec_push(env, Binding { name: var, value: value });
                        let _ = ref_set(global, updated);
                        value
                    }
                    _ => Value::Nil,
                },
                false => Value::Nil,
            }
        }
        "if" => {
            match vec_len(items) == 4 {
                true => {
                    let cond = eval(vec_get(items, 1), local, global);
                    match truthy(cond) {
                        true => eval(vec_get(items, 2), local, global),
                        false => eval(vec_get(items, 3), local, global),
                    }
                }
                false => Value::Nil,
            }
        }
        "lambda" => {
            match vec_len(items) == 3 {
                true => match vec_get(items, 1) {
                    SExpr::List(params_exprs) => {
                        let params = params_from_sexprs(params_exprs);
                        let body = vec_get(items, 2);
                        Value::Func(Lambda { params: params, body: body, env: local, global: global })
                    }
                    _ => Value::Nil,
                },
                false => Value::Nil,
            }
        }
        "+" => apply_builtin("+", eval_args(items, 1, local, global)),
        "-" => apply_builtin("-", eval_args(items, 1, local, global)),
        "*" => apply_builtin("*", eval_args(items, 1, local, global)),
        "/" => apply_builtin("/", eval_args(items, 1, local, global)),
        "=" => apply_builtin("=", eval_args(items, 1, local, global)),
        _ => {
            let f = eval(SExpr::Sym(name), local, global);
            let args = eval_args(items, 1, local, global);
            apply(f, args, global)
        }
    }
}

fn eval_begin(items: Vec[SExpr], start: int32, local: Vec[Binding], global: Ref[Vec[Binding]]) -> Value {
    let i = ref(start);
    let last = ref(Value::Nil);
    let _ = (while ref_get(i) < vec_len(items) {
        let v = eval(vec_get(items, ref_get(i)), local, global);
        let _ = ref_set(last, v);
        ref_set(i, ref_get(i) + 1)
    });
    ref_get(last)
}

fn params_from_sexprs(items: Vec[SExpr]) -> Vec[string] {
    let i = ref(0);
    let acc: Vec[string] = vec_new();
    let params = ref(acc);
    let _ = (while ref_get(i) < vec_len(items) {
        match vec_get(items, ref_get(i)) {
            SExpr::Sym(name) => {
                let _ = ref_set(params, vec_push(ref_get(params), name));
                ref_set(i, ref_get(i) + 1)
            }
            _ => ref_set(i, ref_get(i) + 1),
        }
    });
    ref_get(params)
}

fn eval_args(items: Vec[SExpr], start: int32, local: Vec[Binding], global: Ref[Vec[Binding]]) -> Vec[Value] {
    let i = ref(start);
    let acc: Vec[Value] = vec_new();
    let args = ref(acc);
    let _ = (while ref_get(i) < vec_len(items) {
        let v = eval(vec_get(items, ref_get(i)), local, global);
        let _ = ref_set(args, vec_push(ref_get(args), v));
        ref_set(i, ref_get(i) + 1)
    });
    ref_get(args)
}

fn apply_builtin(name: string, args: Vec[Value]) -> Value {
    match name {
        "=" => match vec_len(args) == 2 {
            true => match (vec_get(args, 0), vec_get(args, 1)) {
                (Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
                (Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
                _ => Value::Bool(false),
            },
            false => Value::Bool(false),
        },
        "+" => {
            let i = ref(0);
            let acc = ref(0);
            let _ = (while ref_get(i) < vec_len(args) {
                match vec_get(args, ref_get(i)) {
                    Value::Int(n) => {
                        let _ = ref_set(acc, ref_get(acc) + n);
                        ref_set(i, ref_get(i) + 1)
                    }
                    _ => ref_set(i, ref_get(i) + 1),
                }
            });
            Value::Int(ref_get(acc))
        }
        "*" => {
            let i = ref(0);
            let acc = ref(1);
            let _ = (while ref_get(i) < vec_len(args) {
                match vec_get(args, ref_get(i)) {
                    Value::Int(n) => {
                        let _ = ref_set(acc, ref_get(acc) * n);
                        ref_set(i, ref_get(i) + 1)
                    }
                    _ => ref_set(i, ref_get(i) + 1),
                }
            });
            Value::Int(ref_get(acc))
        }
        "-" => match vec_len(args) {
            1 => match vec_get(args, 0) {
                Value::Int(n) => Value::Int(0 - n),
                _ => Value::Nil,
            },
            2 => match (vec_get(args, 0), vec_get(args, 1)) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                _ => Value::Nil,
            },
            _ => Value::Nil,
        },
        "/" => match vec_len(args) == 2 {
            true => match (vec_get(args, 0), vec_get(args, 1)) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
                _ => Value::Nil,
            },
            false => Value::Nil,
        },
        _ => Value::Nil,
    }
}

fn apply(func: Value, args: Vec[Value], global: Ref[Vec[Binding]]) -> Value {
    match func {
        Value::Func(fun) => apply_lambda(fun, args),
        _ => Value::Nil,
    }
}

fn apply_lambda(lambda: Lambda, args: Vec[Value]) -> Value {
    let env = ref(lambda.env);
    let i = ref(0);
    let _ = (while ref_get(i) < vec_len(lambda.params) && ref_get(i) < vec_len(args) {
        let name = vec_get(lambda.params, ref_get(i));
        let value = vec_get(args, ref_get(i));
        let updated = vec_push(ref_get(env), Binding { name: name, value: value });
        let _ = ref_set(env, updated);
        ref_set(i, ref_get(i) + 1)
    });
    eval(lambda.body, ref_get(env), lambda.global)
}

fn main() -> unit {
    let global: Ref[Vec[Binding]] = ref(vec_new());

    let program = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))";
    let exprs = parse_program(lex(program));
    let result = eval(vec_get(exprs, 0), vec_new(), global);
    let _ = string_println(value_to_string(result));

    let exprs2 = parse_program(lex("(add3 10 20 30)"));
    let result2 = eval(vec_get(exprs2, 0), vec_new(), global);
    let _ = string_println(value_to_string(result2));
    ()
}
`;
export {
  e as default
};
