package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func string_len(s string) int32 {
    return int32(len(s))
}

func string_get(s string, i int32) string {
    return string(s[i])
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_string_x struct {
    value string
}

func ref__Ref_string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_vec_token_x struct {
    value []Token
}

func ref__Ref_Vec_Token(value []Token) *ref_vec_token_x {
    return &ref_vec_token_x{
        value: value,
    }
}

func ref_get__Ref_Vec_Token(reference *ref_vec_token_x) []Token {
    return reference.value
}

func ref_set__Ref_Vec_Token(reference *ref_vec_token_x, value []Token) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_value_x struct {
    value Value
}

func ref__Ref_Value(value Value) *ref_value_x {
    return &ref_value_x{
        value: value,
    }
}

func ref_get__Ref_Value(reference *ref_value_x) Value {
    return reference.value
}

func ref_set__Ref_Value(reference *ref_value_x, value Value) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_vec_sexpr_x struct {
    value []SExpr
}

func ref__Ref_Vec_SExpr(value []SExpr) *ref_vec_sexpr_x {
    return &ref_vec_sexpr_x{
        value: value,
    }
}

func ref_get__Ref_Vec_SExpr(reference *ref_vec_sexpr_x) []SExpr {
    return reference.value
}

func ref_set__Ref_Vec_SExpr(reference *ref_vec_sexpr_x, value []SExpr) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_vec_binding_x struct {
    value []Binding
}

func ref__Ref_Vec_Binding(value []Binding) *ref_vec_binding_x {
    return &ref_vec_binding_x{
        value: value,
    }
}

func ref_get__Ref_Vec_Binding(reference *ref_vec_binding_x) []Binding {
    return reference.value
}

func ref_set__Ref_Vec_Binding(reference *ref_vec_binding_x, value []Binding) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_vec_string_x struct {
    value []string
}

func ref__Ref_Vec_string(value []string) *ref_vec_string_x {
    return &ref_vec_string_x{
        value: value,
    }
}

func ref_get__Ref_Vec_string(reference *ref_vec_string_x) []string {
    return reference.value
}

func ref_set__Ref_Vec_string(reference *ref_vec_string_x, value []string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_vec_value_x struct {
    value []Value
}

func ref__Ref_Vec_Value(value []Value) *ref_vec_value_x {
    return &ref_vec_value_x{
        value: value,
    }
}

func ref_get__Ref_Vec_Value(reference *ref_vec_value_x) []Value {
    return reference.value
}

func ref_set__Ref_Vec_Value(reference *ref_vec_value_x, value []Value) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_bool_string struct {
    _0 bool
    _1 string
}

type Tuple2_Token_int32 struct {
    _0 Token
    _1 int32
}

type Tuple2_Vec_SExpr_int32 struct {
    _0 []SExpr
    _1 int32
}

type Tuple2_SExpr_int32 struct {
    _0 SExpr
    _1 int32
}

type Tuple2_Value_Value struct {
    _0 Value
    _1 Value
}

type Binding struct {
    name string
    value Value
}

type Lambda struct {
    params []string
    body SExpr
    env []Binding
    global *ref_vec_binding_x
}

type Token interface {
    isToken()
}

type LParen struct {}

func (_ LParen) isToken() {}

type RParen struct {}

func (_ RParen) isToken() {}

type Token_Sym struct {
    _0 string
}

func (_ Token_Sym) isToken() {}

type Token_Int struct {
    _0 int32
}

func (_ Token_Int) isToken() {}

type Token_Bool struct {
    _0 bool
}

func (_ Token_Bool) isToken() {}

type Value interface {
    isValue()
}

type Value_Int struct {
    _0 int32
}

func (_ Value_Int) isValue() {}

type Value_Bool struct {
    _0 bool
}

func (_ Value_Bool) isValue() {}

type Func struct {
    _0 Lambda
}

func (_ Func) isValue() {}

type Nil struct {}

func (_ Nil) isValue() {}

type SExpr interface {
    isSExpr()
}

type SExpr_Int struct {
    _0 int32
}

func (_ SExpr_Int) isSExpr() {}

type SExpr_Bool struct {
    _0 bool
}

func (_ SExpr_Bool) isSExpr() {}

type SExpr_Sym struct {
    _0 string
}

func (_ SExpr_Sym) isSExpr() {}

type List struct {
    _0 []SExpr
}

func (_ List) isSExpr() {}

func is_digit(ch__0 string) bool {
    var ret391 bool
    switch ch__0 {
    case "0":
        ret391 = true
    case "1":
        ret391 = true
    case "2":
        ret391 = true
    case "3":
        ret391 = true
    case "4":
        ret391 = true
    case "5":
        ret391 = true
    case "6":
        ret391 = true
    case "7":
        ret391 = true
    case "8":
        ret391 = true
    case "9":
        ret391 = true
    default:
        ret391 = false
    }
    return ret391
}

func digit_value(ch__1 string) int32 {
    var ret392 int32
    switch ch__1 {
    case "0":
        ret392 = 0
    case "1":
        ret392 = 1
    case "2":
        ret392 = 2
    case "3":
        ret392 = 3
    case "4":
        ret392 = 4
    case "5":
        ret392 = 5
    case "6":
        ret392 = 6
    case "7":
        ret392 = 7
    case "8":
        ret392 = 8
    case "9":
        ret392 = 9
    default:
        ret392 = 0
    }
    return ret392
}

func is_int_text(text__2 string) bool {
    var ret393 bool
    var len__3 int32 = string_len(text__2)
    var mtmp0 bool = len__3 == 0
    switch mtmp0 {
    case true:
        ret393 = false
    case false:
        var i__4 *ref_int32_x = ref__Ref_int32(0)
        var saw_digit__5 *ref_bool_x = ref__Ref_bool(false)
        var ok__6 *ref_bool_x = ref__Ref_bool(true)
        var started__7 *ref_bool_x = ref__Ref_bool(false)
        var cond394 bool
        for {
            var t170 bool = ref_get__Ref_bool(ok__6)
            var t172 int32 = ref_get__Ref_int32(i__4)
            var t171 bool = t172 < len__3
            cond394 = t170 && t171
            if !cond394 {
                break
            }
            var t173 int32 = ref_get__Ref_int32(i__4)
            var ch__8 string = string_get(text__2, t173)
            var t174 bool = ref_get__Ref_bool(started__7)
            var mtmp1 Tuple2_bool_string = Tuple2_bool_string{
                _0: t174,
                _1: ch__8,
            }
            var x2 bool = mtmp1._0
            var x3 string = mtmp1._1
            switch x3 {
            case "-":
                switch x2 {
                case true:
                    var mtmp4 bool = is_digit(ch__8)
                    switch mtmp4 {
                    case true:
                        ref_set__Ref_bool(started__7, true)
                        ref_set__Ref_bool(saw_digit__5, true)
                        var t176 int32 = ref_get__Ref_int32(i__4)
                        var t175 int32 = t176 + 1
                        ref_set__Ref_int32(i__4, t175)
                    case false:
                        ref_set__Ref_bool(ok__6, false)
                    }
                case false:
                    ref_set__Ref_bool(started__7, true)
                    var t178 int32 = ref_get__Ref_int32(i__4)
                    var t177 int32 = t178 + 1
                    ref_set__Ref_int32(i__4, t177)
                }
            default:
                var mtmp8 bool = is_digit(ch__8)
                switch mtmp8 {
                case true:
                    ref_set__Ref_bool(started__7, true)
                    ref_set__Ref_bool(saw_digit__5, true)
                    var t180 int32 = ref_get__Ref_int32(i__4)
                    var t179 int32 = t180 + 1
                    ref_set__Ref_int32(i__4, t179)
                case false:
                    ref_set__Ref_bool(ok__6, false)
                }
            }
        }
        var t181 bool = ref_get__Ref_bool(ok__6)
        var t182 bool = ref_get__Ref_bool(saw_digit__5)
        ret393 = t181 && t182
    }
    return ret393
}

func parse_int32(text__9 string) int32 {
    var ret395 int32
    var len__10 int32 = string_len(text__9)
    var i__11 *ref_int32_x = ref__Ref_int32(0)
    var negative__12 *ref_bool_x = ref__Ref_bool(false)
    var started__13 *ref_bool_x = ref__Ref_bool(false)
    var acc__14 *ref_int32_x = ref__Ref_int32(0)
    var cond396 bool
    for {
        var t183 int32 = ref_get__Ref_int32(i__11)
        cond396 = t183 < len__10
        if !cond396 {
            break
        }
        var t184 int32 = ref_get__Ref_int32(i__11)
        var ch__15 string = string_get(text__9, t184)
        var t185 bool = ref_get__Ref_bool(started__13)
        var mtmp12 Tuple2_bool_string = Tuple2_bool_string{
            _0: t185,
            _1: ch__15,
        }
        var x13 bool = mtmp12._0
        var x14 string = mtmp12._1
        switch x14 {
        case "-":
            switch x13 {
            case true:
                ref_set__Ref_bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t188 int32 = ref_get__Ref_int32(acc__14)
                var t187 int32 = t188 * 10
                var t186 int32 = t187 + d__16
                ref_set__Ref_int32(acc__14, t186)
                var t190 int32 = ref_get__Ref_int32(i__11)
                var t189 int32 = t190 + 1
                ref_set__Ref_int32(i__11, t189)
            case false:
                ref_set__Ref_bool(started__13, true)
                ref_set__Ref_bool(negative__12, true)
                var t192 int32 = ref_get__Ref_int32(i__11)
                var t191 int32 = t192 + 1
                ref_set__Ref_int32(i__11, t191)
            }
        default:
            ref_set__Ref_bool(started__13, true)
            var d__16 int32 = digit_value(ch__15)
            var t195 int32 = ref_get__Ref_int32(acc__14)
            var t194 int32 = t195 * 10
            var t193 int32 = t194 + d__16
            ref_set__Ref_int32(acc__14, t193)
            var t197 int32 = ref_get__Ref_int32(i__11)
            var t196 int32 = t197 + 1
            ref_set__Ref_int32(i__11, t196)
        }
    }
    var mtmp22 bool = ref_get__Ref_bool(negative__12)
    switch mtmp22 {
    case true:
        var t198 int32 = ref_get__Ref_int32(acc__14)
        ret395 = 0 - t198
    case false:
        ret395 = ref_get__Ref_int32(acc__14)
    }
    return ret395
}

func is_delim(ch__17 string) bool {
    var ret397 bool
    switch ch__17 {
    case "(":
        ret397 = true
    case ")":
        ret397 = true
    case " ":
        ret397 = true
    default:
        ret397 = false
    }
    return ret397
}

func lex_atom(source__18 string, start__19 int32) Tuple2_Token_int32 {
    var ret398 Tuple2_Token_int32
    var len__20 int32 = string_len(source__18)
    var text__21 *ref_string_x = ref__Ref_string("")
    var i__22 *ref_int32_x = ref__Ref_int32(start__19)
    var done__23 *ref_bool_x = ref__Ref_bool(false)
    var cond399 bool
    for {
        var t200 bool = ref_get__Ref_bool(done__23)
        var t199 bool = !t200
        var t202 int32 = ref_get__Ref_int32(i__22)
        var t201 bool = t202 < len__20
        cond399 = t199 && t201
        if !cond399 {
            break
        }
        var t203 int32 = ref_get__Ref_int32(i__22)
        var ch__24 string = string_get(source__18, t203)
        var mtmp23 bool = is_delim(ch__24)
        switch mtmp23 {
        case true:
            ref_set__Ref_bool(done__23, true)
        case false:
            var t205 string = ref_get__Ref_string(text__21)
            var t204 string = t205 + ch__24
            ref_set__Ref_string(text__21, t204)
            var t207 int32 = ref_get__Ref_int32(i__22)
            var t206 int32 = t207 + 1
            ref_set__Ref_int32(i__22, t206)
        }
    }
    var atom__25 string = ref_get__Ref_string(text__21)
    var token__26 Token
    switch atom__25 {
    case "true":
        token__26 = Token_Bool{
            _0: true,
        }
    case "false":
        token__26 = Token_Bool{
            _0: false,
        }
    default:
        var mtmp26 bool = is_int_text(atom__25)
        switch mtmp26 {
        case true:
            var t208 int32 = parse_int32(atom__25)
            token__26 = Token_Int{
                _0: t208,
            }
        case false:
            token__26 = Token_Sym{
                _0: atom__25,
            }
        }
    }
    var t209 int32 = ref_get__Ref_int32(i__22)
    ret398 = Tuple2_Token_int32{
        _0: token__26,
        _1: t209,
    }
    return ret398
}

func lex(source__27 string) []Token {
    var ret400 []Token
    var len__28 int32 = string_len(source__27)
    var toks0__29 []Token = nil
    var toks__30 *ref_vec_token_x = ref__Ref_Vec_Token(toks0__29)
    var i__31 *ref_int32_x = ref__Ref_int32(0)
    var cond401 bool
    for {
        var t210 int32 = ref_get__Ref_int32(i__31)
        cond401 = t210 < len__28
        if !cond401 {
            break
        }
        var t211 int32 = ref_get__Ref_int32(i__31)
        var ch__32 string = string_get(source__27, t211)
        switch ch__32 {
        case "(":
            var t213 []Token = ref_get__Ref_Vec_Token(toks__30)
            var t214 Token = LParen{}
            var t212 []Token = append(t213, t214)
            ref_set__Ref_Vec_Token(toks__30, t212)
            var t216 int32 = ref_get__Ref_int32(i__31)
            var t215 int32 = t216 + 1
            ref_set__Ref_int32(i__31, t215)
        case ")":
            var t218 []Token = ref_get__Ref_Vec_Token(toks__30)
            var t219 Token = RParen{}
            var t217 []Token = append(t218, t219)
            ref_set__Ref_Vec_Token(toks__30, t217)
            var t221 int32 = ref_get__Ref_int32(i__31)
            var t220 int32 = t221 + 1
            ref_set__Ref_int32(i__31, t220)
        case " ":
            var t223 int32 = ref_get__Ref_int32(i__31)
            var t222 int32 = t223 + 1
            ref_set__Ref_int32(i__31, t222)
        default:
            var t224 int32 = ref_get__Ref_int32(i__31)
            var mtmp29 Tuple2_Token_int32 = lex_atom(source__27, t224)
            var x30 Token = mtmp29._0
            var x31 int32 = mtmp29._1
            var next__34 int32 = x31
            var tok__33 Token = x30
            var t226 []Token = ref_get__Ref_Vec_Token(toks__30)
            var t225 []Token = append(t226, tok__33)
            ref_set__Ref_Vec_Token(toks__30, t225)
            ref_set__Ref_int32(i__31, next__34)
        }
    }
    ret400 = ref_get__Ref_Vec_Token(toks__30)
    return ret400
}

func env_lookup(env__35 []Binding, name__36 string) Value {
    var ret402 Value
    var t228 int32 = int32(len(env__35))
    var t227 int32 = t228 - 1
    var i__37 *ref_int32_x = ref__Ref_int32(t227)
    var t229 Value = Nil{}
    var result__38 *ref_value_x = ref__Ref_Value(t229)
    var done__39 *ref_bool_x = ref__Ref_bool(false)
    var cond403 bool
    for {
        var t231 bool = ref_get__Ref_bool(done__39)
        var t230 bool = !t231
        var t233 int32 = ref_get__Ref_int32(i__37)
        var t232 bool = t233 >= 0
        cond403 = t230 && t232
        if !cond403 {
            break
        }
        var t234 int32 = ref_get__Ref_int32(i__37)
        var binding__40 Binding = env__35[t234]
        var t236 string = binding__40.name
        var t235 bool = t236 == name__36
        if t235 {
            var t237 Value = binding__40.value
            ref_set__Ref_Value(result__38, t237)
            ref_set__Ref_bool(done__39, true)
        } else {
            var t239 int32 = ref_get__Ref_int32(i__37)
            var t238 int32 = t239 - 1
            ref_set__Ref_int32(i__37, t238)
        }
    }
    ret402 = ref_get__Ref_Value(result__38)
    return ret402
}

func lookup(local__41 []Binding, global__42 []Binding, name__43 string) Value {
    var ret404 Value
    var mtmp36 Value = env_lookup(local__41, name__43)
    switch mtmp36 := mtmp36.(type) {
    case Value_Int:
        var other__44 Value = mtmp36
        ret404 = other__44
    case Value_Bool:
        var other__44 Value = mtmp36
        ret404 = other__44
    case Func:
        var other__44 Value = mtmp36
        ret404 = other__44
    case Nil:
        ret404 = env_lookup(global__42, name__43)
    }
    return ret404
}

func parse_list(tokens__45 []Token, start__46 int32) Tuple2_Vec_SExpr_int32 {
    var ret405 Tuple2_Vec_SExpr_int32
    var acc__47 []SExpr = nil
    var exprs__48 *ref_vec_sexpr_x = ref__Ref_Vec_SExpr(acc__47)
    var i__49 *ref_int32_x = ref__Ref_int32(start__46)
    var done__50 *ref_bool_x = ref__Ref_bool(false)
    var cond406 bool
    for {
        var t241 bool = ref_get__Ref_bool(done__50)
        var t240 bool = !t241
        var t243 int32 = ref_get__Ref_int32(i__49)
        var t244 int32 = int32(len(tokens__45))
        var t242 bool = t243 < t244
        cond406 = t240 && t242
        if !cond406 {
            break
        }
        var t245 int32 = ref_get__Ref_int32(i__49)
        var mtmp40 Token = tokens__45[t245]
        switch mtmp40.(type) {
        case LParen:
            var t246 int32 = ref_get__Ref_int32(i__49)
            var mtmp44 Tuple2_SExpr_int32 = parse_expr(tokens__45, t246)
            var x45 SExpr = mtmp44._0
            var x46 int32 = mtmp44._1
            var next__52 int32 = x46
            var expr__51 SExpr = x45
            var t248 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t247 []SExpr = append(t248, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t247)
            ref_set__Ref_int32(i__49, next__52)
        case RParen:
            ref_set__Ref_bool(done__50, true)
            var t250 int32 = ref_get__Ref_int32(i__49)
            var t249 int32 = t250 + 1
            ref_set__Ref_int32(i__49, t249)
        case Token_Sym:
            var t251 int32 = ref_get__Ref_int32(i__49)
            var mtmp49 Tuple2_SExpr_int32 = parse_expr(tokens__45, t251)
            var x50 SExpr = mtmp49._0
            var x51 int32 = mtmp49._1
            var next__52 int32 = x51
            var expr__51 SExpr = x50
            var t253 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t252 []SExpr = append(t253, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t252)
            ref_set__Ref_int32(i__49, next__52)
        case Token_Int:
            var t254 int32 = ref_get__Ref_int32(i__49)
            var mtmp53 Tuple2_SExpr_int32 = parse_expr(tokens__45, t254)
            var x54 SExpr = mtmp53._0
            var x55 int32 = mtmp53._1
            var next__52 int32 = x55
            var expr__51 SExpr = x54
            var t256 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t255 []SExpr = append(t256, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t255)
            ref_set__Ref_int32(i__49, next__52)
        case Token_Bool:
            var t257 int32 = ref_get__Ref_int32(i__49)
            var mtmp57 Tuple2_SExpr_int32 = parse_expr(tokens__45, t257)
            var x58 SExpr = mtmp57._0
            var x59 int32 = mtmp57._1
            var next__52 int32 = x59
            var expr__51 SExpr = x58
            var t259 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t258 []SExpr = append(t259, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t258)
            ref_set__Ref_int32(i__49, next__52)
        }
    }
    var t260 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
    var t261 int32 = ref_get__Ref_int32(i__49)
    ret405 = Tuple2_Vec_SExpr_int32{
        _0: t260,
        _1: t261,
    }
    return ret405
}

func parse_expr(tokens__53 []Token, start__54 int32) Tuple2_SExpr_int32 {
    var ret407 Tuple2_SExpr_int32
    var mtmp62 Token = tokens__53[start__54]
    switch mtmp62 := mtmp62.(type) {
    case LParen:
        var t262 int32 = start__54 + 1
        var mtmp66 Tuple2_Vec_SExpr_int32 = parse_list(tokens__53, t262)
        var x67 []SExpr = mtmp66._0
        var x68 int32 = mtmp66._1
        var next__56 int32 = x68
        var items__55 []SExpr = x67
        var t263 SExpr = List{
            _0: items__55,
        }
        ret407 = Tuple2_SExpr_int32{
            _0: t263,
            _1: next__56,
        }
    case RParen:
        var t264 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t265 int32 = start__54 + 1
        ret407 = Tuple2_SExpr_int32{
            _0: t264,
            _1: t265,
        }
    case Token_Sym:
        var x63 string = mtmp62._0
        var name__59 string = x63
        var t266 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t267 int32 = start__54 + 1
        ret407 = Tuple2_SExpr_int32{
            _0: t266,
            _1: t267,
        }
    case Token_Int:
        var x64 int32 = mtmp62._0
        var n__58 int32 = x64
        var t268 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t269 int32 = start__54 + 1
        ret407 = Tuple2_SExpr_int32{
            _0: t268,
            _1: t269,
        }
    case Token_Bool:
        var x65 bool = mtmp62._0
        var b__57 bool = x65
        var t270 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t271 int32 = start__54 + 1
        ret407 = Tuple2_SExpr_int32{
            _0: t270,
            _1: t271,
        }
    }
    return ret407
}

func parse_program(tokens__60 []Token) []SExpr {
    var ret408 []SExpr
    var i__61 *ref_int32_x = ref__Ref_int32(0)
    var acc__62 []SExpr = nil
    var exprs__63 *ref_vec_sexpr_x = ref__Ref_Vec_SExpr(acc__62)
    var cond409 bool
    for {
        var t272 int32 = ref_get__Ref_int32(i__61)
        var t273 int32 = int32(len(tokens__60))
        cond409 = t272 < t273
        if !cond409 {
            break
        }
        var t274 int32 = ref_get__Ref_int32(i__61)
        var mtmp69 Tuple2_SExpr_int32 = parse_expr(tokens__60, t274)
        var x70 SExpr = mtmp69._0
        var x71 int32 = mtmp69._1
        var next__65 int32 = x71
        var expr__64 SExpr = x70
        var t276 []SExpr = ref_get__Ref_Vec_SExpr(exprs__63)
        var t275 []SExpr = append(t276, expr__64)
        ref_set__Ref_Vec_SExpr(exprs__63, t275)
        ref_set__Ref_int32(i__61, next__65)
    }
    ret408 = ref_get__Ref_Vec_SExpr(exprs__63)
    return ret408
}

func value_to_string(value__66 Value) string {
    var ret410 string
    switch value__66 := value__66.(type) {
    case Value_Int:
        var x74 int32 = value__66._0
        var n__67 int32 = x74
        ret410 = int32_to_string(n__67)
    case Value_Bool:
        var x75 bool = value__66._0
        var b__68 bool = x75
        ret410 = bool_to_string(b__68)
    case Func:
        ret410 = "<lambda>"
    case Nil:
        ret410 = "nil"
    }
    return ret410
}

func truthy(value__69 Value) bool {
    var ret411 bool
    switch value__69 := value__69.(type) {
    case Value_Int:
        var x77 int32 = value__69._0
        var n__71 int32 = x77
        ret411 = n__71 != 0
    case Value_Bool:
        var x78 bool = value__69._0
        var b__70 bool = x78
        ret411 = b__70
    case Func:
        ret411 = true
    case Nil:
        ret411 = false
    }
    return ret411
}

func eval(expr__72 SExpr, local__73 []Binding, global__74 *ref_vec_binding_x) Value {
    var ret412 Value
    switch expr__72 := expr__72.(type) {
    case SExpr_Int:
        var x80 int32 = expr__72._0
        var n__75 int32 = x80
        ret412 = Value_Int{
            _0: n__75,
        }
    case SExpr_Bool:
        var x81 bool = expr__72._0
        var b__76 bool = x81
        ret412 = Value_Bool{
            _0: b__76,
        }
    case SExpr_Sym:
        var x82 string = expr__72._0
        var name__77 string = x82
        var t277 []Binding = ref_get__Ref_Vec_Binding(global__74)
        ret412 = lookup(local__73, t277, name__77)
    case List:
        var x83 []SExpr = expr__72._0
        var items__78 []SExpr = x83
        ret412 = eval_list(items__78, local__73, global__74)
    }
    return ret412
}

func eval_list(items__79 []SExpr, local__80 []Binding, global__81 *ref_vec_binding_x) Value {
    var ret413 Value
    var t279 int32 = int32(len(items__79))
    var t278 bool = t279 == 0
    if t278 {
        ret413 = Nil{}
    } else {
        var head__82 SExpr = items__79[0]
        switch head__82 := head__82.(type) {
        case SExpr_Int:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            ret413 = apply(f__84, args__85, global__81)
        case SExpr_Bool:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            ret413 = apply(f__84, args__85, global__81)
        case SExpr_Sym:
            var x86 string = head__82._0
            var name__83 string = x86
            ret413 = eval_list_sym(name__83, items__79, local__80, global__81)
        case List:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            ret413 = apply(f__84, args__85, global__81)
        }
    }
    return ret413
}

func eval_list_sym(name__86 string, items__87 []SExpr, local__88 []Binding, global__89 *ref_vec_binding_x) Value {
    var ret414 Value
    switch name__86 {
    case "begin":
        ret414 = eval_begin(items__87, 1, local__88, global__89)
    case "define":
        var t280 int32 = int32(len(items__87))
        var mtmp88 bool = t280 == 3
        switch mtmp88 {
        case true:
            var mtmp89 SExpr = items__87[1]
            switch mtmp89 := mtmp89.(type) {
            case SExpr_Int:
                ret414 = Nil{}
            case SExpr_Bool:
                ret414 = Nil{}
            case SExpr_Sym:
                var x92 string = mtmp89._0
                var var__90 string = x92
                var t281 SExpr = items__87[2]
                var value__91 Value = eval(t281, local__88, global__89)
                var env__92 []Binding = ref_get__Ref_Vec_Binding(global__89)
                var t282 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 []Binding = append(env__92, t282)
                ref_set__Ref_Vec_Binding(global__89, updated__93)
                ret414 = value__91
            case List:
                ret414 = Nil{}
            }
        case false:
            ret414 = Nil{}
        }
    case "if":
        var t283 int32 = int32(len(items__87))
        var mtmp95 bool = t283 == 4
        switch mtmp95 {
        case true:
            var t284 SExpr = items__87[1]
            var cond__94 Value = eval(t284, local__88, global__89)
            var mtmp96 bool = truthy(cond__94)
            switch mtmp96 {
            case true:
                var t285 SExpr = items__87[2]
                ret414 = eval(t285, local__88, global__89)
            case false:
                var t286 SExpr = items__87[3]
                ret414 = eval(t286, local__88, global__89)
            }
        case false:
            ret414 = Nil{}
        }
    case "lambda":
        var t287 int32 = int32(len(items__87))
        var mtmp97 bool = t287 == 3
        switch mtmp97 {
        case true:
            var mtmp98 SExpr = items__87[1]
            switch mtmp98 := mtmp98.(type) {
            case SExpr_Int:
                ret414 = Nil{}
            case SExpr_Bool:
                ret414 = Nil{}
            case SExpr_Sym:
                ret414 = Nil{}
            case List:
                var x102 []SExpr = mtmp98._0
                var params_exprs__95 []SExpr = x102
                var params__96 []string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = items__87[2]
                var t288 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                ret414 = Func{
                    _0: t288,
                }
            }
        case false:
            ret414 = Nil{}
        }
    case "+":
        var t289 []Value = eval_args(items__87, 1, local__88, global__89)
        ret414 = apply_builtin("+", t289)
    case "-":
        var t290 []Value = eval_args(items__87, 1, local__88, global__89)
        ret414 = apply_builtin("-", t290)
    case "*":
        var t291 []Value = eval_args(items__87, 1, local__88, global__89)
        ret414 = apply_builtin("*", t291)
    case "/":
        var t292 []Value = eval_args(items__87, 1, local__88, global__89)
        ret414 = apply_builtin("/", t292)
    case "=":
        var t293 []Value = eval_args(items__87, 1, local__88, global__89)
        ret414 = apply_builtin("=", t293)
    default:
        var t294 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t294, local__88, global__89)
        var args__99 []Value = eval_args(items__87, 1, local__88, global__89)
        ret414 = apply(f__98, args__99, global__89)
    }
    return ret414
}

func eval_begin(items__100 []SExpr, start__101 int32, local__102 []Binding, global__103 *ref_vec_binding_x) Value {
    var ret415 Value
    var i__104 *ref_int32_x = ref__Ref_int32(start__101)
    var t295 Value = Nil{}
    var last__105 *ref_value_x = ref__Ref_Value(t295)
    var cond416 bool
    for {
        var t296 int32 = ref_get__Ref_int32(i__104)
        var t297 int32 = int32(len(items__100))
        cond416 = t296 < t297
        if !cond416 {
            break
        }
        var t299 int32 = ref_get__Ref_int32(i__104)
        var t298 SExpr = items__100[t299]
        var v__106 Value = eval(t298, local__102, global__103)
        ref_set__Ref_Value(last__105, v__106)
        var t301 int32 = ref_get__Ref_int32(i__104)
        var t300 int32 = t301 + 1
        ref_set__Ref_int32(i__104, t300)
    }
    ret415 = ref_get__Ref_Value(last__105)
    return ret415
}

func params_from_sexprs(items__107 []SExpr) []string {
    var ret417 []string
    var i__108 *ref_int32_x = ref__Ref_int32(0)
    var acc__109 []string = nil
    var params__110 *ref_vec_string_x = ref__Ref_Vec_string(acc__109)
    var cond418 bool
    for {
        var t302 int32 = ref_get__Ref_int32(i__108)
        var t303 int32 = int32(len(items__107))
        cond418 = t302 < t303
        if !cond418 {
            break
        }
        var t304 int32 = ref_get__Ref_int32(i__108)
        var mtmp105 SExpr = items__107[t304]
        switch mtmp105 := mtmp105.(type) {
        case SExpr_Int:
            var t306 int32 = ref_get__Ref_int32(i__108)
            var t305 int32 = t306 + 1
            ref_set__Ref_int32(i__108, t305)
        case SExpr_Bool:
            var t308 int32 = ref_get__Ref_int32(i__108)
            var t307 int32 = t308 + 1
            ref_set__Ref_int32(i__108, t307)
        case SExpr_Sym:
            var x108 string = mtmp105._0
            var name__111 string = x108
            var t310 []string = ref_get__Ref_Vec_string(params__110)
            var t309 []string = append(t310, name__111)
            ref_set__Ref_Vec_string(params__110, t309)
            var t312 int32 = ref_get__Ref_int32(i__108)
            var t311 int32 = t312 + 1
            ref_set__Ref_int32(i__108, t311)
        case List:
            var t314 int32 = ref_get__Ref_int32(i__108)
            var t313 int32 = t314 + 1
            ref_set__Ref_int32(i__108, t313)
        }
    }
    ret417 = ref_get__Ref_Vec_string(params__110)
    return ret417
}

func eval_args(items__112 []SExpr, start__113 int32, local__114 []Binding, global__115 *ref_vec_binding_x) []Value {
    var ret419 []Value
    var i__116 *ref_int32_x = ref__Ref_int32(start__113)
    var acc__117 []Value = nil
    var args__118 *ref_vec_value_x = ref__Ref_Vec_Value(acc__117)
    var cond420 bool
    for {
        var t315 int32 = ref_get__Ref_int32(i__116)
        var t316 int32 = int32(len(items__112))
        cond420 = t315 < t316
        if !cond420 {
            break
        }
        var t318 int32 = ref_get__Ref_int32(i__116)
        var t317 SExpr = items__112[t318]
        var v__119 Value = eval(t317, local__114, global__115)
        var t320 []Value = ref_get__Ref_Vec_Value(args__118)
        var t319 []Value = append(t320, v__119)
        ref_set__Ref_Vec_Value(args__118, t319)
        var t322 int32 = ref_get__Ref_int32(i__116)
        var t321 int32 = t322 + 1
        ref_set__Ref_int32(i__116, t321)
    }
    ret419 = ref_get__Ref_Vec_Value(args__118)
    return ret419
}

func apply_builtin(name__120 string, args__121 []Value) Value {
    var ret421 Value
    switch name__120 {
    case "=":
        var t323 int32 = int32(len(args__121))
        var mtmp114 bool = t323 == 2
        switch mtmp114 {
        case true:
            var t324 Value = args__121[0]
            var t325 Value = args__121[1]
            var mtmp115 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t324,
                _1: t325,
            }
            var x116 Value = mtmp115._0
            var x117 Value = mtmp115._1
            switch x117 := x117.(type) {
            case Value_Int:
                var x118 int32 = x117._0
                switch x116 := x116.(type) {
                case Value_Int:
                    var x121 int32 = x116._0
                    var a__122 int32 = x121
                    var b__123 int32 = x118
                    var t326 bool = a__122 == b__123
                    ret421 = Value_Bool{
                        _0: t326,
                    }
                case Value_Bool:
                    ret421 = Value_Bool{
                        _0: false,
                    }
                case Func:
                    ret421 = Value_Bool{
                        _0: false,
                    }
                case Nil:
                    ret421 = Value_Bool{
                        _0: false,
                    }
                }
            case Value_Bool:
                var x119 bool = x117._0
                switch x116 := x116.(type) {
                case Value_Int:
                    ret421 = Value_Bool{
                        _0: false,
                    }
                case Value_Bool:
                    var x125 bool = x116._0
                    var a__124 bool = x125
                    var b__125 bool = x119
                    var t327 bool = a__124 == b__125
                    ret421 = Value_Bool{
                        _0: t327,
                    }
                case Func:
                    ret421 = Value_Bool{
                        _0: false,
                    }
                case Nil:
                    ret421 = Value_Bool{
                        _0: false,
                    }
                }
            case Func:
                ret421 = Value_Bool{
                    _0: false,
                }
            case Nil:
                ret421 = Value_Bool{
                    _0: false,
                }
            }
        case false:
            ret421 = Value_Bool{
                _0: false,
            }
        }
    case "+":
        var i__126 *ref_int32_x = ref__Ref_int32(0)
        var acc__127 *ref_int32_x = ref__Ref_int32(0)
        var cond422 bool
        for {
            var t328 int32 = ref_get__Ref_int32(i__126)
            var t329 int32 = int32(len(args__121))
            cond422 = t328 < t329
            if !cond422 {
                break
            }
            var t330 int32 = ref_get__Ref_int32(i__126)
            var mtmp127 Value = args__121[t330]
            switch mtmp127 := mtmp127.(type) {
            case Value_Int:
                var x128 int32 = mtmp127._0
                var n__128 int32 = x128
                var t332 int32 = ref_get__Ref_int32(acc__127)
                var t331 int32 = t332 + n__128
                ref_set__Ref_int32(acc__127, t331)
                var t334 int32 = ref_get__Ref_int32(i__126)
                var t333 int32 = t334 + 1
                ref_set__Ref_int32(i__126, t333)
            case Value_Bool:
                var t336 int32 = ref_get__Ref_int32(i__126)
                var t335 int32 = t336 + 1
                ref_set__Ref_int32(i__126, t335)
            case Func:
                var t338 int32 = ref_get__Ref_int32(i__126)
                var t337 int32 = t338 + 1
                ref_set__Ref_int32(i__126, t337)
            case Nil:
                var t340 int32 = ref_get__Ref_int32(i__126)
                var t339 int32 = t340 + 1
                ref_set__Ref_int32(i__126, t339)
            }
        }
        var t341 int32 = ref_get__Ref_int32(acc__127)
        ret421 = Value_Int{
            _0: t341,
        }
    case "*":
        var i__129 *ref_int32_x = ref__Ref_int32(0)
        var acc__130 *ref_int32_x = ref__Ref_int32(1)
        var cond423 bool
        for {
            var t342 int32 = ref_get__Ref_int32(i__129)
            var t343 int32 = int32(len(args__121))
            cond423 = t342 < t343
            if !cond423 {
                break
            }
            var t344 int32 = ref_get__Ref_int32(i__129)
            var mtmp133 Value = args__121[t344]
            switch mtmp133 := mtmp133.(type) {
            case Value_Int:
                var x134 int32 = mtmp133._0
                var n__131 int32 = x134
                var t346 int32 = ref_get__Ref_int32(acc__130)
                var t345 int32 = t346 * n__131
                ref_set__Ref_int32(acc__130, t345)
                var t348 int32 = ref_get__Ref_int32(i__129)
                var t347 int32 = t348 + 1
                ref_set__Ref_int32(i__129, t347)
            case Value_Bool:
                var t350 int32 = ref_get__Ref_int32(i__129)
                var t349 int32 = t350 + 1
                ref_set__Ref_int32(i__129, t349)
            case Func:
                var t352 int32 = ref_get__Ref_int32(i__129)
                var t351 int32 = t352 + 1
                ref_set__Ref_int32(i__129, t351)
            case Nil:
                var t354 int32 = ref_get__Ref_int32(i__129)
                var t353 int32 = t354 + 1
                ref_set__Ref_int32(i__129, t353)
            }
        }
        var t355 int32 = ref_get__Ref_int32(acc__130)
        ret421 = Value_Int{
            _0: t355,
        }
    case "-":
        var mtmp139 int32 = int32(len(args__121))
        switch mtmp139 {
        case 1:
            var mtmp140 Value = args__121[0]
            switch mtmp140 := mtmp140.(type) {
            case Value_Int:
                var x141 int32 = mtmp140._0
                var n__132 int32 = x141
                var t356 int32 = 0 - n__132
                ret421 = Value_Int{
                    _0: t356,
                }
            case Value_Bool:
                ret421 = Nil{}
            case Func:
                ret421 = Nil{}
            case Nil:
                ret421 = Nil{}
            }
        case 2:
            var t357 Value = args__121[0]
            var t358 Value = args__121[1]
            var mtmp144 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t357,
                _1: t358,
            }
            var x145 Value = mtmp144._0
            var x146 Value = mtmp144._1
            switch x146 := x146.(type) {
            case Value_Int:
                var x147 int32 = x146._0
                switch x145 := x145.(type) {
                case Value_Int:
                    var x150 int32 = x145._0
                    var a__133 int32 = x150
                    var b__134 int32 = x147
                    var t359 int32 = a__133 - b__134
                    ret421 = Value_Int{
                        _0: t359,
                    }
                case Value_Bool:
                    ret421 = Nil{}
                case Func:
                    ret421 = Nil{}
                case Nil:
                    ret421 = Nil{}
                }
            case Value_Bool:
                ret421 = Nil{}
            case Func:
                ret421 = Nil{}
            case Nil:
                ret421 = Nil{}
            }
        default:
            ret421 = Nil{}
        }
    case "/":
        var t360 int32 = int32(len(args__121))
        var mtmp153 bool = t360 == 2
        switch mtmp153 {
        case true:
            var t361 Value = args__121[0]
            var t362 Value = args__121[1]
            var mtmp154 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t361,
                _1: t362,
            }
            var x155 Value = mtmp154._0
            var x156 Value = mtmp154._1
            switch x156 := x156.(type) {
            case Value_Int:
                var x157 int32 = x156._0
                switch x155 := x155.(type) {
                case Value_Int:
                    var x160 int32 = x155._0
                    var a__135 int32 = x160
                    var b__136 int32 = x157
                    var t363 int32 = a__135 / b__136
                    ret421 = Value_Int{
                        _0: t363,
                    }
                case Value_Bool:
                    ret421 = Nil{}
                case Func:
                    ret421 = Nil{}
                case Nil:
                    ret421 = Nil{}
                }
            case Value_Bool:
                ret421 = Nil{}
            case Func:
                ret421 = Nil{}
            case Nil:
                ret421 = Nil{}
            }
        case false:
            ret421 = Nil{}
        }
    default:
        ret421 = Nil{}
    }
    return ret421
}

func apply(func__137 Value, args__138 []Value, global__139 *ref_vec_binding_x) Value {
    var ret424 Value
    switch func__137 := func__137.(type) {
    case Value_Int:
        ret424 = Nil{}
    case Value_Bool:
        ret424 = Nil{}
    case Func:
        var x165 Lambda = func__137._0
        var fun__140 Lambda = x165
        ret424 = apply_lambda(fun__140, args__138)
    case Nil:
        ret424 = Nil{}
    }
    return ret424
}

func apply_lambda(lambda__141 Lambda, args__142 []Value) Value {
    var ret425 Value
    var t364 []Binding = lambda__141.env
    var env__143 *ref_vec_binding_x = ref__Ref_Vec_Binding(t364)
    var i__144 *ref_int32_x = ref__Ref_int32(0)
    var cond426 bool
    for {
        var t366 int32 = ref_get__Ref_int32(i__144)
        var t368 []string = lambda__141.params
        var t367 int32 = int32(len(t368))
        var t365 bool = t366 < t367
        var t370 int32 = ref_get__Ref_int32(i__144)
        var t371 int32 = int32(len(args__142))
        var t369 bool = t370 < t371
        cond426 = t365 && t369
        if !cond426 {
            break
        }
        var t372 []string = lambda__141.params
        var t373 int32 = ref_get__Ref_int32(i__144)
        var name__145 string = t372[t373]
        var t374 int32 = ref_get__Ref_int32(i__144)
        var value__146 Value = args__142[t374]
        var t375 []Binding = ref_get__Ref_Vec_Binding(env__143)
        var t376 Binding = Binding{
            name: name__145,
            value: value__146,
        }
        var updated__147 []Binding = append(t375, t376)
        ref_set__Ref_Vec_Binding(env__143, updated__147)
        var t378 int32 = ref_get__Ref_int32(i__144)
        var t377 int32 = t378 + 1
        ref_set__Ref_int32(i__144, t377)
    }
    var t379 SExpr = lambda__141.body
    var t380 []Binding = ref_get__Ref_Vec_Binding(env__143)
    var t381 *ref_vec_binding_x = lambda__141.global
    ret425 = eval(t379, t380, t381)
    return ret425
}

func main0() struct{} {
    var ret427 struct{}
    var t382 []Binding = nil
    var global__148 *ref_vec_binding_x = ref__Ref_Vec_Binding(t382)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t383 []Token = lex(program__149)
    var exprs__150 []SExpr = parse_program(t383)
    var t384 SExpr = exprs__150[0]
    var t385 []Binding = nil
    var result__151 Value = eval(t384, t385, global__148)
    var t386 string = value_to_string(result__151)
    string_println(t386)
    var t387 []Token = lex("(add3 10 20 30)")
    var exprs2__152 []SExpr = parse_program(t387)
    var t388 SExpr = exprs2__152[0]
    var t389 []Binding = nil
    var result2__153 Value = eval(t388, t389, global__148)
    var t390 string = value_to_string(result2__153)
    string_println(t390)
    ret427 = struct{}{}
    return ret427
}

func main() {
    main0()
}
