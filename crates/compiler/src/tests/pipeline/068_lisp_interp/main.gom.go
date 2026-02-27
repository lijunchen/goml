package main

import (
    "fmt"
    "unicode/utf8"
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

func string_get(s string, i int32) rune {
    return rune(s[i])
}

func char_to_string(x rune) string {
    if !utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
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

func is_digit(ch__0 rune) bool {
    var ret389 bool
    var t148 bool = ch__0 >= 48
    var t149 bool = ch__0 <= 57
    ret389 = t148 && t149
    return ret389
}

func digit_value(ch__1 rune) int32 {
    var ret390 int32
    switch ch__1 {
    case 48:
        ret390 = 0
    case 49:
        ret390 = 1
    case 50:
        ret390 = 2
    case 51:
        ret390 = 3
    case 52:
        ret390 = 4
    case 53:
        ret390 = 5
    case 54:
        ret390 = 6
    case 55:
        ret390 = 7
    case 56:
        ret390 = 8
    case 57:
        ret390 = 9
    default:
        ret390 = 0
    }
    return ret390
}

func is_int_text(text__2 string) bool {
    var ret391 bool
    var len__3 int32 = string_len(text__2)
    var t150 bool = len__3 == 0
    if t150 {
        ret391 = false
    } else {
        var i__4 *ref_int32_x = ref__Ref_int32(0)
        var saw_digit__5 *ref_bool_x = ref__Ref_bool(false)
        var ok__6 *ref_bool_x = ref__Ref_bool(true)
        var started__7 *ref_bool_x = ref__Ref_bool(false)
        var cond392 bool
        for {
            var t151 bool = ref_get__Ref_bool(ok__6)
            var t153 int32 = ref_get__Ref_int32(i__4)
            var t152 bool = t153 < len__3
            cond392 = t151 && t152
            if !cond392 {
                break
            }
            var t154 int32 = ref_get__Ref_int32(i__4)
            var ch__8 rune = string_get(text__2, t154)
            var t157 bool = ref_get__Ref_bool(started__7)
            var t156 bool = !t157
            var t158 bool = ch__8 == 45
            var t155 bool = t156 && t158
            if t155 {
                ref_set__Ref_bool(started__7, true)
                var t160 int32 = ref_get__Ref_int32(i__4)
                var t159 int32 = t160 + 1
                ref_set__Ref_int32(i__4, t159)
            } else {
                var t161 bool = is_digit(ch__8)
                if t161 {
                    ref_set__Ref_bool(started__7, true)
                    ref_set__Ref_bool(saw_digit__5, true)
                    var t163 int32 = ref_get__Ref_int32(i__4)
                    var t162 int32 = t163 + 1
                    ref_set__Ref_int32(i__4, t162)
                } else {
                    ref_set__Ref_bool(ok__6, false)
                }
            }
        }
        var t164 bool = ref_get__Ref_bool(ok__6)
        var t165 bool = ref_get__Ref_bool(saw_digit__5)
        ret391 = t164 && t165
    }
    return ret391
}

func parse_int32(text__9 string) int32 {
    var ret393 int32
    var len__10 int32 = string_len(text__9)
    var i__11 *ref_int32_x = ref__Ref_int32(0)
    var negative__12 *ref_bool_x = ref__Ref_bool(false)
    var started__13 *ref_bool_x = ref__Ref_bool(false)
    var acc__14 *ref_int32_x = ref__Ref_int32(0)
    var cond394 bool
    for {
        var t166 int32 = ref_get__Ref_int32(i__11)
        cond394 = t166 < len__10
        if !cond394 {
            break
        }
        var t167 int32 = ref_get__Ref_int32(i__11)
        var ch__15 rune = string_get(text__9, t167)
        var t170 bool = ref_get__Ref_bool(started__13)
        var t169 bool = !t170
        var t171 bool = ch__15 == 45
        var t168 bool = t169 && t171
        if t168 {
            ref_set__Ref_bool(started__13, true)
            ref_set__Ref_bool(negative__12, true)
            var t173 int32 = ref_get__Ref_int32(i__11)
            var t172 int32 = t173 + 1
            ref_set__Ref_int32(i__11, t172)
        } else {
            ref_set__Ref_bool(started__13, true)
            var d__16 int32 = digit_value(ch__15)
            var t176 int32 = ref_get__Ref_int32(acc__14)
            var t175 int32 = t176 * 10
            var t174 int32 = t175 + d__16
            ref_set__Ref_int32(acc__14, t174)
            var t178 int32 = ref_get__Ref_int32(i__11)
            var t177 int32 = t178 + 1
            ref_set__Ref_int32(i__11, t177)
        }
    }
    var t179 bool = ref_get__Ref_bool(negative__12)
    if t179 {
        var t180 int32 = ref_get__Ref_int32(acc__14)
        ret393 = 0 - t180
    } else {
        ret393 = ref_get__Ref_int32(acc__14)
    }
    return ret393
}

func is_delim(ch__17 rune) bool {
    var ret395 bool
    var t182 bool = ch__17 == 40
    var t183 bool = ch__17 == 41
    var t181 bool = t182 || t183
    var t184 bool = ch__17 == 32
    ret395 = t181 || t184
    return ret395
}

func lex_atom(source__18 string, start__19 int32) Tuple2_Token_int32 {
    var ret396 Tuple2_Token_int32
    var len__20 int32 = string_len(source__18)
    var text__21 *ref_string_x = ref__Ref_string("")
    var i__22 *ref_int32_x = ref__Ref_int32(start__19)
    var done__23 *ref_bool_x = ref__Ref_bool(false)
    var cond397 bool
    for {
        var t186 bool = ref_get__Ref_bool(done__23)
        var t185 bool = !t186
        var t188 int32 = ref_get__Ref_int32(i__22)
        var t187 bool = t188 < len__20
        cond397 = t185 && t187
        if !cond397 {
            break
        }
        var t189 int32 = ref_get__Ref_int32(i__22)
        var ch__24 rune = string_get(source__18, t189)
        var t190 bool = is_delim(ch__24)
        if t190 {
            ref_set__Ref_bool(done__23, true)
        } else {
            var t192 string = ref_get__Ref_string(text__21)
            var t193 string = char_to_string(ch__24)
            var t191 string = t192 + t193
            ref_set__Ref_string(text__21, t191)
            var t195 int32 = ref_get__Ref_int32(i__22)
            var t194 int32 = t195 + 1
            ref_set__Ref_int32(i__22, t194)
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
        var t196 bool = is_int_text(atom__25)
        if t196 {
            var t197 int32 = parse_int32(atom__25)
            token__26 = Token_Int{
                _0: t197,
            }
        } else {
            token__26 = Token_Sym{
                _0: atom__25,
            }
        }
    }
    var t198 int32 = ref_get__Ref_int32(i__22)
    ret396 = Tuple2_Token_int32{
        _0: token__26,
        _1: t198,
    }
    return ret396
}

func lex(source__27 string) []Token {
    var ret398 []Token
    var len__28 int32 = string_len(source__27)
    var toks0__29 []Token = nil
    var toks__30 *ref_vec_token_x = ref__Ref_Vec_Token(toks0__29)
    var i__31 *ref_int32_x = ref__Ref_int32(0)
    var cond399 bool
    for {
        var t199 int32 = ref_get__Ref_int32(i__31)
        cond399 = t199 < len__28
        if !cond399 {
            break
        }
        var t200 int32 = ref_get__Ref_int32(i__31)
        var ch__32 rune = string_get(source__27, t200)
        var t201 bool = ch__32 == 40
        if t201 {
            var t203 []Token = ref_get__Ref_Vec_Token(toks__30)
            var t204 Token = LParen{}
            var t202 []Token = append(t203, t204)
            ref_set__Ref_Vec_Token(toks__30, t202)
            var t206 int32 = ref_get__Ref_int32(i__31)
            var t205 int32 = t206 + 1
            ref_set__Ref_int32(i__31, t205)
        } else {
            var t207 bool = ch__32 == 41
            if t207 {
                var t209 []Token = ref_get__Ref_Vec_Token(toks__30)
                var t210 Token = RParen{}
                var t208 []Token = append(t209, t210)
                ref_set__Ref_Vec_Token(toks__30, t208)
                var t212 int32 = ref_get__Ref_int32(i__31)
                var t211 int32 = t212 + 1
                ref_set__Ref_int32(i__31, t211)
            } else {
                var t213 bool = ch__32 == 32
                if t213 {
                    var t215 int32 = ref_get__Ref_int32(i__31)
                    var t214 int32 = t215 + 1
                    ref_set__Ref_int32(i__31, t214)
                } else {
                    var t216 int32 = ref_get__Ref_int32(i__31)
                    var mtmp13 Tuple2_Token_int32 = lex_atom(source__27, t216)
                    var x14 Token = mtmp13._0
                    var x15 int32 = mtmp13._1
                    var next__34 int32 = x15
                    var tok__33 Token = x14
                    var t218 []Token = ref_get__Ref_Vec_Token(toks__30)
                    var t217 []Token = append(t218, tok__33)
                    ref_set__Ref_Vec_Token(toks__30, t217)
                    ref_set__Ref_int32(i__31, next__34)
                }
            }
        }
    }
    ret398 = ref_get__Ref_Vec_Token(toks__30)
    return ret398
}

func env_lookup(env__35 []Binding, name__36 string) Value {
    var ret400 Value
    var t220 int32 = int32(len(env__35))
    var t219 int32 = t220 - 1
    var i__37 *ref_int32_x = ref__Ref_int32(t219)
    var t221 Value = Nil{}
    var result__38 *ref_value_x = ref__Ref_Value(t221)
    var done__39 *ref_bool_x = ref__Ref_bool(false)
    var cond401 bool
    for {
        var t223 bool = ref_get__Ref_bool(done__39)
        var t222 bool = !t223
        var t225 int32 = ref_get__Ref_int32(i__37)
        var t224 bool = t225 >= 0
        cond401 = t222 && t224
        if !cond401 {
            break
        }
        var t226 int32 = ref_get__Ref_int32(i__37)
        var binding__40 Binding = env__35[t226]
        var t228 string = binding__40.name
        var t227 bool = t228 == name__36
        if t227 {
            var t229 Value = binding__40.value
            ref_set__Ref_Value(result__38, t229)
            ref_set__Ref_bool(done__39, true)
        } else {
            var t231 int32 = ref_get__Ref_int32(i__37)
            var t230 int32 = t231 - 1
            ref_set__Ref_int32(i__37, t230)
        }
    }
    ret400 = ref_get__Ref_Value(result__38)
    return ret400
}

func lookup(local__41 []Binding, global__42 []Binding, name__43 string) Value {
    var ret402 Value
    var mtmp20 Value = env_lookup(local__41, name__43)
    switch mtmp20 := mtmp20.(type) {
    case Value_Int:
        var other__44 Value = mtmp20
        ret402 = other__44
    case Value_Bool:
        var other__44 Value = mtmp20
        ret402 = other__44
    case Func:
        var other__44 Value = mtmp20
        ret402 = other__44
    case Nil:
        ret402 = env_lookup(global__42, name__43)
    }
    return ret402
}

func parse_list(tokens__45 []Token, start__46 int32) Tuple2_Vec_SExpr_int32 {
    var ret403 Tuple2_Vec_SExpr_int32
    var acc__47 []SExpr = nil
    var exprs__48 *ref_vec_sexpr_x = ref__Ref_Vec_SExpr(acc__47)
    var i__49 *ref_int32_x = ref__Ref_int32(start__46)
    var done__50 *ref_bool_x = ref__Ref_bool(false)
    var cond404 bool
    for {
        var t233 bool = ref_get__Ref_bool(done__50)
        var t232 bool = !t233
        var t235 int32 = ref_get__Ref_int32(i__49)
        var t236 int32 = int32(len(tokens__45))
        var t234 bool = t235 < t236
        cond404 = t232 && t234
        if !cond404 {
            break
        }
        var t237 int32 = ref_get__Ref_int32(i__49)
        var mtmp24 Token = tokens__45[t237]
        switch mtmp24.(type) {
        case LParen:
            var t238 int32 = ref_get__Ref_int32(i__49)
            var mtmp28 Tuple2_SExpr_int32 = parse_expr(tokens__45, t238)
            var x29 SExpr = mtmp28._0
            var x30 int32 = mtmp28._1
            var next__52 int32 = x30
            var expr__51 SExpr = x29
            var t240 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t239 []SExpr = append(t240, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t239)
            ref_set__Ref_int32(i__49, next__52)
        case RParen:
            ref_set__Ref_bool(done__50, true)
            var t242 int32 = ref_get__Ref_int32(i__49)
            var t241 int32 = t242 + 1
            ref_set__Ref_int32(i__49, t241)
        case Token_Sym:
            var t243 int32 = ref_get__Ref_int32(i__49)
            var mtmp33 Tuple2_SExpr_int32 = parse_expr(tokens__45, t243)
            var x34 SExpr = mtmp33._0
            var x35 int32 = mtmp33._1
            var next__52 int32 = x35
            var expr__51 SExpr = x34
            var t245 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t244 []SExpr = append(t245, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t244)
            ref_set__Ref_int32(i__49, next__52)
        case Token_Int:
            var t246 int32 = ref_get__Ref_int32(i__49)
            var mtmp37 Tuple2_SExpr_int32 = parse_expr(tokens__45, t246)
            var x38 SExpr = mtmp37._0
            var x39 int32 = mtmp37._1
            var next__52 int32 = x39
            var expr__51 SExpr = x38
            var t248 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t247 []SExpr = append(t248, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t247)
            ref_set__Ref_int32(i__49, next__52)
        case Token_Bool:
            var t249 int32 = ref_get__Ref_int32(i__49)
            var mtmp41 Tuple2_SExpr_int32 = parse_expr(tokens__45, t249)
            var x42 SExpr = mtmp41._0
            var x43 int32 = mtmp41._1
            var next__52 int32 = x43
            var expr__51 SExpr = x42
            var t251 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t250 []SExpr = append(t251, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t250)
            ref_set__Ref_int32(i__49, next__52)
        }
    }
    var t252 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
    var t253 int32 = ref_get__Ref_int32(i__49)
    ret403 = Tuple2_Vec_SExpr_int32{
        _0: t252,
        _1: t253,
    }
    return ret403
}

func parse_expr(tokens__53 []Token, start__54 int32) Tuple2_SExpr_int32 {
    var ret405 Tuple2_SExpr_int32
    var mtmp46 Token = tokens__53[start__54]
    switch mtmp46 := mtmp46.(type) {
    case LParen:
        var t254 int32 = start__54 + 1
        var mtmp50 Tuple2_Vec_SExpr_int32 = parse_list(tokens__53, t254)
        var x51 []SExpr = mtmp50._0
        var x52 int32 = mtmp50._1
        var next__56 int32 = x52
        var items__55 []SExpr = x51
        var t255 SExpr = List{
            _0: items__55,
        }
        ret405 = Tuple2_SExpr_int32{
            _0: t255,
            _1: next__56,
        }
    case RParen:
        var t256 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t257 int32 = start__54 + 1
        ret405 = Tuple2_SExpr_int32{
            _0: t256,
            _1: t257,
        }
    case Token_Sym:
        var x47 string = mtmp46._0
        var name__59 string = x47
        var t258 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t259 int32 = start__54 + 1
        ret405 = Tuple2_SExpr_int32{
            _0: t258,
            _1: t259,
        }
    case Token_Int:
        var x48 int32 = mtmp46._0
        var n__58 int32 = x48
        var t260 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t261 int32 = start__54 + 1
        ret405 = Tuple2_SExpr_int32{
            _0: t260,
            _1: t261,
        }
    case Token_Bool:
        var x49 bool = mtmp46._0
        var b__57 bool = x49
        var t262 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t263 int32 = start__54 + 1
        ret405 = Tuple2_SExpr_int32{
            _0: t262,
            _1: t263,
        }
    }
    return ret405
}

func parse_program(tokens__60 []Token) []SExpr {
    var ret406 []SExpr
    var i__61 *ref_int32_x = ref__Ref_int32(0)
    var acc__62 []SExpr = nil
    var exprs__63 *ref_vec_sexpr_x = ref__Ref_Vec_SExpr(acc__62)
    var cond407 bool
    for {
        var t264 int32 = ref_get__Ref_int32(i__61)
        var t265 int32 = int32(len(tokens__60))
        cond407 = t264 < t265
        if !cond407 {
            break
        }
        var t266 int32 = ref_get__Ref_int32(i__61)
        var mtmp53 Tuple2_SExpr_int32 = parse_expr(tokens__60, t266)
        var x54 SExpr = mtmp53._0
        var x55 int32 = mtmp53._1
        var next__65 int32 = x55
        var expr__64 SExpr = x54
        var t268 []SExpr = ref_get__Ref_Vec_SExpr(exprs__63)
        var t267 []SExpr = append(t268, expr__64)
        ref_set__Ref_Vec_SExpr(exprs__63, t267)
        ref_set__Ref_int32(i__61, next__65)
    }
    ret406 = ref_get__Ref_Vec_SExpr(exprs__63)
    return ret406
}

func value_to_string(value__66 Value) string {
    var ret408 string
    switch value__66 := value__66.(type) {
    case Value_Int:
        var x58 int32 = value__66._0
        var n__67 int32 = x58
        ret408 = int32_to_string(n__67)
    case Value_Bool:
        var x59 bool = value__66._0
        var b__68 bool = x59
        ret408 = bool_to_string(b__68)
    case Func:
        ret408 = "<lambda>"
    case Nil:
        ret408 = "nil"
    }
    return ret408
}

func truthy(value__69 Value) bool {
    var ret409 bool
    switch value__69 := value__69.(type) {
    case Value_Int:
        var x61 int32 = value__69._0
        var n__71 int32 = x61
        ret409 = n__71 != 0
    case Value_Bool:
        var x62 bool = value__69._0
        var b__70 bool = x62
        ret409 = b__70
    case Func:
        ret409 = true
    case Nil:
        ret409 = false
    }
    return ret409
}

func eval(expr__72 SExpr, local__73 []Binding, global__74 *ref_vec_binding_x) Value {
    var ret410 Value
    switch expr__72 := expr__72.(type) {
    case SExpr_Int:
        var x64 int32 = expr__72._0
        var n__75 int32 = x64
        ret410 = Value_Int{
            _0: n__75,
        }
    case SExpr_Bool:
        var x65 bool = expr__72._0
        var b__76 bool = x65
        ret410 = Value_Bool{
            _0: b__76,
        }
    case SExpr_Sym:
        var x66 string = expr__72._0
        var name__77 string = x66
        var t269 []Binding = ref_get__Ref_Vec_Binding(global__74)
        ret410 = lookup(local__73, t269, name__77)
    case List:
        var x67 []SExpr = expr__72._0
        var items__78 []SExpr = x67
        ret410 = eval_list(items__78, local__73, global__74)
    }
    return ret410
}

func eval_list(items__79 []SExpr, local__80 []Binding, global__81 *ref_vec_binding_x) Value {
    var ret411 Value
    var t271 int32 = int32(len(items__79))
    var t270 bool = t271 == 0
    if t270 {
        ret411 = Nil{}
    } else {
        var head__82 SExpr = items__79[0]
        switch head__82 := head__82.(type) {
        case SExpr_Int:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            ret411 = apply(f__84, args__85, global__81)
        case SExpr_Bool:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            ret411 = apply(f__84, args__85, global__81)
        case SExpr_Sym:
            var x70 string = head__82._0
            var name__83 string = x70
            ret411 = eval_list_sym(name__83, items__79, local__80, global__81)
        case List:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            ret411 = apply(f__84, args__85, global__81)
        }
    }
    return ret411
}

func eval_list_sym(name__86 string, items__87 []SExpr, local__88 []Binding, global__89 *ref_vec_binding_x) Value {
    var ret412 Value
    switch name__86 {
    case "begin":
        ret412 = eval_begin(items__87, 1, local__88, global__89)
    case "define":
        var t273 int32 = int32(len(items__87))
        var t272 bool = t273 == 3
        if t272 {
            var mtmp72 SExpr = items__87[1]
            switch mtmp72 := mtmp72.(type) {
            case SExpr_Int:
                ret412 = Nil{}
            case SExpr_Bool:
                ret412 = Nil{}
            case SExpr_Sym:
                var x75 string = mtmp72._0
                var var__90 string = x75
                var t274 SExpr = items__87[2]
                var value__91 Value = eval(t274, local__88, global__89)
                var env__92 []Binding = ref_get__Ref_Vec_Binding(global__89)
                var t275 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 []Binding = append(env__92, t275)
                ref_set__Ref_Vec_Binding(global__89, updated__93)
                ret412 = value__91
            case List:
                ret412 = Nil{}
            }
        } else {
            ret412 = Nil{}
        }
    case "if":
        var t277 int32 = int32(len(items__87))
        var t276 bool = t277 == 4
        if t276 {
            var t278 SExpr = items__87[1]
            var cond__94 Value = eval(t278, local__88, global__89)
            var t279 bool = truthy(cond__94)
            if t279 {
                var t280 SExpr = items__87[2]
                ret412 = eval(t280, local__88, global__89)
            } else {
                var t281 SExpr = items__87[3]
                ret412 = eval(t281, local__88, global__89)
            }
        } else {
            ret412 = Nil{}
        }
    case "lambda":
        var t283 int32 = int32(len(items__87))
        var t282 bool = t283 == 3
        if t282 {
            var mtmp78 SExpr = items__87[1]
            switch mtmp78 := mtmp78.(type) {
            case SExpr_Int:
                ret412 = Nil{}
            case SExpr_Bool:
                ret412 = Nil{}
            case SExpr_Sym:
                ret412 = Nil{}
            case List:
                var x82 []SExpr = mtmp78._0
                var params_exprs__95 []SExpr = x82
                var params__96 []string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = items__87[2]
                var t284 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                ret412 = Func{
                    _0: t284,
                }
            }
        } else {
            ret412 = Nil{}
        }
    case "+":
        var t285 []Value = eval_args(items__87, 1, local__88, global__89)
        ret412 = apply_builtin("+", t285)
    case "-":
        var t286 []Value = eval_args(items__87, 1, local__88, global__89)
        ret412 = apply_builtin("-", t286)
    case "*":
        var t287 []Value = eval_args(items__87, 1, local__88, global__89)
        ret412 = apply_builtin("*", t287)
    case "/":
        var t288 []Value = eval_args(items__87, 1, local__88, global__89)
        ret412 = apply_builtin("/", t288)
    case "=":
        var t289 []Value = eval_args(items__87, 1, local__88, global__89)
        ret412 = apply_builtin("=", t289)
    default:
        var t290 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t290, local__88, global__89)
        var args__99 []Value = eval_args(items__87, 1, local__88, global__89)
        ret412 = apply(f__98, args__99, global__89)
    }
    return ret412
}

func eval_begin(items__100 []SExpr, start__101 int32, local__102 []Binding, global__103 *ref_vec_binding_x) Value {
    var ret413 Value
    var i__104 *ref_int32_x = ref__Ref_int32(start__101)
    var t291 Value = Nil{}
    var last__105 *ref_value_x = ref__Ref_Value(t291)
    var cond414 bool
    for {
        var t292 int32 = ref_get__Ref_int32(i__104)
        var t293 int32 = int32(len(items__100))
        cond414 = t292 < t293
        if !cond414 {
            break
        }
        var t295 int32 = ref_get__Ref_int32(i__104)
        var t294 SExpr = items__100[t295]
        var v__106 Value = eval(t294, local__102, global__103)
        ref_set__Ref_Value(last__105, v__106)
        var t297 int32 = ref_get__Ref_int32(i__104)
        var t296 int32 = t297 + 1
        ref_set__Ref_int32(i__104, t296)
    }
    ret413 = ref_get__Ref_Value(last__105)
    return ret413
}

func params_from_sexprs(items__107 []SExpr) []string {
    var ret415 []string
    var i__108 *ref_int32_x = ref__Ref_int32(0)
    var acc__109 []string = nil
    var params__110 *ref_vec_string_x = ref__Ref_Vec_string(acc__109)
    var cond416 bool
    for {
        var t298 int32 = ref_get__Ref_int32(i__108)
        var t299 int32 = int32(len(items__107))
        cond416 = t298 < t299
        if !cond416 {
            break
        }
        var t300 int32 = ref_get__Ref_int32(i__108)
        var mtmp85 SExpr = items__107[t300]
        switch mtmp85 := mtmp85.(type) {
        case SExpr_Int:
            var t302 int32 = ref_get__Ref_int32(i__108)
            var t301 int32 = t302 + 1
            ref_set__Ref_int32(i__108, t301)
        case SExpr_Bool:
            var t304 int32 = ref_get__Ref_int32(i__108)
            var t303 int32 = t304 + 1
            ref_set__Ref_int32(i__108, t303)
        case SExpr_Sym:
            var x88 string = mtmp85._0
            var name__111 string = x88
            var t306 []string = ref_get__Ref_Vec_string(params__110)
            var t305 []string = append(t306, name__111)
            ref_set__Ref_Vec_string(params__110, t305)
            var t308 int32 = ref_get__Ref_int32(i__108)
            var t307 int32 = t308 + 1
            ref_set__Ref_int32(i__108, t307)
        case List:
            var t310 int32 = ref_get__Ref_int32(i__108)
            var t309 int32 = t310 + 1
            ref_set__Ref_int32(i__108, t309)
        }
    }
    ret415 = ref_get__Ref_Vec_string(params__110)
    return ret415
}

func eval_args(items__112 []SExpr, start__113 int32, local__114 []Binding, global__115 *ref_vec_binding_x) []Value {
    var ret417 []Value
    var i__116 *ref_int32_x = ref__Ref_int32(start__113)
    var acc__117 []Value = nil
    var args__118 *ref_vec_value_x = ref__Ref_Vec_Value(acc__117)
    var cond418 bool
    for {
        var t311 int32 = ref_get__Ref_int32(i__116)
        var t312 int32 = int32(len(items__112))
        cond418 = t311 < t312
        if !cond418 {
            break
        }
        var t314 int32 = ref_get__Ref_int32(i__116)
        var t313 SExpr = items__112[t314]
        var v__119 Value = eval(t313, local__114, global__115)
        var t316 []Value = ref_get__Ref_Vec_Value(args__118)
        var t315 []Value = append(t316, v__119)
        ref_set__Ref_Vec_Value(args__118, t315)
        var t318 int32 = ref_get__Ref_int32(i__116)
        var t317 int32 = t318 + 1
        ref_set__Ref_int32(i__116, t317)
    }
    ret417 = ref_get__Ref_Vec_Value(args__118)
    return ret417
}

func apply_builtin(name__120 string, args__121 []Value) Value {
    var ret419 Value
    switch name__120 {
    case "=":
        var t320 int32 = int32(len(args__121))
        var t319 bool = t320 == 2
        if t319 {
            var t321 Value = args__121[0]
            var t322 Value = args__121[1]
            var mtmp94 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t321,
                _1: t322,
            }
            var x95 Value = mtmp94._0
            var x96 Value = mtmp94._1
            switch x96 := x96.(type) {
            case Value_Int:
                var x97 int32 = x96._0
                switch x95 := x95.(type) {
                case Value_Int:
                    var x100 int32 = x95._0
                    var a__122 int32 = x100
                    var b__123 int32 = x97
                    var t323 bool = a__122 == b__123
                    ret419 = Value_Bool{
                        _0: t323,
                    }
                case Value_Bool:
                    ret419 = Value_Bool{
                        _0: false,
                    }
                case Func:
                    ret419 = Value_Bool{
                        _0: false,
                    }
                case Nil:
                    ret419 = Value_Bool{
                        _0: false,
                    }
                }
            case Value_Bool:
                var x98 bool = x96._0
                switch x95 := x95.(type) {
                case Value_Int:
                    ret419 = Value_Bool{
                        _0: false,
                    }
                case Value_Bool:
                    var x104 bool = x95._0
                    var a__124 bool = x104
                    var b__125 bool = x98
                    var t324 bool = a__124 == b__125
                    ret419 = Value_Bool{
                        _0: t324,
                    }
                case Func:
                    ret419 = Value_Bool{
                        _0: false,
                    }
                case Nil:
                    ret419 = Value_Bool{
                        _0: false,
                    }
                }
            case Func:
                ret419 = Value_Bool{
                    _0: false,
                }
            case Nil:
                ret419 = Value_Bool{
                    _0: false,
                }
            }
        } else {
            ret419 = Value_Bool{
                _0: false,
            }
        }
    case "+":
        var i__126 *ref_int32_x = ref__Ref_int32(0)
        var acc__127 *ref_int32_x = ref__Ref_int32(0)
        var cond420 bool
        for {
            var t325 int32 = ref_get__Ref_int32(i__126)
            var t326 int32 = int32(len(args__121))
            cond420 = t325 < t326
            if !cond420 {
                break
            }
            var t327 int32 = ref_get__Ref_int32(i__126)
            var mtmp106 Value = args__121[t327]
            switch mtmp106 := mtmp106.(type) {
            case Value_Int:
                var x107 int32 = mtmp106._0
                var n__128 int32 = x107
                var t329 int32 = ref_get__Ref_int32(acc__127)
                var t328 int32 = t329 + n__128
                ref_set__Ref_int32(acc__127, t328)
                var t331 int32 = ref_get__Ref_int32(i__126)
                var t330 int32 = t331 + 1
                ref_set__Ref_int32(i__126, t330)
            case Value_Bool:
                var t333 int32 = ref_get__Ref_int32(i__126)
                var t332 int32 = t333 + 1
                ref_set__Ref_int32(i__126, t332)
            case Func:
                var t335 int32 = ref_get__Ref_int32(i__126)
                var t334 int32 = t335 + 1
                ref_set__Ref_int32(i__126, t334)
            case Nil:
                var t337 int32 = ref_get__Ref_int32(i__126)
                var t336 int32 = t337 + 1
                ref_set__Ref_int32(i__126, t336)
            }
        }
        var t338 int32 = ref_get__Ref_int32(acc__127)
        ret419 = Value_Int{
            _0: t338,
        }
    case "*":
        var i__129 *ref_int32_x = ref__Ref_int32(0)
        var acc__130 *ref_int32_x = ref__Ref_int32(1)
        var cond421 bool
        for {
            var t339 int32 = ref_get__Ref_int32(i__129)
            var t340 int32 = int32(len(args__121))
            cond421 = t339 < t340
            if !cond421 {
                break
            }
            var t341 int32 = ref_get__Ref_int32(i__129)
            var mtmp112 Value = args__121[t341]
            switch mtmp112 := mtmp112.(type) {
            case Value_Int:
                var x113 int32 = mtmp112._0
                var n__131 int32 = x113
                var t343 int32 = ref_get__Ref_int32(acc__130)
                var t342 int32 = t343 * n__131
                ref_set__Ref_int32(acc__130, t342)
                var t345 int32 = ref_get__Ref_int32(i__129)
                var t344 int32 = t345 + 1
                ref_set__Ref_int32(i__129, t344)
            case Value_Bool:
                var t347 int32 = ref_get__Ref_int32(i__129)
                var t346 int32 = t347 + 1
                ref_set__Ref_int32(i__129, t346)
            case Func:
                var t349 int32 = ref_get__Ref_int32(i__129)
                var t348 int32 = t349 + 1
                ref_set__Ref_int32(i__129, t348)
            case Nil:
                var t351 int32 = ref_get__Ref_int32(i__129)
                var t350 int32 = t351 + 1
                ref_set__Ref_int32(i__129, t350)
            }
        }
        var t352 int32 = ref_get__Ref_int32(acc__130)
        ret419 = Value_Int{
            _0: t352,
        }
    case "-":
        var mtmp118 int32 = int32(len(args__121))
        switch mtmp118 {
        case 1:
            var mtmp119 Value = args__121[0]
            switch mtmp119 := mtmp119.(type) {
            case Value_Int:
                var x120 int32 = mtmp119._0
                var n__132 int32 = x120
                var t353 int32 = 0 - n__132
                ret419 = Value_Int{
                    _0: t353,
                }
            case Value_Bool:
                ret419 = Nil{}
            case Func:
                ret419 = Nil{}
            case Nil:
                ret419 = Nil{}
            }
        case 2:
            var t354 Value = args__121[0]
            var t355 Value = args__121[1]
            var mtmp123 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t354,
                _1: t355,
            }
            var x124 Value = mtmp123._0
            var x125 Value = mtmp123._1
            switch x125 := x125.(type) {
            case Value_Int:
                var x126 int32 = x125._0
                switch x124 := x124.(type) {
                case Value_Int:
                    var x129 int32 = x124._0
                    var a__133 int32 = x129
                    var b__134 int32 = x126
                    var t356 int32 = a__133 - b__134
                    ret419 = Value_Int{
                        _0: t356,
                    }
                case Value_Bool:
                    ret419 = Nil{}
                case Func:
                    ret419 = Nil{}
                case Nil:
                    ret419 = Nil{}
                }
            case Value_Bool:
                ret419 = Nil{}
            case Func:
                ret419 = Nil{}
            case Nil:
                ret419 = Nil{}
            }
        default:
            ret419 = Nil{}
        }
    case "/":
        var t358 int32 = int32(len(args__121))
        var t357 bool = t358 == 2
        if t357 {
            var t359 Value = args__121[0]
            var t360 Value = args__121[1]
            var mtmp132 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t359,
                _1: t360,
            }
            var x133 Value = mtmp132._0
            var x134 Value = mtmp132._1
            switch x134 := x134.(type) {
            case Value_Int:
                var x135 int32 = x134._0
                switch x133 := x133.(type) {
                case Value_Int:
                    var x138 int32 = x133._0
                    var a__135 int32 = x138
                    var b__136 int32 = x135
                    var t361 int32 = a__135 / b__136
                    ret419 = Value_Int{
                        _0: t361,
                    }
                case Value_Bool:
                    ret419 = Nil{}
                case Func:
                    ret419 = Nil{}
                case Nil:
                    ret419 = Nil{}
                }
            case Value_Bool:
                ret419 = Nil{}
            case Func:
                ret419 = Nil{}
            case Nil:
                ret419 = Nil{}
            }
        } else {
            ret419 = Nil{}
        }
    default:
        ret419 = Nil{}
    }
    return ret419
}

func apply(func__137 Value, args__138 []Value, global__139 *ref_vec_binding_x) Value {
    var ret422 Value
    switch func__137 := func__137.(type) {
    case Value_Int:
        ret422 = Nil{}
    case Value_Bool:
        ret422 = Nil{}
    case Func:
        var x143 Lambda = func__137._0
        var fun__140 Lambda = x143
        ret422 = apply_lambda(fun__140, args__138)
    case Nil:
        ret422 = Nil{}
    }
    return ret422
}

func apply_lambda(lambda__141 Lambda, args__142 []Value) Value {
    var ret423 Value
    var t362 []Binding = lambda__141.env
    var env__143 *ref_vec_binding_x = ref__Ref_Vec_Binding(t362)
    var i__144 *ref_int32_x = ref__Ref_int32(0)
    var cond424 bool
    for {
        var t364 int32 = ref_get__Ref_int32(i__144)
        var t366 []string = lambda__141.params
        var t365 int32 = int32(len(t366))
        var t363 bool = t364 < t365
        var t368 int32 = ref_get__Ref_int32(i__144)
        var t369 int32 = int32(len(args__142))
        var t367 bool = t368 < t369
        cond424 = t363 && t367
        if !cond424 {
            break
        }
        var t370 []string = lambda__141.params
        var t371 int32 = ref_get__Ref_int32(i__144)
        var name__145 string = t370[t371]
        var t372 int32 = ref_get__Ref_int32(i__144)
        var value__146 Value = args__142[t372]
        var t373 []Binding = ref_get__Ref_Vec_Binding(env__143)
        var t374 Binding = Binding{
            name: name__145,
            value: value__146,
        }
        var updated__147 []Binding = append(t373, t374)
        ref_set__Ref_Vec_Binding(env__143, updated__147)
        var t376 int32 = ref_get__Ref_int32(i__144)
        var t375 int32 = t376 + 1
        ref_set__Ref_int32(i__144, t375)
    }
    var t377 SExpr = lambda__141.body
    var t378 []Binding = ref_get__Ref_Vec_Binding(env__143)
    var t379 *ref_vec_binding_x = lambda__141.global
    ret423 = eval(t377, t378, t379)
    return ret423
}

func main0() struct{} {
    var ret425 struct{}
    var t380 []Binding = nil
    var global__148 *ref_vec_binding_x = ref__Ref_Vec_Binding(t380)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t381 []Token = lex(program__149)
    var exprs__150 []SExpr = parse_program(t381)
    var t382 SExpr = exprs__150[0]
    var t383 []Binding = nil
    var result__151 Value = eval(t382, t383, global__148)
    var t384 string = value_to_string(result__151)
    string_println(t384)
    var t385 []Token = lex("(add3 10 20 30)")
    var exprs2__152 []SExpr = parse_program(t385)
    var t386 SExpr = exprs2__152[0]
    var t387 []Binding = nil
    var result2__153 Value = eval(t386, t387, global__148)
    var t388 string = value_to_string(result2__153)
    string_println(t388)
    ret425 = struct{}{}
    return ret425
}

func main() {
    main0()
}
