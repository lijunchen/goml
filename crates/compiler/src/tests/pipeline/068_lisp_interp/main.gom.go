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
    var t148 bool = ch__0 >= 48
    var t149 bool = ch__0 <= 57
    var t150 bool = t148 && t149
    return t150
}

func digit_value(ch__1 rune) int32 {
    var jp152 int32
    switch ch__1 {
    case 48:
        jp152 = 0
    case 49:
        jp152 = 1
    case 50:
        jp152 = 2
    case 51:
        jp152 = 3
    case 52:
        jp152 = 4
    case 53:
        jp152 = 5
    case 54:
        jp152 = 6
    case 55:
        jp152 = 7
    case 56:
        jp152 = 8
    case 57:
        jp152 = 9
    default:
        jp152 = 0
    }
    return jp152
}

func is_int_text(text__2 string) bool {
    var len__3 int32 = string_len(text__2)
    var t155 bool = len__3 == 0
    var jp154 bool
    if t155 {
        jp154 = false
        return jp154
    } else {
        var i__4 *ref_int32_x = ref__Ref_int32(0)
        var saw_digit__5 *ref_bool_x = ref__Ref_bool(false)
        var ok__6 *ref_bool_x = ref__Ref_bool(true)
        var started__7 *ref_bool_x = ref__Ref_bool(false)
        for {
            var t161 bool = ref_get__Ref_bool(ok__6)
            var t162 int32 = ref_get__Ref_int32(i__4)
            var t163 bool = t162 < len__3
            var t164 bool = t161 && t163
            if !t164 {
                break
            }
            var t165 int32 = ref_get__Ref_int32(i__4)
            var ch__8 rune = string_get(text__2, t165)
            var t167 bool = ref_get__Ref_bool(started__7)
            var t168 bool = !t167
            var t169 bool = ch__8 == 45
            var t170 bool = t168 && t169
            if t170 {
                ref_set__Ref_bool(started__7, true)
                var t171 int32 = ref_get__Ref_int32(i__4)
                var t172 int32 = t171 + 1
                ref_set__Ref_int32(i__4, t172)
            } else {
                var t175 bool = is_digit(ch__8)
                if t175 {
                    ref_set__Ref_bool(started__7, true)
                    ref_set__Ref_bool(saw_digit__5, true)
                    var t176 int32 = ref_get__Ref_int32(i__4)
                    var t177 int32 = t176 + 1
                    ref_set__Ref_int32(i__4, t177)
                } else {
                    ref_set__Ref_bool(ok__6, false)
                }
            }
        }
        var t157 bool = ref_get__Ref_bool(ok__6)
        var t158 bool = ref_get__Ref_bool(saw_digit__5)
        var t159 bool = t157 && t158
        jp154 = t159
        return jp154
    }
}

func parse_int32(text__9 string) int32 {
    var len__10 int32 = string_len(text__9)
    var i__11 *ref_int32_x = ref__Ref_int32(0)
    var negative__12 *ref_bool_x = ref__Ref_bool(false)
    var started__13 *ref_bool_x = ref__Ref_bool(false)
    var acc__14 *ref_int32_x = ref__Ref_int32(0)
    for {
        var t188 int32 = ref_get__Ref_int32(i__11)
        var t189 bool = t188 < len__10
        if !t189 {
            break
        }
        var t190 int32 = ref_get__Ref_int32(i__11)
        var ch__15 rune = string_get(text__9, t190)
        var t192 bool = ref_get__Ref_bool(started__13)
        var t193 bool = !t192
        var t194 bool = ch__15 == 45
        var t195 bool = t193 && t194
        if t195 {
            ref_set__Ref_bool(started__13, true)
            ref_set__Ref_bool(negative__12, true)
            var t196 int32 = ref_get__Ref_int32(i__11)
            var t197 int32 = t196 + 1
            ref_set__Ref_int32(i__11, t197)
        } else {
            ref_set__Ref_bool(started__13, true)
            var d__16 int32 = digit_value(ch__15)
            var t199 int32 = ref_get__Ref_int32(acc__14)
            var t200 int32 = t199 * 10
            var t201 int32 = t200 + d__16
            ref_set__Ref_int32(acc__14, t201)
            var t202 int32 = ref_get__Ref_int32(i__11)
            var t203 int32 = t202 + 1
            ref_set__Ref_int32(i__11, t203)
        }
    }
    var t183 bool = ref_get__Ref_bool(negative__12)
    var jp182 int32
    if t183 {
        var t184 int32 = ref_get__Ref_int32(acc__14)
        var t185 int32 = 0 - t184
        jp182 = t185
    } else {
        var t186 int32 = ref_get__Ref_int32(acc__14)
        jp182 = t186
    }
    return jp182
}

func is_delim(ch__17 rune) bool {
    var t205 bool = ch__17 == 40
    var t206 bool = ch__17 == 41
    var t207 bool = t205 || t206
    var t208 bool = ch__17 == 32
    var t209 bool = t207 || t208
    return t209
}

func lex_atom(source__18 string, start__19 int32) Tuple2_Token_int32 {
    var len__20 int32 = string_len(source__18)
    var text__21 *ref_string_x = ref__Ref_string("")
    var i__22 *ref_int32_x = ref__Ref_int32(start__19)
    var done__23 *ref_bool_x = ref__Ref_bool(false)
    for {
        var t224 bool = ref_get__Ref_bool(done__23)
        var t225 bool = !t224
        var t226 int32 = ref_get__Ref_int32(i__22)
        var t227 bool = t226 < len__20
        var t228 bool = t225 && t227
        if !t228 {
            break
        }
        var t229 int32 = ref_get__Ref_int32(i__22)
        var ch__24 rune = string_get(source__18, t229)
        var t231 bool = is_delim(ch__24)
        if t231 {
            ref_set__Ref_bool(done__23, true)
        } else {
            var t233 string = ref_get__Ref_string(text__21)
            var t234 string = char_to_string(ch__24)
            var t235 string = t233 + t234
            ref_set__Ref_string(text__21, t235)
            var t236 int32 = ref_get__Ref_int32(i__22)
            var t237 int32 = t236 + 1
            ref_set__Ref_int32(i__22, t237)
        }
    }
    var atom__25 string = ref_get__Ref_string(text__21)
    var jp212 Token
    switch atom__25 {
    case "true":
        var t215 Token = Token_Bool{
            _0: true,
        }
        jp212 = t215
        var token__26 Token = jp212
        var t213 int32 = ref_get__Ref_int32(i__22)
        var t214 Tuple2_Token_int32 = Tuple2_Token_int32{
            _0: token__26,
            _1: t213,
        }
        return t214
    case "false":
        var t216 Token = Token_Bool{
            _0: false,
        }
        jp212 = t216
        var token__26 Token = jp212
        var t213 int32 = ref_get__Ref_int32(i__22)
        var t214 Tuple2_Token_int32 = Tuple2_Token_int32{
            _0: token__26,
            _1: t213,
        }
        return t214
    default:
        var t219 bool = is_int_text(atom__25)
        var jp218 Token
        if t219 {
            var t220 int32 = parse_int32(atom__25)
            var t221 Token = Token_Int{
                _0: t220,
            }
            jp218 = t221
        } else {
            var t222 Token = Token_Sym{
                _0: atom__25,
            }
            jp218 = t222
        }
        jp212 = jp218
        var token__26 Token = jp212
        var t213 int32 = ref_get__Ref_int32(i__22)
        var t214 Tuple2_Token_int32 = Tuple2_Token_int32{
            _0: token__26,
            _1: t213,
        }
        return t214
    }
}

func lex(source__27 string) []Token {
    var len__28 int32 = string_len(source__27)
    var toks0__29 []Token = nil
    var toks__30 *ref_vec_token_x = ref__Ref_Vec_Token(toks0__29)
    var i__31 *ref_int32_x = ref__Ref_int32(0)
    for {
        var t242 int32 = ref_get__Ref_int32(i__31)
        var t243 bool = t242 < len__28
        if !t243 {
            break
        }
        var t244 int32 = ref_get__Ref_int32(i__31)
        var ch__32 rune = string_get(source__27, t244)
        var t246 bool = ch__32 == 40
        if t246 {
            var t247 []Token = ref_get__Ref_Vec_Token(toks__30)
            var t248 []Token = append(t247, LParen{})
            ref_set__Ref_Vec_Token(toks__30, t248)
            var t249 int32 = ref_get__Ref_int32(i__31)
            var t250 int32 = t249 + 1
            ref_set__Ref_int32(i__31, t250)
        } else {
            var t253 bool = ch__32 == 41
            if t253 {
                var t254 []Token = ref_get__Ref_Vec_Token(toks__30)
                var t255 []Token = append(t254, RParen{})
                ref_set__Ref_Vec_Token(toks__30, t255)
                var t256 int32 = ref_get__Ref_int32(i__31)
                var t257 int32 = t256 + 1
                ref_set__Ref_int32(i__31, t257)
            } else {
                var t260 bool = ch__32 == 32
                if t260 {
                    var t261 int32 = ref_get__Ref_int32(i__31)
                    var t262 int32 = t261 + 1
                    ref_set__Ref_int32(i__31, t262)
                } else {
                    var t264 int32 = ref_get__Ref_int32(i__31)
                    var mtmp13 Tuple2_Token_int32 = lex_atom(source__27, t264)
                    var x14 Token = mtmp13._0
                    var x15 int32 = mtmp13._1
                    var next__34 int32 = x15
                    var tok__33 Token = x14
                    var t265 []Token = ref_get__Ref_Vec_Token(toks__30)
                    var t266 []Token = append(t265, tok__33)
                    ref_set__Ref_Vec_Token(toks__30, t266)
                    ref_set__Ref_int32(i__31, next__34)
                }
            }
        }
    }
    var t240 []Token = ref_get__Ref_Vec_Token(toks__30)
    return t240
}

func env_lookup(env__35 []Binding, name__36 string) Value {
    var t268 int32 = int32(len(env__35))
    var t269 int32 = t268 - 1
    var i__37 *ref_int32_x = ref__Ref_int32(t269)
    var result__38 *ref_value_x = ref__Ref_Value(Nil{})
    var done__39 *ref_bool_x = ref__Ref_bool(false)
    for {
        var t273 bool = ref_get__Ref_bool(done__39)
        var t274 bool = !t273
        var t275 int32 = ref_get__Ref_int32(i__37)
        var t276 bool = t275 >= 0
        var t277 bool = t274 && t276
        if !t277 {
            break
        }
        var t278 int32 = ref_get__Ref_int32(i__37)
        var binding__40 Binding = env__35[t278]
        var t280 string = binding__40.name
        var t281 bool = t280 == name__36
        if t281 {
            var t282 Value = binding__40.value
            ref_set__Ref_Value(result__38, t282)
            ref_set__Ref_bool(done__39, true)
        } else {
            var t284 int32 = ref_get__Ref_int32(i__37)
            var t285 int32 = t284 - 1
            ref_set__Ref_int32(i__37, t285)
        }
    }
    var t271 Value = ref_get__Ref_Value(result__38)
    return t271
}

func lookup(local__41 []Binding, global__42 []Binding, name__43 string) Value {
    var mtmp20 Value = env_lookup(local__41, name__43)
    var jp288 Value
    switch mtmp20.(type) {
    case Value_Int:
        var other__44 Value = mtmp20
        jp288 = other__44
    case Value_Bool:
        var other__44 Value = mtmp20
        jp288 = other__44
    case Func:
        var other__44 Value = mtmp20
        jp288 = other__44
    case Nil:
        var t289 Value = env_lookup(global__42, name__43)
        jp288 = t289
    default:
        panic("non-exhaustive match")
    }
    return jp288
}

func parse_list(tokens__45 []Token, start__46 int32) Tuple2_Vec_SExpr_int32 {
    var acc__47 []SExpr = nil
    var exprs__48 *ref_vec_sexpr_x = ref__Ref_Vec_SExpr(acc__47)
    var i__49 *ref_int32_x = ref__Ref_int32(start__46)
    var done__50 *ref_bool_x = ref__Ref_bool(false)
    for {
        var t295 bool = ref_get__Ref_bool(done__50)
        var t296 bool = !t295
        var t297 int32 = ref_get__Ref_int32(i__49)
        var t298 int32 = int32(len(tokens__45))
        var t299 bool = t297 < t298
        var t300 bool = t296 && t299
        if !t300 {
            break
        }
        var t301 int32 = ref_get__Ref_int32(i__49)
        var mtmp24 Token = tokens__45[t301]
        switch mtmp24.(type) {
        case LParen:
            var t303 int32 = ref_get__Ref_int32(i__49)
            var mtmp28 Tuple2_SExpr_int32 = parse_expr(tokens__45, t303)
            var x29 SExpr = mtmp28._0
            var x30 int32 = mtmp28._1
            var next__52 int32 = x30
            var expr__51 SExpr = x29
            var t304 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t305 []SExpr = append(t304, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t305)
            ref_set__Ref_int32(i__49, next__52)
        case RParen:
            ref_set__Ref_bool(done__50, true)
            var t307 int32 = ref_get__Ref_int32(i__49)
            var t308 int32 = t307 + 1
            ref_set__Ref_int32(i__49, t308)
        case Token_Sym:
            var t310 int32 = ref_get__Ref_int32(i__49)
            var mtmp33 Tuple2_SExpr_int32 = parse_expr(tokens__45, t310)
            var x34 SExpr = mtmp33._0
            var x35 int32 = mtmp33._1
            var next__52 int32 = x35
            var expr__51 SExpr = x34
            var t311 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t312 []SExpr = append(t311, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t312)
            ref_set__Ref_int32(i__49, next__52)
        case Token_Int:
            var t314 int32 = ref_get__Ref_int32(i__49)
            var mtmp37 Tuple2_SExpr_int32 = parse_expr(tokens__45, t314)
            var x38 SExpr = mtmp37._0
            var x39 int32 = mtmp37._1
            var next__52 int32 = x39
            var expr__51 SExpr = x38
            var t315 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t316 []SExpr = append(t315, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t316)
            ref_set__Ref_int32(i__49, next__52)
        case Token_Bool:
            var t318 int32 = ref_get__Ref_int32(i__49)
            var mtmp41 Tuple2_SExpr_int32 = parse_expr(tokens__45, t318)
            var x42 SExpr = mtmp41._0
            var x43 int32 = mtmp41._1
            var next__52 int32 = x43
            var expr__51 SExpr = x42
            var t319 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t320 []SExpr = append(t319, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t320)
            ref_set__Ref_int32(i__49, next__52)
        default:
            panic("non-exhaustive match")
        }
    }
    var t291 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
    var t292 int32 = ref_get__Ref_int32(i__49)
    var t293 Tuple2_Vec_SExpr_int32 = Tuple2_Vec_SExpr_int32{
        _0: t291,
        _1: t292,
    }
    return t293
}

func parse_expr(tokens__53 []Token, start__54 int32) Tuple2_SExpr_int32 {
    var mtmp46 Token = tokens__53[start__54]
    var jp323 Tuple2_SExpr_int32
    switch mtmp46.(type) {
    case LParen:
        var t324 int32 = start__54 + 1
        var mtmp50 Tuple2_Vec_SExpr_int32 = parse_list(tokens__53, t324)
        var x51 []SExpr = mtmp50._0
        var x52 int32 = mtmp50._1
        var next__56 int32 = x52
        var items__55 []SExpr = x51
        var t325 SExpr = List{
            _0: items__55,
        }
        var t326 Tuple2_SExpr_int32 = Tuple2_SExpr_int32{
            _0: t325,
            _1: next__56,
        }
        jp323 = t326
    case RParen:
        var t327 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t328 int32 = start__54 + 1
        var t329 Tuple2_SExpr_int32 = Tuple2_SExpr_int32{
            _0: t327,
            _1: t328,
        }
        jp323 = t329
    case Token_Sym:
        var x47 string = mtmp46.(Token_Sym)._0
        var name__59 string = x47
        var t330 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t331 int32 = start__54 + 1
        var t332 Tuple2_SExpr_int32 = Tuple2_SExpr_int32{
            _0: t330,
            _1: t331,
        }
        jp323 = t332
    case Token_Int:
        var x48 int32 = mtmp46.(Token_Int)._0
        var n__58 int32 = x48
        var t333 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t334 int32 = start__54 + 1
        var t335 Tuple2_SExpr_int32 = Tuple2_SExpr_int32{
            _0: t333,
            _1: t334,
        }
        jp323 = t335
    case Token_Bool:
        var x49 bool = mtmp46.(Token_Bool)._0
        var b__57 bool = x49
        var t336 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t337 int32 = start__54 + 1
        var t338 Tuple2_SExpr_int32 = Tuple2_SExpr_int32{
            _0: t336,
            _1: t337,
        }
        jp323 = t338
    default:
        panic("non-exhaustive match")
    }
    return jp323
}

func parse_program(tokens__60 []Token) []SExpr {
    var i__61 *ref_int32_x = ref__Ref_int32(0)
    var acc__62 []SExpr = nil
    var exprs__63 *ref_vec_sexpr_x = ref__Ref_Vec_SExpr(acc__62)
    for {
        var t342 int32 = ref_get__Ref_int32(i__61)
        var t343 int32 = int32(len(tokens__60))
        var t344 bool = t342 < t343
        if !t344 {
            break
        }
        var t345 int32 = ref_get__Ref_int32(i__61)
        var mtmp53 Tuple2_SExpr_int32 = parse_expr(tokens__60, t345)
        var x54 SExpr = mtmp53._0
        var x55 int32 = mtmp53._1
        var next__65 int32 = x55
        var expr__64 SExpr = x54
        var t346 []SExpr = ref_get__Ref_Vec_SExpr(exprs__63)
        var t347 []SExpr = append(t346, expr__64)
        ref_set__Ref_Vec_SExpr(exprs__63, t347)
        ref_set__Ref_int32(i__61, next__65)
    }
    var t340 []SExpr = ref_get__Ref_Vec_SExpr(exprs__63)
    return t340
}

func value_to_string(value__66 Value) string {
    var jp350 string
    switch value__66.(type) {
    case Value_Int:
        var x58 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x58
        var t351 string = int32_to_string(n__67)
        jp350 = t351
    case Value_Bool:
        var x59 bool = value__66.(Value_Bool)._0
        var b__68 bool = x59
        var t352 string = bool_to_string(b__68)
        jp350 = t352
    case Func:
        jp350 = "<lambda>"
    case Nil:
        jp350 = "nil"
    default:
        panic("non-exhaustive match")
    }
    return jp350
}

func truthy(value__69 Value) bool {
    var jp354 bool
    switch value__69.(type) {
    case Value_Int:
        var x61 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x61
        var t355 bool = n__71 != 0
        jp354 = t355
    case Value_Bool:
        var x62 bool = value__69.(Value_Bool)._0
        var b__70 bool = x62
        jp354 = b__70
    case Func:
        jp354 = true
    case Nil:
        jp354 = false
    default:
        panic("non-exhaustive match")
    }
    return jp354
}

func eval(expr__72 SExpr, local__73 []Binding, global__74 *ref_vec_binding_x) Value {
    var jp357 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x64 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x64
        var t358 Value = Value_Int{
            _0: n__75,
        }
        jp357 = t358
    case SExpr_Bool:
        var x65 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x65
        var t359 Value = Value_Bool{
            _0: b__76,
        }
        jp357 = t359
    case SExpr_Sym:
        var x66 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x66
        var t360 []Binding = ref_get__Ref_Vec_Binding(global__74)
        var t361 Value = lookup(local__73, t360, name__77)
        jp357 = t361
    case List:
        var x67 []SExpr = expr__72.(List)._0
        var items__78 []SExpr = x67
        var t362 Value = eval_list(items__78, local__73, global__74)
        jp357 = t362
    default:
        panic("non-exhaustive match")
    }
    return jp357
}

func eval_list(items__79 []SExpr, local__80 []Binding, global__81 *ref_vec_binding_x) Value {
    var t365 int32 = int32(len(items__79))
    var t366 bool = t365 == 0
    var jp364 Value
    if t366 {
        jp364 = Nil{}
        return jp364
    } else {
        var head__82 SExpr = items__79[0]
        var jp368 Value
        switch head__82.(type) {
        case SExpr_Int:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            var t369 Value = apply(f__84, args__85, global__81)
            jp368 = t369
        case SExpr_Bool:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            var t370 Value = apply(f__84, args__85, global__81)
            jp368 = t370
        case SExpr_Sym:
            var x70 string = head__82.(SExpr_Sym)._0
            var name__83 string = x70
            var t371 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp368 = t371
        case List:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            var t372 Value = apply(f__84, args__85, global__81)
            jp368 = t372
        default:
            panic("non-exhaustive match")
        }
        jp364 = jp368
        return jp364
    }
}

func eval_list_sym(name__86 string, items__87 []SExpr, local__88 []Binding, global__89 *ref_vec_binding_x) Value {
    var jp374 Value
    switch name__86 {
    case "begin":
        var t375 Value = eval_begin(items__87, 1, local__88, global__89)
        jp374 = t375
        return jp374
    case "define":
        var t378 int32 = int32(len(items__87))
        var t379 bool = t378 == 3
        var jp377 Value
        if t379 {
            var mtmp72 SExpr = items__87[1]
            var jp381 Value
            switch mtmp72.(type) {
            case SExpr_Int:
                jp381 = Nil{}
            case SExpr_Bool:
                jp381 = Nil{}
            case SExpr_Sym:
                var x75 string = mtmp72.(SExpr_Sym)._0
                var var__90 string = x75
                var t382 SExpr = items__87[2]
                var value__91 Value = eval(t382, local__88, global__89)
                var env__92 []Binding = ref_get__Ref_Vec_Binding(global__89)
                var t383 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 []Binding = append(env__92, t383)
                ref_set__Ref_Vec_Binding(global__89, updated__93)
                jp381 = value__91
            case List:
                jp381 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp377 = jp381
            jp374 = jp377
            return jp374
        } else {
            jp377 = Nil{}
            jp374 = jp377
            return jp374
        }
    case "if":
        var t386 int32 = int32(len(items__87))
        var t387 bool = t386 == 4
        var jp385 Value
        if t387 {
            var t388 SExpr = items__87[1]
            var cond__94 Value = eval(t388, local__88, global__89)
            var t391 bool = truthy(cond__94)
            var jp390 Value
            if t391 {
                var t392 SExpr = items__87[2]
                var t393 Value = eval(t392, local__88, global__89)
                jp390 = t393
            } else {
                var t394 SExpr = items__87[3]
                var t395 Value = eval(t394, local__88, global__89)
                jp390 = t395
            }
            jp385 = jp390
            jp374 = jp385
            return jp374
        } else {
            jp385 = Nil{}
            jp374 = jp385
            return jp374
        }
    case "lambda":
        var t398 int32 = int32(len(items__87))
        var t399 bool = t398 == 3
        var jp397 Value
        if t399 {
            var mtmp78 SExpr = items__87[1]
            var jp401 Value
            switch mtmp78.(type) {
            case SExpr_Int:
                jp401 = Nil{}
            case SExpr_Bool:
                jp401 = Nil{}
            case SExpr_Sym:
                jp401 = Nil{}
            case List:
                var x82 []SExpr = mtmp78.(List)._0
                var params_exprs__95 []SExpr = x82
                var params__96 []string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = items__87[2]
                var t402 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t403 Value = Func{
                    _0: t402,
                }
                jp401 = t403
            default:
                panic("non-exhaustive match")
            }
            jp397 = jp401
            jp374 = jp397
            return jp374
        } else {
            jp397 = Nil{}
            jp374 = jp397
            return jp374
        }
    case "+":
        var t404 []Value = eval_args(items__87, 1, local__88, global__89)
        var t405 Value = apply_builtin("+", t404)
        jp374 = t405
        return jp374
    case "-":
        var t406 []Value = eval_args(items__87, 1, local__88, global__89)
        var t407 Value = apply_builtin("-", t406)
        jp374 = t407
        return jp374
    case "*":
        var t408 []Value = eval_args(items__87, 1, local__88, global__89)
        var t409 Value = apply_builtin("*", t408)
        jp374 = t409
        return jp374
    case "/":
        var t410 []Value = eval_args(items__87, 1, local__88, global__89)
        var t411 Value = apply_builtin("/", t410)
        jp374 = t411
        return jp374
    case "=":
        var t412 []Value = eval_args(items__87, 1, local__88, global__89)
        var t413 Value = apply_builtin("=", t412)
        jp374 = t413
        return jp374
    default:
        var t414 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t414, local__88, global__89)
        var args__99 []Value = eval_args(items__87, 1, local__88, global__89)
        var t415 Value = apply(f__98, args__99, global__89)
        jp374 = t415
        return jp374
    }
}

func eval_begin(items__100 []SExpr, start__101 int32, local__102 []Binding, global__103 *ref_vec_binding_x) Value {
    var i__104 *ref_int32_x = ref__Ref_int32(start__101)
    var last__105 *ref_value_x = ref__Ref_Value(Nil{})
    for {
        var t419 int32 = ref_get__Ref_int32(i__104)
        var t420 int32 = int32(len(items__100))
        var t421 bool = t419 < t420
        if !t421 {
            break
        }
        var t422 int32 = ref_get__Ref_int32(i__104)
        var t423 SExpr = items__100[t422]
        var v__106 Value = eval(t423, local__102, global__103)
        ref_set__Ref_Value(last__105, v__106)
        var t424 int32 = ref_get__Ref_int32(i__104)
        var t425 int32 = t424 + 1
        ref_set__Ref_int32(i__104, t425)
    }
    var t417 Value = ref_get__Ref_Value(last__105)
    return t417
}

func params_from_sexprs(items__107 []SExpr) []string {
    var i__108 *ref_int32_x = ref__Ref_int32(0)
    var acc__109 []string = nil
    var params__110 *ref_vec_string_x = ref__Ref_Vec_string(acc__109)
    for {
        var t430 int32 = ref_get__Ref_int32(i__108)
        var t431 int32 = int32(len(items__107))
        var t432 bool = t430 < t431
        if !t432 {
            break
        }
        var t433 int32 = ref_get__Ref_int32(i__108)
        var mtmp85 SExpr = items__107[t433]
        switch mtmp85.(type) {
        case SExpr_Int:
            var t435 int32 = ref_get__Ref_int32(i__108)
            var t436 int32 = t435 + 1
            ref_set__Ref_int32(i__108, t436)
        case SExpr_Bool:
            var t438 int32 = ref_get__Ref_int32(i__108)
            var t439 int32 = t438 + 1
            ref_set__Ref_int32(i__108, t439)
        case SExpr_Sym:
            var x88 string = mtmp85.(SExpr_Sym)._0
            var name__111 string = x88
            var t441 []string = ref_get__Ref_Vec_string(params__110)
            var t442 []string = append(t441, name__111)
            ref_set__Ref_Vec_string(params__110, t442)
            var t443 int32 = ref_get__Ref_int32(i__108)
            var t444 int32 = t443 + 1
            ref_set__Ref_int32(i__108, t444)
        case List:
            var t446 int32 = ref_get__Ref_int32(i__108)
            var t447 int32 = t446 + 1
            ref_set__Ref_int32(i__108, t447)
        default:
            panic("non-exhaustive match")
        }
    }
    var t428 []string = ref_get__Ref_Vec_string(params__110)
    return t428
}

func eval_args(items__112 []SExpr, start__113 int32, local__114 []Binding, global__115 *ref_vec_binding_x) []Value {
    var i__116 *ref_int32_x = ref__Ref_int32(start__113)
    var acc__117 []Value = nil
    var args__118 *ref_vec_value_x = ref__Ref_Vec_Value(acc__117)
    for {
        var t452 int32 = ref_get__Ref_int32(i__116)
        var t453 int32 = int32(len(items__112))
        var t454 bool = t452 < t453
        if !t454 {
            break
        }
        var t455 int32 = ref_get__Ref_int32(i__116)
        var t456 SExpr = items__112[t455]
        var v__119 Value = eval(t456, local__114, global__115)
        var t457 []Value = ref_get__Ref_Vec_Value(args__118)
        var t458 []Value = append(t457, v__119)
        ref_set__Ref_Vec_Value(args__118, t458)
        var t459 int32 = ref_get__Ref_int32(i__116)
        var t460 int32 = t459 + 1
        ref_set__Ref_int32(i__116, t460)
    }
    var t450 []Value = ref_get__Ref_Vec_Value(args__118)
    return t450
}

func apply_builtin(name__120 string, args__121 []Value) Value {
    var jp463 Value
    switch name__120 {
    case "=":
        var t466 int32 = int32(len(args__121))
        var t467 bool = t466 == 2
        var jp465 Value
        if t467 {
            var t468 Value = args__121[0]
            var t469 Value = args__121[1]
            var mtmp94 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t468,
                _1: t469,
            }
            var x95 Value = mtmp94._0
            var x96 Value = mtmp94._1
            var jp471 Value
            switch x96.(type) {
            case Value_Int:
                var x97 int32 = x96.(Value_Int)._0
                var jp473 Value
                switch x95.(type) {
                case Value_Int:
                    var x100 int32 = x95.(Value_Int)._0
                    var a__122 int32 = x100
                    var b__123 int32 = x97
                    var t474 bool = a__122 == b__123
                    var t475 Value = Value_Bool{
                        _0: t474,
                    }
                    jp473 = t475
                case Value_Bool:
                    var t476 Value = Value_Bool{
                        _0: false,
                    }
                    jp473 = t476
                case Func:
                    var t477 Value = Value_Bool{
                        _0: false,
                    }
                    jp473 = t477
                case Nil:
                    var t478 Value = Value_Bool{
                        _0: false,
                    }
                    jp473 = t478
                default:
                    panic("non-exhaustive match")
                }
                jp471 = jp473
                jp465 = jp471
                jp463 = jp465
                return jp463
            case Value_Bool:
                var x98 bool = x96.(Value_Bool)._0
                var jp480 Value
                switch x95.(type) {
                case Value_Int:
                    var t481 Value = Value_Bool{
                        _0: false,
                    }
                    jp480 = t481
                case Value_Bool:
                    var x104 bool = x95.(Value_Bool)._0
                    var a__124 bool = x104
                    var b__125 bool = x98
                    var t482 bool = a__124 == b__125
                    var t483 Value = Value_Bool{
                        _0: t482,
                    }
                    jp480 = t483
                case Func:
                    var t484 Value = Value_Bool{
                        _0: false,
                    }
                    jp480 = t484
                case Nil:
                    var t485 Value = Value_Bool{
                        _0: false,
                    }
                    jp480 = t485
                default:
                    panic("non-exhaustive match")
                }
                jp471 = jp480
                jp465 = jp471
                jp463 = jp465
                return jp463
            case Func:
                var t486 Value = Value_Bool{
                    _0: false,
                }
                jp471 = t486
                jp465 = jp471
                jp463 = jp465
                return jp463
            case Nil:
                var t487 Value = Value_Bool{
                    _0: false,
                }
                jp471 = t487
                jp465 = jp471
                jp463 = jp465
                return jp463
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t488 Value = Value_Bool{
                _0: false,
            }
            jp465 = t488
            jp463 = jp465
            return jp463
        }
    case "+":
        var i__126 *ref_int32_x = ref__Ref_int32(0)
        var acc__127 *ref_int32_x = ref__Ref_int32(0)
        for {
            var t493 int32 = ref_get__Ref_int32(i__126)
            var t494 int32 = int32(len(args__121))
            var t495 bool = t493 < t494
            if !t495 {
                break
            }
            var t496 int32 = ref_get__Ref_int32(i__126)
            var mtmp106 Value = args__121[t496]
            switch mtmp106.(type) {
            case Value_Int:
                var x107 int32 = mtmp106.(Value_Int)._0
                var n__128 int32 = x107
                var t498 int32 = ref_get__Ref_int32(acc__127)
                var t499 int32 = t498 + n__128
                ref_set__Ref_int32(acc__127, t499)
                var t500 int32 = ref_get__Ref_int32(i__126)
                var t501 int32 = t500 + 1
                ref_set__Ref_int32(i__126, t501)
            case Value_Bool:
                var t503 int32 = ref_get__Ref_int32(i__126)
                var t504 int32 = t503 + 1
                ref_set__Ref_int32(i__126, t504)
            case Func:
                var t506 int32 = ref_get__Ref_int32(i__126)
                var t507 int32 = t506 + 1
                ref_set__Ref_int32(i__126, t507)
            case Nil:
                var t509 int32 = ref_get__Ref_int32(i__126)
                var t510 int32 = t509 + 1
                ref_set__Ref_int32(i__126, t510)
            default:
                panic("non-exhaustive match")
            }
        }
        var t490 int32 = ref_get__Ref_int32(acc__127)
        var t491 Value = Value_Int{
            _0: t490,
        }
        jp463 = t491
        return jp463
    case "*":
        var i__129 *ref_int32_x = ref__Ref_int32(0)
        var acc__130 *ref_int32_x = ref__Ref_int32(1)
        for {
            var t516 int32 = ref_get__Ref_int32(i__129)
            var t517 int32 = int32(len(args__121))
            var t518 bool = t516 < t517
            if !t518 {
                break
            }
            var t519 int32 = ref_get__Ref_int32(i__129)
            var mtmp112 Value = args__121[t519]
            switch mtmp112.(type) {
            case Value_Int:
                var x113 int32 = mtmp112.(Value_Int)._0
                var n__131 int32 = x113
                var t521 int32 = ref_get__Ref_int32(acc__130)
                var t522 int32 = t521 * n__131
                ref_set__Ref_int32(acc__130, t522)
                var t523 int32 = ref_get__Ref_int32(i__129)
                var t524 int32 = t523 + 1
                ref_set__Ref_int32(i__129, t524)
            case Value_Bool:
                var t526 int32 = ref_get__Ref_int32(i__129)
                var t527 int32 = t526 + 1
                ref_set__Ref_int32(i__129, t527)
            case Func:
                var t529 int32 = ref_get__Ref_int32(i__129)
                var t530 int32 = t529 + 1
                ref_set__Ref_int32(i__129, t530)
            case Nil:
                var t532 int32 = ref_get__Ref_int32(i__129)
                var t533 int32 = t532 + 1
                ref_set__Ref_int32(i__129, t533)
            default:
                panic("non-exhaustive match")
            }
        }
        var t513 int32 = ref_get__Ref_int32(acc__130)
        var t514 Value = Value_Int{
            _0: t513,
        }
        jp463 = t514
        return jp463
    case "-":
        var mtmp118 int32 = int32(len(args__121))
        var jp536 Value
        switch mtmp118 {
        case 1:
            var mtmp119 Value = args__121[0]
            var jp538 Value
            switch mtmp119.(type) {
            case Value_Int:
                var x120 int32 = mtmp119.(Value_Int)._0
                var n__132 int32 = x120
                var t539 int32 = 0 - n__132
                var t540 Value = Value_Int{
                    _0: t539,
                }
                jp538 = t540
            case Value_Bool:
                jp538 = Nil{}
            case Func:
                jp538 = Nil{}
            case Nil:
                jp538 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp536 = jp538
            jp463 = jp536
            return jp463
        case 2:
            var t541 Value = args__121[0]
            var t542 Value = args__121[1]
            var mtmp123 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t541,
                _1: t542,
            }
            var x124 Value = mtmp123._0
            var x125 Value = mtmp123._1
            var jp544 Value
            switch x125.(type) {
            case Value_Int:
                var x126 int32 = x125.(Value_Int)._0
                var jp546 Value
                switch x124.(type) {
                case Value_Int:
                    var x129 int32 = x124.(Value_Int)._0
                    var a__133 int32 = x129
                    var b__134 int32 = x126
                    var t547 int32 = a__133 - b__134
                    var t548 Value = Value_Int{
                        _0: t547,
                    }
                    jp546 = t548
                case Value_Bool:
                    jp546 = Nil{}
                case Func:
                    jp546 = Nil{}
                case Nil:
                    jp546 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp544 = jp546
                jp536 = jp544
                jp463 = jp536
                return jp463
            case Value_Bool:
                jp544 = Nil{}
                jp536 = jp544
                jp463 = jp536
                return jp463
            case Func:
                jp544 = Nil{}
                jp536 = jp544
                jp463 = jp536
                return jp463
            case Nil:
                jp544 = Nil{}
                jp536 = jp544
                jp463 = jp536
                return jp463
            default:
                panic("non-exhaustive match")
            }
        default:
            jp536 = Nil{}
            jp463 = jp536
            return jp463
        }
    case "/":
        var t551 int32 = int32(len(args__121))
        var t552 bool = t551 == 2
        var jp550 Value
        if t552 {
            var t553 Value = args__121[0]
            var t554 Value = args__121[1]
            var mtmp132 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t553,
                _1: t554,
            }
            var x133 Value = mtmp132._0
            var x134 Value = mtmp132._1
            var jp556 Value
            switch x134.(type) {
            case Value_Int:
                var x135 int32 = x134.(Value_Int)._0
                var jp558 Value
                switch x133.(type) {
                case Value_Int:
                    var x138 int32 = x133.(Value_Int)._0
                    var a__135 int32 = x138
                    var b__136 int32 = x135
                    var t559 int32 = a__135 / b__136
                    var t560 Value = Value_Int{
                        _0: t559,
                    }
                    jp558 = t560
                case Value_Bool:
                    jp558 = Nil{}
                case Func:
                    jp558 = Nil{}
                case Nil:
                    jp558 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp556 = jp558
                jp550 = jp556
                jp463 = jp550
                return jp463
            case Value_Bool:
                jp556 = Nil{}
                jp550 = jp556
                jp463 = jp550
                return jp463
            case Func:
                jp556 = Nil{}
                jp550 = jp556
                jp463 = jp550
                return jp463
            case Nil:
                jp556 = Nil{}
                jp550 = jp556
                jp463 = jp550
                return jp463
            default:
                panic("non-exhaustive match")
            }
        } else {
            jp550 = Nil{}
            jp463 = jp550
            return jp463
        }
    default:
        jp463 = Nil{}
        return jp463
    }
}

func apply(func__137 Value, args__138 []Value, global__139 *ref_vec_binding_x) Value {
    var jp562 Value
    switch func__137.(type) {
    case Value_Int:
        jp562 = Nil{}
    case Value_Bool:
        jp562 = Nil{}
    case Func:
        var x143 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x143
        var t563 Value = apply_lambda(fun__140, args__138)
        jp562 = t563
    case Nil:
        jp562 = Nil{}
    default:
        panic("non-exhaustive match")
    }
    return jp562
}

func apply_lambda(lambda__141 Lambda, args__142 []Value) Value {
    var t564 []Binding = lambda__141.env
    var env__143 *ref_vec_binding_x = ref__Ref_Vec_Binding(t564)
    var i__144 *ref_int32_x = ref__Ref_int32(0)
    for {
        var t571 int32 = ref_get__Ref_int32(i__144)
        var t572 []string = lambda__141.params
        var t573 int32 = int32(len(t572))
        var t574 bool = t571 < t573
        var t575 int32 = ref_get__Ref_int32(i__144)
        var t576 int32 = int32(len(args__142))
        var t577 bool = t575 < t576
        var t578 bool = t574 && t577
        if !t578 {
            break
        }
        var t579 []string = lambda__141.params
        var t580 int32 = ref_get__Ref_int32(i__144)
        var name__145 string = t579[t580]
        var t581 int32 = ref_get__Ref_int32(i__144)
        var value__146 Value = args__142[t581]
        var t582 []Binding = ref_get__Ref_Vec_Binding(env__143)
        var t583 Binding = Binding{
            name: name__145,
            value: value__146,
        }
        var updated__147 []Binding = append(t582, t583)
        ref_set__Ref_Vec_Binding(env__143, updated__147)
        var t584 int32 = ref_get__Ref_int32(i__144)
        var t585 int32 = t584 + 1
        ref_set__Ref_int32(i__144, t585)
    }
    var t566 SExpr = lambda__141.body
    var t567 []Binding = ref_get__Ref_Vec_Binding(env__143)
    var t568 *ref_vec_binding_x = lambda__141.global
    var t569 Value = eval(t566, t567, t568)
    return t569
}

func main0() struct{} {
    var t587 []Binding = nil
    var global__148 *ref_vec_binding_x = ref__Ref_Vec_Binding(t587)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t588 []Token = lex(program__149)
    var exprs__150 []SExpr = parse_program(t588)
    var t589 SExpr = exprs__150[0]
    var t590 []Binding = nil
    var result__151 Value = eval(t589, t590, global__148)
    var t591 string = value_to_string(result__151)
    string_println(t591)
    var t592 []Token = lex("(add3 10 20 30)")
    var exprs2__152 []SExpr = parse_program(t592)
    var t593 SExpr = exprs2__152[0]
    var t594 []Binding = nil
    var result2__153 Value = eval(t593, t594, global__148)
    var t595 string = value_to_string(result2__153)
    string_println(t595)
    return struct{}{}
}

func main() {
    main0()
}
