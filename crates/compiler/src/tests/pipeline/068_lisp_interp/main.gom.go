package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
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
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5Token_x struct {
    value []Token
}

func ref__Ref_10Vec_5Token(value []Token) *ref_Vec_5Token_x {
    return &ref_Vec_5Token_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Token(reference *ref_Vec_5Token_x) []Token {
    return reference.value
}

func ref_set__Ref_10Vec_5Token(reference *ref_Vec_5Token_x, value []Token) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Value_x struct {
    value Value
}

func ref__Ref_5Value(value Value) *ref_Value_x {
    return &ref_Value_x{
        value: value,
    }
}

func ref_get__Ref_5Value(reference *ref_Value_x) Value {
    return reference.value
}

func ref_set__Ref_5Value(reference *ref_Value_x, value Value) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5SExpr_x struct {
    value []SExpr
}

func ref__Ref_10Vec_5SExpr(value []SExpr) *ref_Vec_5SExpr_x {
    return &ref_Vec_5SExpr_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x) []SExpr {
    return reference.value
}

func ref_set__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x, value []SExpr) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_7Binding_x struct {
    value []Binding
}

func ref__Ref_12Vec_7Binding(value []Binding) *ref_Vec_7Binding_x {
    return &ref_Vec_7Binding_x{
        value: value,
    }
}

func ref_get__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x) []Binding {
    return reference.value
}

func ref_set__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x, value []Binding) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_6string_x struct {
    value []string
}

func ref__Ref_11Vec_6string(value []string) *ref_Vec_6string_x {
    return &ref_Vec_6string_x{
        value: value,
    }
}

func ref_get__Ref_11Vec_6string(reference *ref_Vec_6string_x) []string {
    return reference.value
}

func ref_set__Ref_11Vec_6string(reference *ref_Vec_6string_x, value []string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5Value_x struct {
    value []Value
}

func ref__Ref_10Vec_5Value(value []Value) *ref_Vec_5Value_x {
    return &ref_Vec_5Value_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Value(reference *ref_Vec_5Value_x) []Value {
    return reference.value
}

func ref_set__Ref_10Vec_5Value(reference *ref_Vec_5Value_x, value []Value) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_5Token_5int32 struct {
    _0 Token
    _1 int32
}

type Tuple2_10Vec_5SExpr_5int32 struct {
    _0 []SExpr
    _1 int32
}

type Tuple2_5SExpr_5int32 struct {
    _0 SExpr
    _1 int32
}

type Tuple2_5Value_5Value struct {
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
    global *ref_Vec_7Binding_x
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
    var retv149 bool
    var t152 bool = ch__0 >= 48
    var jp151 bool
    if t152 {
        var t153 bool = ch__0 <= 57
        jp151 = t153
    } else {
        jp151 = false
    }
    retv149 = jp151
    return retv149
}

func digit_value(ch__1 rune) int32 {
    var retv155 int32
    var jp157 int32
    switch ch__1 {
    case 48:
        jp157 = 0
    case 49:
        jp157 = 1
    case 50:
        jp157 = 2
    case 51:
        jp157 = 3
    case 52:
        jp157 = 4
    case 53:
        jp157 = 5
    case 54:
        jp157 = 6
    case 55:
        jp157 = 7
    case 56:
        jp157 = 8
    case 57:
        jp157 = 9
    default:
        jp157 = 0
    }
    retv155 = jp157
    return retv155
}

func is_int_text(text__2 string) bool {
    var retv159 bool
    var len__3 int32 = string_len(text__2)
    var t162 bool = len__3 == 0
    var jp161 bool
    if t162 {
        jp161 = false
        retv159 = jp161
        return retv159
    } else {
        var i__4 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
        var saw_digit__5 *ref_bool_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool(false)
        var ok__6 *ref_bool_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool(true)
        var started__7 *ref_bool_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool(false)
        Loop_loop168:
        for {
            var t187 bool = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(ok__6)
            var jp170 bool
            if t187 {
                var t188 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__4)
                var t189 bool = t188 < len__3
                jp170 = t189
            } else {
                jp170 = false
            }
            if jp170 {
                var t171 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__4)
                var ch__8 rune = string_get(text__2, t171)
                var t184 bool = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(started__7)
                var t185 bool = !t184
                var jp174 bool
                if t185 {
                    var t186 bool = ch__8 == 45
                    jp174 = t186
                } else {
                    jp174 = false
                }
                if jp174 {
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(started__7, true)
                    var t175 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__4)
                    var t176 int32 = t175 + 1
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__4, t176)
                } else {
                    var t179 bool = is_digit(ch__8)
                    if t179 {
                        _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(started__7, true)
                        _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(saw_digit__5, true)
                        var t180 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__4)
                        var t181 int32 = t180 + 1
                        _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__4, t181)
                    } else {
                        _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop168
            }
        }
        var t166 bool = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(ok__6)
        var jp165 bool
        if t166 {
            var t167 bool = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(saw_digit__5)
            jp165 = t167
        } else {
            jp165 = false
        }
        jp161 = jp165
        retv159 = jp161
        return retv159
    }
}

func parse_int32(text__9 string) int32 {
    var retv191 int32
    var len__10 int32 = string_len(text__9)
    var i__11 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
    var negative__12 *ref_bool_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool(false)
    var started__13 *ref_bool_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool(false)
    var acc__14 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
    Loop_loop199:
    for {
        var t200 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__11)
        var t201 bool = t200 < len__10
        if t201 {
            var t202 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__11)
            var ch__15 rune = string_get(text__9, t202)
            var t215 bool = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(started__13)
            var t216 bool = !t215
            var jp205 bool
            if t216 {
                var t217 bool = ch__15 == 45
                jp205 = t217
            } else {
                jp205 = false
            }
            if jp205 {
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(started__13, true)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(negative__12, true)
                var t206 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__11)
                var t207 int32 = t206 + 1
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__11, t207)
            } else {
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t209 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(acc__14)
                var t210 int32 = t209 * 10
                var t211 int32 = t210 + d__16
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(acc__14, t211)
                var t212 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__11)
                var t213 int32 = t212 + 1
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__11, t213)
            }
            continue
        } else {
            break Loop_loop199
        }
    }
    var t195 bool = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(negative__12)
    var jp194 int32
    if t195 {
        var t196 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(acc__14)
        var t197 int32 = 0 - t196
        jp194 = t197
    } else {
        var t198 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(acc__14)
        jp194 = t198
    }
    retv191 = jp194
    return retv191
}

func is_delim(ch__17 rune) bool {
    var retv219 bool
    var t225 bool = ch__17 == 40
    var jp223 bool
    if t225 {
        jp223 = true
    } else {
        var t226 bool = ch__17 == 41
        jp223 = t226
    }
    var jp221 bool
    if jp223 {
        jp221 = true
    } else {
        var t224 bool = ch__17 == 32
        jp221 = t224
    }
    retv219 = jp221
    return retv219
}

func lex_atom(source__18 string, start__19 int32) Tuple2_5Token_5int32 {
    var retv228 Tuple2_5Token_5int32
    var len__20 int32 = string_len(source__18)
    var text__21 *ref_string_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_string("")
    var i__22 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(start__19)
    var done__23 *ref_bool_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool(false)
    Loop_loop242:
    for {
        var t255 bool = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(done__23)
        var t256 bool = !t255
        var jp244 bool
        if t256 {
            var t257 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__22)
            var t258 bool = t257 < len__20
            jp244 = t258
        } else {
            jp244 = false
        }
        if jp244 {
            var t245 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__22)
            var ch__24 rune = string_get(source__18, t245)
            var t247 bool = is_delim(ch__24)
            if t247 {
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(done__23, true)
            } else {
                var t249 string = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_string(text__21)
                var t250 string = char_to_string(ch__24)
                var t251 string = t249 + t250
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_string(text__21, t251)
                var t252 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__22)
                var t253 int32 = t252 + 1
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__22, t253)
            }
            continue
        } else {
            break Loop_loop242
        }
    }
    var atom__25 string = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_string(text__21)
    var jp231 Token
    switch atom__25 {
    case "true":
        var t234 Token = Token_Bool{
            _0: true,
        }
        jp231 = t234
    case "false":
        var t235 Token = Token_Bool{
            _0: false,
        }
        jp231 = t235
    default:
        var t238 bool = is_int_text(atom__25)
        var jp237 Token
        if t238 {
            var t239 int32 = parse_int32(atom__25)
            var t240 Token = Token_Int{
                _0: t239,
            }
            jp237 = t240
        } else {
            var t241 Token = Token_Sym{
                _0: atom__25,
            }
            jp237 = t241
        }
        jp231 = jp237
    }
    var token__26 Token = jp231
    var t232 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__22)
    var t233 Tuple2_5Token_5int32 = Tuple2_5Token_5int32{
        _0: token__26,
        _1: t232,
    }
    retv228 = t233
    return retv228
}

func lex(source__27 string) []Token {
    var retv260 []Token
    var len__28 int32 = string_len(source__27)
    var toks0__29 []Token = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Token()
    var toks__30 *ref_Vec_5Token_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_Token_x5d_(toks0__29)
    var i__31 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
    Loop_loop263:
    for {
        var t264 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__31)
        var t265 bool = t264 < len__28
        if t265 {
            var t266 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__31)
            var ch__32 rune = string_get(source__27, t266)
            var t268 bool = ch__32 == 40
            if t268 {
                var t269 []Token = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Token_x5d_(toks__30)
                var t270 []Token = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_Token(t269, LParen{})
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_Token_x5d_(toks__30, t270)
                var t271 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__31)
                var t272 int32 = t271 + 1
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__31, t272)
            } else {
                var t275 bool = ch__32 == 41
                if t275 {
                    var t276 []Token = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Token_x5d_(toks__30)
                    var t277 []Token = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_Token(t276, RParen{})
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_Token_x5d_(toks__30, t277)
                    var t278 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__31)
                    var t279 int32 = t278 + 1
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__31, t279)
                } else {
                    var t282 bool = ch__32 == 32
                    if t282 {
                        var t283 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__31)
                        var t284 int32 = t283 + 1
                        _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__31, t284)
                    } else {
                        var t286 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__31)
                        var mtmp13 Tuple2_5Token_5int32 = lex_atom(source__27, t286)
                        var x14 Token = mtmp13._0
                        var x15 int32 = mtmp13._1
                        var next__34 int32 = x15
                        var tok__33 Token = x14
                        var t287 []Token = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Token_x5d_(toks__30)
                        var t288 []Token = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_Token(t287, tok__33)
                        _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_Token_x5d_(toks__30, t288)
                        _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop263
        }
    }
    var t262 []Token = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Token_x5d_(toks__30)
    retv260 = t262
    return retv260
}

func env_lookup(env__35 []Binding, name__36 string) Value {
    var retv291 Value
    var t292 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Binding(env__35)
    var t293 int32 = t292 - 1
    var i__37 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(t293)
    var result__38 *ref_Value_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Value(Nil{})
    var done__39 *ref_bool_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool(false)
    Loop_loop296:
    for {
        var t308 bool = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(done__39)
        var t309 bool = !t308
        var jp298 bool
        if t309 {
            var t310 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__37)
            var t311 bool = t310 >= 0
            jp298 = t311
        } else {
            jp298 = false
        }
        if jp298 {
            var t299 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__37)
            var binding__40 Binding = env__35[t299]
            var t301 string = binding__40.name
            var t302 bool = t301 == name__36
            if t302 {
                var t303 Value = binding__40.value
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Value(result__38, t303)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(done__39, true)
            } else {
                var t305 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__37)
                var t306 int32 = t305 - 1
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__37, t306)
            }
            continue
        } else {
            break Loop_loop296
        }
    }
    var t295 Value = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Value(result__38)
    retv291 = t295
    return retv291
}

func lookup(local__41 []Binding, global__42 []Binding, name__43 string) Value {
    var retv313 Value
    var mtmp20 Value = env_lookup(local__41, name__43)
    var jp315 Value
    switch mtmp20.(type) {
    case Value_Int:
        var other__44 Value = mtmp20
        jp315 = other__44
    case Value_Bool:
        var other__44 Value = mtmp20
        jp315 = other__44
    case Func:
        var other__44 Value = mtmp20
        jp315 = other__44
    case Nil:
        var t316 Value = env_lookup(global__42, name__43)
        jp315 = t316
    default:
        panic("non-exhaustive match")
    }
    retv313 = jp315
    return retv313
}

func parse_list(tokens__45 []Token, start__46 int32) Tuple2_10Vec_5SExpr_5int32 {
    var retv318 Tuple2_10Vec_5SExpr_5int32
    var acc__47 []SExpr = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(acc__47)
    var i__49 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(start__46)
    var done__50 *ref_bool_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool(false)
    Loop_loop323:
    for {
        var t347 bool = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(done__50)
        var t348 bool = !t347
        var jp325 bool
        if t348 {
            var t349 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__49)
            var t350 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Token(tokens__45)
            var t351 bool = t349 < t350
            jp325 = t351
        } else {
            jp325 = false
        }
        if jp325 {
            var t326 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__49)
            var mtmp24 Token = tokens__45[t326]
            switch mtmp24.(type) {
            case LParen:
                var t328 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__49)
                var mtmp28 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t328)
                var x29 SExpr = mtmp28._0
                var x30 int32 = mtmp28._1
                var next__52 int32 = x30
                var expr__51 SExpr = x29
                var t329 []SExpr = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__48)
                var t330 []SExpr = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_SExpr(t329, expr__51)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__48, t330)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__49, next__52)
            case RParen:
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(done__50, true)
                var t332 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__49)
                var t333 int32 = t332 + 1
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__49, t333)
            case Token_Sym:
                var t335 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__49)
                var mtmp33 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t335)
                var x34 SExpr = mtmp33._0
                var x35 int32 = mtmp33._1
                var next__52 int32 = x35
                var expr__51 SExpr = x34
                var t336 []SExpr = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__48)
                var t337 []SExpr = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_SExpr(t336, expr__51)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__48, t337)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__49, next__52)
            case Token_Int:
                var t339 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__49)
                var mtmp37 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t339)
                var x38 SExpr = mtmp37._0
                var x39 int32 = mtmp37._1
                var next__52 int32 = x39
                var expr__51 SExpr = x38
                var t340 []SExpr = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__48)
                var t341 []SExpr = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_SExpr(t340, expr__51)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__48, t341)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__49, next__52)
            case Token_Bool:
                var t343 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__49)
                var mtmp41 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t343)
                var x42 SExpr = mtmp41._0
                var x43 int32 = mtmp41._1
                var next__52 int32 = x43
                var expr__51 SExpr = x42
                var t344 []SExpr = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__48)
                var t345 []SExpr = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_SExpr(t344, expr__51)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__48, t345)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__49, next__52)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop323
        }
    }
    var t320 []SExpr = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__48)
    var t321 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__49)
    var t322 Tuple2_10Vec_5SExpr_5int32 = Tuple2_10Vec_5SExpr_5int32{
        _0: t320,
        _1: t321,
    }
    retv318 = t322
    return retv318
}

func parse_expr(tokens__53 []Token, start__54 int32) Tuple2_5SExpr_5int32 {
    var retv353 Tuple2_5SExpr_5int32
    var mtmp46 Token = tokens__53[start__54]
    var jp355 Tuple2_5SExpr_5int32
    switch mtmp46.(type) {
    case LParen:
        var t356 int32 = start__54 + 1
        var mtmp50 Tuple2_10Vec_5SExpr_5int32 = parse_list(tokens__53, t356)
        var x51 []SExpr = mtmp50._0
        var x52 int32 = mtmp50._1
        var next__56 int32 = x52
        var items__55 []SExpr = x51
        var t357 SExpr = List{
            _0: items__55,
        }
        var t358 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t357,
            _1: next__56,
        }
        jp355 = t358
    case RParen:
        var t359 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t360 int32 = start__54 + 1
        var t361 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t359,
            _1: t360,
        }
        jp355 = t361
    case Token_Sym:
        var x47 string = mtmp46.(Token_Sym)._0
        var name__59 string = x47
        var t362 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t363 int32 = start__54 + 1
        var t364 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t362,
            _1: t363,
        }
        jp355 = t364
    case Token_Int:
        var x48 int32 = mtmp46.(Token_Int)._0
        var n__58 int32 = x48
        var t365 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t366 int32 = start__54 + 1
        var t367 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t365,
            _1: t366,
        }
        jp355 = t367
    case Token_Bool:
        var x49 bool = mtmp46.(Token_Bool)._0
        var b__57 bool = x49
        var t368 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t369 int32 = start__54 + 1
        var t370 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t368,
            _1: t369,
        }
        jp355 = t370
    default:
        panic("non-exhaustive match")
    }
    retv353 = jp355
    return retv353
}

func parse_program(tokens__60 []Token) []SExpr {
    var retv372 []SExpr
    var i__61 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
    var acc__62 []SExpr = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(acc__62)
    Loop_loop375:
    for {
        var t376 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__61)
        var t377 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Token(tokens__60)
        var t378 bool = t376 < t377
        if t378 {
            var t379 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__61)
            var mtmp53 Tuple2_5SExpr_5int32 = parse_expr(tokens__60, t379)
            var x54 SExpr = mtmp53._0
            var x55 int32 = mtmp53._1
            var next__65 int32 = x55
            var expr__64 SExpr = x54
            var t380 []SExpr = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__63)
            var t381 []SExpr = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_SExpr(t380, expr__64)
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__63, t381)
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__61, next__65)
            continue
        } else {
            break Loop_loop375
        }
    }
    var t374 []SExpr = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(exprs__63)
    retv372 = t374
    return retv372
}

func value_to_string(value__66 Value) string {
    var retv384 string
    var jp386 string
    switch value__66.(type) {
    case Value_Int:
        var x58 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x58
        var t387 string = int32_to_string(n__67)
        jp386 = t387
    case Value_Bool:
        var x59 bool = value__66.(Value_Bool)._0
        var b__68 bool = x59
        var t388 string = bool_to_string(b__68)
        jp386 = t388
    case Func:
        jp386 = "<lambda>"
    case Nil:
        jp386 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv384 = jp386
    return retv384
}

func truthy(value__69 Value) bool {
    var retv390 bool
    var jp392 bool
    switch value__69.(type) {
    case Value_Int:
        var x61 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x61
        var t393 bool = n__71 != 0
        jp392 = t393
    case Value_Bool:
        var x62 bool = value__69.(Value_Bool)._0
        var b__70 bool = x62
        jp392 = b__70
    case Func:
        jp392 = true
    case Nil:
        jp392 = false
    default:
        panic("non-exhaustive match")
    }
    retv390 = jp392
    return retv390
}

func eval(expr__72 SExpr, local__73 []Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv395 Value
    var jp397 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x64 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x64
        var t398 Value = Value_Int{
            _0: n__75,
        }
        jp397 = t398
    case SExpr_Bool:
        var x65 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x65
        var t399 Value = Value_Bool{
            _0: b__76,
        }
        jp397 = t399
    case SExpr_Sym:
        var x66 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x66
        var t400 []Binding = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Binding_x5d_(global__74)
        var t401 Value = lookup(local__73, t400, name__77)
        jp397 = t401
    case List:
        var x67 []SExpr = expr__72.(List)._0
        var items__78 []SExpr = x67
        var t402 Value = eval_list(items__78, local__73, global__74)
        jp397 = t402
    default:
        panic("non-exhaustive match")
    }
    retv395 = jp397
    return retv395
}

func eval_list(items__79 []SExpr, local__80 []Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv404 Value
    var t407 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_SExpr(items__79)
    var t408 bool = t407 == 0
    var jp406 Value
    if t408 {
        jp406 = Nil{}
    } else {
        var head__82 SExpr = items__79[0]
        var jp410 Value
        switch head__82.(type) {
        case SExpr_Int:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            var t411 Value = apply(f__84, args__85, global__81)
            jp410 = t411
        case SExpr_Bool:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            var t412 Value = apply(f__84, args__85, global__81)
            jp410 = t412
        case SExpr_Sym:
            var x70 string = head__82.(SExpr_Sym)._0
            var name__83 string = x70
            var t413 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp410 = t413
        case List:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            var t414 Value = apply(f__84, args__85, global__81)
            jp410 = t414
        default:
            panic("non-exhaustive match")
        }
        jp406 = jp410
    }
    retv404 = jp406
    return retv404
}

func eval_list_sym(name__86 string, items__87 []SExpr, local__88 []Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv416 Value
    var jp418 Value
    switch name__86 {
    case "begin":
        var t419 Value = eval_begin(items__87, 1, local__88, global__89)
        jp418 = t419
    case "define":
        var t422 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_SExpr(items__87)
        var t423 bool = t422 == 3
        var jp421 Value
        if t423 {
            var mtmp72 SExpr = items__87[1]
            var jp425 Value
            switch mtmp72.(type) {
            case SExpr_Int:
                jp425 = Nil{}
            case SExpr_Bool:
                jp425 = Nil{}
            case SExpr_Sym:
                var x75 string = mtmp72.(SExpr_Sym)._0
                var var__90 string = x75
                var t426 SExpr = items__87[2]
                var value__91 Value = eval(t426, local__88, global__89)
                var env__92 []Binding = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Binding_x5d_(global__89)
                var t427 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 []Binding = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_Binding(env__92, t427)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_Binding_x5d_(global__89, updated__93)
                jp425 = value__91
            case List:
                jp425 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp421 = jp425
        } else {
            jp421 = Nil{}
        }
        jp418 = jp421
    case "if":
        var t430 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_SExpr(items__87)
        var t431 bool = t430 == 4
        var jp429 Value
        if t431 {
            var t432 SExpr = items__87[1]
            var cond__94 Value = eval(t432, local__88, global__89)
            var t435 bool = truthy(cond__94)
            var jp434 Value
            if t435 {
                var t436 SExpr = items__87[2]
                var t437 Value = eval(t436, local__88, global__89)
                jp434 = t437
            } else {
                var t438 SExpr = items__87[3]
                var t439 Value = eval(t438, local__88, global__89)
                jp434 = t439
            }
            jp429 = jp434
        } else {
            jp429 = Nil{}
        }
        jp418 = jp429
    case "lambda":
        var t442 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_SExpr(items__87)
        var t443 bool = t442 == 3
        var jp441 Value
        if t443 {
            var mtmp78 SExpr = items__87[1]
            var jp445 Value
            switch mtmp78.(type) {
            case SExpr_Int:
                jp445 = Nil{}
            case SExpr_Bool:
                jp445 = Nil{}
            case SExpr_Sym:
                jp445 = Nil{}
            case List:
                var x82 []SExpr = mtmp78.(List)._0
                var params_exprs__95 []SExpr = x82
                var params__96 []string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = items__87[2]
                var t446 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t447 Value = Func{
                    _0: t446,
                }
                jp445 = t447
            default:
                panic("non-exhaustive match")
            }
            jp441 = jp445
        } else {
            jp441 = Nil{}
        }
        jp418 = jp441
    case "+":
        var t448 []Value = eval_args(items__87, 1, local__88, global__89)
        var t449 Value = apply_builtin("+", t448)
        jp418 = t449
    case "-":
        var t450 []Value = eval_args(items__87, 1, local__88, global__89)
        var t451 Value = apply_builtin("-", t450)
        jp418 = t451
    case "*":
        var t452 []Value = eval_args(items__87, 1, local__88, global__89)
        var t453 Value = apply_builtin("*", t452)
        jp418 = t453
    case "/":
        var t454 []Value = eval_args(items__87, 1, local__88, global__89)
        var t455 Value = apply_builtin("/", t454)
        jp418 = t455
    case "=":
        var t456 []Value = eval_args(items__87, 1, local__88, global__89)
        var t457 Value = apply_builtin("=", t456)
        jp418 = t457
    default:
        var t458 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t458, local__88, global__89)
        var args__99 []Value = eval_args(items__87, 1, local__88, global__89)
        var t459 Value = apply(f__98, args__99, global__89)
        jp418 = t459
    }
    retv416 = jp418
    return retv416
}

func eval_begin(items__100 []SExpr, start__101 int32, local__102 []Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv461 Value
    var i__104 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(start__101)
    var last__105 *ref_Value_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Value(Nil{})
    Loop_loop464:
    for {
        var t465 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__104)
        var t466 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_SExpr(items__100)
        var t467 bool = t465 < t466
        if t467 {
            var t468 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__104)
            var t469 SExpr = items__100[t468]
            var v__106 Value = eval(t469, local__102, global__103)
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Value(last__105, v__106)
            var t470 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__104)
            var t471 int32 = t470 + 1
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__104, t471)
            continue
        } else {
            break Loop_loop464
        }
    }
    var t463 Value = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Value(last__105)
    retv461 = t463
    return retv461
}

func params_from_sexprs(items__107 []SExpr) []string {
    var retv474 []string
    var i__108 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
    var acc__109 []string = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_string()
    var params__110 *ref_Vec_6string_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_string_x5d_(acc__109)
    Loop_loop477:
    for {
        var t478 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__108)
        var t479 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_SExpr(items__107)
        var t480 bool = t478 < t479
        if t480 {
            var t481 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__108)
            var mtmp85 SExpr = items__107[t481]
            switch mtmp85.(type) {
            case SExpr_Int:
                var t483 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__108)
                var t484 int32 = t483 + 1
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__108, t484)
            case SExpr_Bool:
                var t486 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__108)
                var t487 int32 = t486 + 1
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__108, t487)
            case SExpr_Sym:
                var x88 string = mtmp85.(SExpr_Sym)._0
                var name__111 string = x88
                var t489 []string = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_string_x5d_(params__110)
                var t490 []string = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_string(t489, name__111)
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_string_x5d_(params__110, t490)
                var t491 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__108)
                var t492 int32 = t491 + 1
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__108, t492)
            case List:
                var t494 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__108)
                var t495 int32 = t494 + 1
                _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__108, t495)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop477
        }
    }
    var t476 []string = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_string_x5d_(params__110)
    retv474 = t476
    return retv474
}

func eval_args(items__112 []SExpr, start__113 int32, local__114 []Binding, global__115 *ref_Vec_7Binding_x) []Value {
    var retv498 []Value
    var i__116 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(start__113)
    var acc__117 []Value = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Value()
    var args__118 *ref_Vec_5Value_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_Value_x5d_(acc__117)
    Loop_loop501:
    for {
        var t502 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__116)
        var t503 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_SExpr(items__112)
        var t504 bool = t502 < t503
        if t504 {
            var t505 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__116)
            var t506 SExpr = items__112[t505]
            var v__119 Value = eval(t506, local__114, global__115)
            var t507 []Value = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Value_x5d_(args__118)
            var t508 []Value = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_Value(t507, v__119)
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_Value_x5d_(args__118, t508)
            var t509 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__116)
            var t510 int32 = t509 + 1
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__116, t510)
            continue
        } else {
            break Loop_loop501
        }
    }
    var t500 []Value = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Value_x5d_(args__118)
    retv498 = t500
    return retv498
}

func apply_builtin(name__120 string, args__121 []Value) Value {
    var retv513 Value
    var jp515 Value
    switch name__120 {
    case "=":
        var t518 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Value(args__121)
        var t519 bool = t518 == 2
        var jp517 Value
        if t519 {
            var t520 Value = args__121[0]
            var t521 Value = args__121[1]
            var mtmp94 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t520,
                _1: t521,
            }
            var x95 Value = mtmp94._0
            var x96 Value = mtmp94._1
            var jp523 Value
            switch x96.(type) {
            case Value_Int:
                var x97 int32 = x96.(Value_Int)._0
                var jp525 Value
                switch x95.(type) {
                case Value_Int:
                    var x100 int32 = x95.(Value_Int)._0
                    var a__122 int32 = x100
                    var b__123 int32 = x97
                    var t526 bool = a__122 == b__123
                    var t527 Value = Value_Bool{
                        _0: t526,
                    }
                    jp525 = t527
                case Value_Bool:
                    var t528 Value = Value_Bool{
                        _0: false,
                    }
                    jp525 = t528
                case Func:
                    var t529 Value = Value_Bool{
                        _0: false,
                    }
                    jp525 = t529
                case Nil:
                    var t530 Value = Value_Bool{
                        _0: false,
                    }
                    jp525 = t530
                default:
                    panic("non-exhaustive match")
                }
                jp523 = jp525
            case Value_Bool:
                var x98 bool = x96.(Value_Bool)._0
                var jp532 Value
                switch x95.(type) {
                case Value_Int:
                    var t533 Value = Value_Bool{
                        _0: false,
                    }
                    jp532 = t533
                case Value_Bool:
                    var x104 bool = x95.(Value_Bool)._0
                    var a__124 bool = x104
                    var b__125 bool = x98
                    var t534 bool = a__124 == b__125
                    var t535 Value = Value_Bool{
                        _0: t534,
                    }
                    jp532 = t535
                case Func:
                    var t536 Value = Value_Bool{
                        _0: false,
                    }
                    jp532 = t536
                case Nil:
                    var t537 Value = Value_Bool{
                        _0: false,
                    }
                    jp532 = t537
                default:
                    panic("non-exhaustive match")
                }
                jp523 = jp532
            case Func:
                var t538 Value = Value_Bool{
                    _0: false,
                }
                jp523 = t538
            case Nil:
                var t539 Value = Value_Bool{
                    _0: false,
                }
                jp523 = t539
            default:
                panic("non-exhaustive match")
            }
            jp517 = jp523
        } else {
            var t540 Value = Value_Bool{
                _0: false,
            }
            jp517 = t540
        }
        jp515 = jp517
        retv513 = jp515
        return retv513
    case "+":
        var i__126 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
        var acc__127 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
        Loop_loop544:
        for {
            var t545 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__126)
            var t546 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Value(args__121)
            var t547 bool = t545 < t546
            if t547 {
                var t548 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__126)
                var mtmp106 Value = args__121[t548]
                switch mtmp106.(type) {
                case Value_Int:
                    var x107 int32 = mtmp106.(Value_Int)._0
                    var n__128 int32 = x107
                    var t550 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(acc__127)
                    var t551 int32 = t550 + n__128
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(acc__127, t551)
                    var t552 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__126)
                    var t553 int32 = t552 + 1
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__126, t553)
                case Value_Bool:
                    var t555 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__126)
                    var t556 int32 = t555 + 1
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__126, t556)
                case Func:
                    var t558 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__126)
                    var t559 int32 = t558 + 1
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__126, t559)
                case Nil:
                    var t561 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__126)
                    var t562 int32 = t561 + 1
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__126, t562)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop544
            }
        }
        var t542 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(acc__127)
        var t543 Value = Value_Int{
            _0: t542,
        }
        jp515 = t543
        retv513 = jp515
        return retv513
    case "*":
        var i__129 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
        var acc__130 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(1)
        Loop_loop567:
        for {
            var t568 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__129)
            var t569 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Value(args__121)
            var t570 bool = t568 < t569
            if t570 {
                var t571 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__129)
                var mtmp112 Value = args__121[t571]
                switch mtmp112.(type) {
                case Value_Int:
                    var x113 int32 = mtmp112.(Value_Int)._0
                    var n__131 int32 = x113
                    var t573 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(acc__130)
                    var t574 int32 = t573 * n__131
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(acc__130, t574)
                    var t575 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__129)
                    var t576 int32 = t575 + 1
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__129, t576)
                case Value_Bool:
                    var t578 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__129)
                    var t579 int32 = t578 + 1
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__129, t579)
                case Func:
                    var t581 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__129)
                    var t582 int32 = t581 + 1
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__129, t582)
                case Nil:
                    var t584 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__129)
                    var t585 int32 = t584 + 1
                    _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__129, t585)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop567
            }
        }
        var t565 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(acc__130)
        var t566 Value = Value_Int{
            _0: t565,
        }
        jp515 = t566
        retv513 = jp515
        return retv513
    case "-":
        var mtmp118 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Value(args__121)
        var jp588 Value
        switch mtmp118 {
        case 1:
            var mtmp119 Value = args__121[0]
            var jp590 Value
            switch mtmp119.(type) {
            case Value_Int:
                var x120 int32 = mtmp119.(Value_Int)._0
                var n__132 int32 = x120
                var t591 int32 = 0 - n__132
                var t592 Value = Value_Int{
                    _0: t591,
                }
                jp590 = t592
            case Value_Bool:
                jp590 = Nil{}
            case Func:
                jp590 = Nil{}
            case Nil:
                jp590 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp588 = jp590
        case 2:
            var t593 Value = args__121[0]
            var t594 Value = args__121[1]
            var mtmp123 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t593,
                _1: t594,
            }
            var x124 Value = mtmp123._0
            var x125 Value = mtmp123._1
            var jp596 Value
            switch x125.(type) {
            case Value_Int:
                var x126 int32 = x125.(Value_Int)._0
                var jp598 Value
                switch x124.(type) {
                case Value_Int:
                    var x129 int32 = x124.(Value_Int)._0
                    var a__133 int32 = x129
                    var b__134 int32 = x126
                    var t599 int32 = a__133 - b__134
                    var t600 Value = Value_Int{
                        _0: t599,
                    }
                    jp598 = t600
                case Value_Bool:
                    jp598 = Nil{}
                case Func:
                    jp598 = Nil{}
                case Nil:
                    jp598 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp596 = jp598
            case Value_Bool:
                jp596 = Nil{}
            case Func:
                jp596 = Nil{}
            case Nil:
                jp596 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp588 = jp596
        default:
            jp588 = Nil{}
        }
        jp515 = jp588
        retv513 = jp515
        return retv513
    case "/":
        var t603 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Value(args__121)
        var t604 bool = t603 == 2
        var jp602 Value
        if t604 {
            var t605 Value = args__121[0]
            var t606 Value = args__121[1]
            var mtmp132 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t605,
                _1: t606,
            }
            var x133 Value = mtmp132._0
            var x134 Value = mtmp132._1
            var jp608 Value
            switch x134.(type) {
            case Value_Int:
                var x135 int32 = x134.(Value_Int)._0
                var jp610 Value
                switch x133.(type) {
                case Value_Int:
                    var x138 int32 = x133.(Value_Int)._0
                    var a__135 int32 = x138
                    var b__136 int32 = x135
                    var t611 int32 = a__135 / b__136
                    var t612 Value = Value_Int{
                        _0: t611,
                    }
                    jp610 = t612
                case Value_Bool:
                    jp610 = Nil{}
                case Func:
                    jp610 = Nil{}
                case Nil:
                    jp610 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp608 = jp610
            case Value_Bool:
                jp608 = Nil{}
            case Func:
                jp608 = Nil{}
            case Nil:
                jp608 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp602 = jp608
        } else {
            jp602 = Nil{}
        }
        jp515 = jp602
        retv513 = jp515
        return retv513
    default:
        jp515 = Nil{}
        retv513 = jp515
        return retv513
    }
}

func apply(func__137 Value, args__138 []Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv614 Value
    var jp616 Value
    switch func__137.(type) {
    case Value_Int:
        jp616 = Nil{}
    case Value_Bool:
        jp616 = Nil{}
    case Func:
        var x143 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x143
        var t617 Value = apply_lambda(fun__140, args__138)
        jp616 = t617
    case Nil:
        jp616 = Nil{}
    default:
        panic("non-exhaustive match")
    }
    retv614 = jp616
    return retv614
}

func apply_lambda(lambda__141 Lambda, args__142 []Value) Value {
    var retv619 Value
    var t620 []Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_Binding_x5d_(t620)
    var i__144 *ref_int32_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(0)
    Loop_loop626:
    for {
        var t637 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__144)
        var t638 []string = lambda__141.params
        var t639 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(t638)
        var t640 bool = t637 < t639
        var jp628 bool
        if t640 {
            var t641 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__144)
            var t642 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Value(args__142)
            var t643 bool = t641 < t642
            jp628 = t643
        } else {
            jp628 = false
        }
        if jp628 {
            var t629 []string = lambda__141.params
            var t630 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__144)
            var name__145 string = t629[t630]
            var t631 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__144)
            var value__146 Value = args__142[t631]
            var t632 []Binding = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Binding_x5d_(env__143)
            var t633 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 []Binding = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_Binding(t632, t633)
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_Binding_x5d_(env__143, updated__147)
            var t634 int32 = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(i__144)
            var t635 int32 = t634 + 1
            _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(i__144, t635)
            continue
        } else {
            break Loop_loop626
        }
    }
    var t622 SExpr = lambda__141.body
    var t623 []Binding = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Binding_x5d_(env__143)
    var t624 *ref_Vec_7Binding_x = lambda__141.global
    var t625 Value = eval(t622, t623, t624)
    retv619 = t625
    return retv619
}

func main0() struct{} {
    var t645 []Binding = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_Binding_x5d_(t645)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t646 []Token = lex(program__149)
    var exprs__150 []SExpr = parse_program(t646)
    var t647 SExpr = exprs__150[0]
    var t648 []Binding = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Binding()
    var result__151 Value = eval(t647, t648, global__148)
    var t649 string = value_to_string(result__151)
    println__T_string(t649)
    var t650 []Token = lex("(add3 10 20 30)")
    var exprs2__152 []SExpr = parse_program(t650)
    var t651 SExpr = exprs2__152[0]
    var t652 []Binding = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Binding()
    var result2__153 Value = eval(t651, t652, global__148)
    var t653 string = value_to_string(result2__153)
    println__T_string(t653)
    return struct{}{}
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_int32(value__93 int32) *ref_int32_x {
    var retv655 *ref_int32_x
    var t656 *ref_int32_x = ref__Ref_5int32(value__93)
    retv655 = t656
    return retv655
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_bool(value__93 bool) *ref_bool_x {
    var retv658 *ref_bool_x
    var t659 *ref_bool_x = ref__Ref_4bool(value__93)
    retv658 = t659
    return retv658
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_bool(self__94 *ref_bool_x) bool {
    var retv661 bool
    var t662 bool = ref_get__Ref_4bool(self__94)
    retv661 = t662
    return retv661
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_int32(self__94 *ref_int32_x) int32 {
    var retv664 int32
    var t665 int32 = ref_get__Ref_5int32(self__94)
    retv664 = t665
    return retv664
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_bool(self__95 *ref_bool_x, value__96 bool) struct{} {
    ref_set__Ref_4bool(self__95, value__96)
    return struct{}{}
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_int32(self__95 *ref_int32_x, value__96 int32) struct{} {
    ref_set__Ref_5int32(self__95, value__96)
    return struct{}{}
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_string(value__93 string) *ref_string_x {
    var retv671 *ref_string_x
    var t672 *ref_string_x = ref__Ref_6string(value__93)
    retv671 = t672
    return retv671
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_string(self__94 *ref_string_x) string {
    var retv674 string
    var t675 string = ref_get__Ref_6string(self__94)
    retv674 = t675
    return retv674
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_string(self__95 *ref_string_x, value__96 string) struct{} {
    ref_set__Ref_6string(self__95, value__96)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Token() []Token {
    var retv679 []Token
    var t680 []Token = nil
    retv679 = t680
    return retv679
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_Token_x5d_(value__93 []Token) *ref_Vec_5Token_x {
    var retv682 *ref_Vec_5Token_x
    var t683 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__93)
    retv682 = t683
    return retv682
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Token_x5d_(self__94 *ref_Vec_5Token_x) []Token {
    var retv685 []Token
    var t686 []Token = ref_get__Ref_10Vec_5Token(self__94)
    retv685 = t686
    return retv685
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_Token(self__66 []Token, elem__67 Token) []Token {
    var retv688 []Token
    var t689 []Token = append(self__66, elem__67)
    retv688 = t689
    return retv688
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_Token_x5d_(self__95 *ref_Vec_5Token_x, value__96 []Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__95, value__96)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Binding(self__73 []Binding) int32 {
    var retv693 int32
    var t694 int32 = int32(len(self__73))
    retv693 = t694
    return retv693
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Value(value__93 Value) *ref_Value_x {
    var retv696 *ref_Value_x
    var t697 *ref_Value_x = ref__Ref_5Value(value__93)
    retv696 = t697
    return retv696
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Value(self__95 *ref_Value_x, value__96 Value) struct{} {
    ref_set__Ref_5Value(self__95, value__96)
    return struct{}{}
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Value(self__94 *ref_Value_x) Value {
    var retv701 Value
    var t702 Value = ref_get__Ref_5Value(self__94)
    retv701 = t702
    return retv701
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_SExpr() []SExpr {
    var retv704 []SExpr
    var t705 []SExpr = nil
    retv704 = t705
    return retv704
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(value__93 []SExpr) *ref_Vec_5SExpr_x {
    var retv707 *ref_Vec_5SExpr_x
    var t708 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__93)
    retv707 = t708
    return retv707
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Token(self__73 []Token) int32 {
    var retv710 int32
    var t711 int32 = int32(len(self__73))
    retv710 = t711
    return retv710
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(self__94 *ref_Vec_5SExpr_x) []SExpr {
    var retv713 []SExpr
    var t714 []SExpr = ref_get__Ref_10Vec_5SExpr(self__94)
    retv713 = t714
    return retv713
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_SExpr(self__66 []SExpr, elem__67 SExpr) []SExpr {
    var retv716 []SExpr
    var t717 []SExpr = append(self__66, elem__67)
    retv716 = t717
    return retv716
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_SExpr_x5d_(self__95 *ref_Vec_5SExpr_x, value__96 []SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__95, value__96)
    return struct{}{}
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Binding_x5d_(self__94 *ref_Vec_7Binding_x) []Binding {
    var retv721 []Binding
    var t722 []Binding = ref_get__Ref_12Vec_7Binding(self__94)
    retv721 = t722
    return retv721
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_SExpr(self__73 []SExpr) int32 {
    var retv724 int32
    var t725 int32 = int32(len(self__73))
    retv724 = t725
    return retv724
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_Binding(self__66 []Binding, elem__67 Binding) []Binding {
    var retv727 []Binding
    var t728 []Binding = append(self__66, elem__67)
    retv727 = t728
    return retv727
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_Binding_x5d_(self__95 *ref_Vec_7Binding_x, value__96 []Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__95, value__96)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_string() []string {
    var retv732 []string
    var t733 []string = nil
    retv732 = t733
    return retv732
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_string_x5d_(value__93 []string) *ref_Vec_6string_x {
    var retv735 *ref_Vec_6string_x
    var t736 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__93)
    retv735 = t736
    return retv735
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_string_x5d_(self__94 *ref_Vec_6string_x) []string {
    var retv738 []string
    var t739 []string = ref_get__Ref_11Vec_6string(self__94)
    retv738 = t739
    return retv738
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_string(self__66 []string, elem__67 string) []string {
    var retv741 []string
    var t742 []string = append(self__66, elem__67)
    retv741 = t742
    return retv741
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_string_x5d_(self__95 *ref_Vec_6string_x, value__96 []string) struct{} {
    ref_set__Ref_11Vec_6string(self__95, value__96)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Value() []Value {
    var retv746 []Value
    var t747 []Value = nil
    retv746 = t747
    return retv746
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_Value_x5d_(value__93 []Value) *ref_Vec_5Value_x {
    var retv749 *ref_Vec_5Value_x
    var t750 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__93)
    retv749 = t750
    return retv749
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_get_x5f__x5f_T_x5f_Vec_x5b_Value_x5d_(self__94 *ref_Vec_5Value_x) []Value {
    var retv752 []Value
    var t753 []Value = ref_get__Ref_10Vec_5Value(self__94)
    retv752 = t753
    return retv752
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_Value(self__66 []Value, elem__67 Value) []Value {
    var retv755 []Value
    var t756 []Value = append(self__66, elem__67)
    retv755 = t756
    return retv755
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_set_x5f__x5f_T_x5f_Vec_x5b_Value_x5d_(self__95 *ref_Vec_5Value_x, value__96 []Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__95, value__96)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_Value(self__73 []Value) int32 {
    var retv760 int32
    var t761 int32 = int32(len(self__73))
    retv760 = t761
    return retv760
}

func _goml_inherent_x23_Ref_x23_Ref_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Vec_x5b_Binding_x5d_(value__93 []Binding) *ref_Vec_7Binding_x {
    var retv763 *ref_Vec_7Binding_x
    var t764 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__93)
    retv763 = t764
    return retv763
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(self__73 []string) int32 {
    var retv766 int32
    var t767 int32 = int32(len(self__73))
    retv766 = t767
    return retv766
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_Binding() []Binding {
    var retv769 []Binding
    var t770 []Binding = nil
    retv769 = t770
    return retv769
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
