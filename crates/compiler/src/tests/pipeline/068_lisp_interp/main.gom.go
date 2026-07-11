package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_len(s string) int32 {
    return int32(len(s))
}

func _goml_runtime_core_string_get(s string, i int32) rune {
    return rune(s[i])
}

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_Token struct {
    items []Token
}

func vec_new__Vec_5Token() *_goml_vec_Token {
    return &_goml_vec_Token{
        items: nil,
    }
}

func vec_push__Vec_5Token(vec *_goml_vec_Token, elem Token) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5Token(vec *_goml_vec_Token, index int32) Token {
    return vec.items[index]
}

func vec_len__Vec_5Token(vec *_goml_vec_Token) int32 {
    return int32(len(vec.items))
}

type _goml_vec_Binding struct {
    items []Binding
}

func vec_new__Vec_7Binding() *_goml_vec_Binding {
    return &_goml_vec_Binding{
        items: nil,
    }
}

func vec_push__Vec_7Binding(vec *_goml_vec_Binding, elem Binding) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_7Binding(vec *_goml_vec_Binding, index int32) Binding {
    return vec.items[index]
}

func vec_len__Vec_7Binding(vec *_goml_vec_Binding) int32 {
    return int32(len(vec.items))
}

type _goml_vec_SExpr struct {
    items []SExpr
}

func vec_new__Vec_5SExpr() *_goml_vec_SExpr {
    return &_goml_vec_SExpr{
        items: nil,
    }
}

func vec_push__Vec_5SExpr(vec *_goml_vec_SExpr, elem SExpr) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5SExpr(vec *_goml_vec_SExpr, index int32) SExpr {
    return vec.items[index]
}

func vec_len__Vec_5SExpr(vec *_goml_vec_SExpr) int32 {
    return int32(len(vec.items))
}

type _goml_vec_Value struct {
    items []Value
}

func vec_new__Vec_5Value() *_goml_vec_Value {
    return &_goml_vec_Value{
        items: nil,
    }
}

func vec_push__Vec_5Value(vec *_goml_vec_Value, elem Value) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5Value(vec *_goml_vec_Value, index int32) Value {
    return vec.items[index]
}

func vec_len__Vec_5Value(vec *_goml_vec_Value) int32 {
    return int32(len(vec.items))
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int32) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int32 {
    return int32(len(vec.items))
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
    value *_goml_vec_Token
}

func ref__Ref_10Vec_5Token(value *_goml_vec_Token) *ref_Vec_5Token_x {
    return &ref_Vec_5Token_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Token(reference *ref_Vec_5Token_x) *_goml_vec_Token {
    return reference.value
}

func ref_set__Ref_10Vec_5Token(reference *ref_Vec_5Token_x, value *_goml_vec_Token) struct{} {
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
    value *_goml_vec_SExpr
}

func ref__Ref_10Vec_5SExpr(value *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    return &ref_Vec_5SExpr_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    return reference.value
}

func ref_set__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x, value *_goml_vec_SExpr) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_7Binding_x struct {
    value *_goml_vec_Binding
}

func ref__Ref_12Vec_7Binding(value *_goml_vec_Binding) *ref_Vec_7Binding_x {
    return &ref_Vec_7Binding_x{
        value: value,
    }
}

func ref_get__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x) *_goml_vec_Binding {
    return reference.value
}

func ref_set__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x, value *_goml_vec_Binding) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_6string_x struct {
    value *_goml_vec_string
}

func ref__Ref_11Vec_6string(value *_goml_vec_string) *ref_Vec_6string_x {
    return &ref_Vec_6string_x{
        value: value,
    }
}

func ref_get__Ref_11Vec_6string(reference *ref_Vec_6string_x) *_goml_vec_string {
    return reference.value
}

func ref_set__Ref_11Vec_6string(reference *ref_Vec_6string_x, value *_goml_vec_string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5Value_x struct {
    value *_goml_vec_Value
}

func ref__Ref_10Vec_5Value(value *_goml_vec_Value) *ref_Vec_5Value_x {
    return &ref_Vec_5Value_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Value(reference *ref_Vec_5Value_x) *_goml_vec_Value {
    return reference.value
}

func ref_set__Ref_10Vec_5Value(reference *ref_Vec_5Value_x, value *_goml_vec_Value) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_5Token_5int32 struct {
    _0 Token
    _1 int32
}

type Tuple2_10Vec_5SExpr_5int32 struct {
    _0 *_goml_vec_SExpr
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
    params *_goml_vec_string
    body SExpr
    env *_goml_vec_Binding
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
    _0 *_goml_vec_SExpr
}

func (_ List) isSExpr() {}

func is_digit(ch__0 rune) bool {
    var retv156 bool
    var t159 bool = ch__0 >= 48
    var jp158 bool
    if t159 {
        var t160 bool = ch__0 <= 57
        jp158 = t160
    } else {
        jp158 = false
    }
    retv156 = jp158
    return retv156
}

func digit_value(ch__1 rune) int32 {
    var retv162 int32
    var jp164 int32
    switch ch__1 {
    case 48:
        jp164 = 0
    case 49:
        jp164 = 1
    case 50:
        jp164 = 2
    case 51:
        jp164 = 3
    case 52:
        jp164 = 4
    case 53:
        jp164 = 5
    case 54:
        jp164 = 6
    case 55:
        jp164 = 7
    case 56:
        jp164 = 8
    case 57:
        jp164 = 9
    default:
        jp164 = 0
    }
    retv162 = jp164
    return retv162
}

func is_int_text(text__2 string) bool {
    var retv166 bool
    var len__3 int32 = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t169 bool = len__3 == 0
    var jp168 bool
    if t169 {
        jp168 = false
        retv166 = jp168
        return retv166
    } else {
        var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var saw_digit__5 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        var ok__6 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
        var started__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        Loop_loop175:
        for {
            var t194 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
            var jp177 bool
            if t194 {
                var t195 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t196 bool = t195 < len__3
                jp177 = t196
            } else {
                jp177 = false
            }
            if jp177 {
                var t178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var ch__8 rune = _goml_m_inherent_i_string_i_string_i_get(text__2, t178)
                var t191 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__7)
                var t192 bool = !t191
                var jp181 bool
                if t192 {
                    var t193 bool = ch__8 == 45
                    jp181 = t193
                } else {
                    jp181 = false
                }
                if jp181 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                    var t182 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                    var t183 int32 = t182 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t183)
                } else {
                    var t186 bool = is_digit(ch__8)
                    if t186 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(saw_digit__5, true)
                        var t187 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                        var t188 int32 = t187 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t188)
                    } else {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop175
            }
        }
        var t173 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
        var jp172 bool
        if t173 {
            var t174 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(saw_digit__5)
            jp172 = t174
        } else {
            jp172 = false
        }
        jp168 = jp172
        retv166 = jp168
        return retv166
    }
}

func parse_int32(text__9 string) int32 {
    var retv198 int32
    var len__10 int32 = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop206:
    for {
        var t207 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
        var t208 bool = t207 < len__10
        if t208 {
            var t209 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
            var ch__15 rune = _goml_m_inherent_i_string_i_string_i_get(text__9, t209)
            var t222 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__13)
            var t223 bool = !t222
            var jp212 bool
            if t223 {
                var t224 bool = ch__15 == 45
                jp212 = t224
            } else {
                jp212 = false
            }
            if jp212 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(negative__12, true)
                var t213 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t214 int32 = t213 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t214)
            } else {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t216 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
                var t217 int32 = t216 * 10
                var t218 int32 = t217 + d__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__14, t218)
                var t219 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t220 int32 = t219 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t220)
            }
            continue
        } else {
            break Loop_loop206
        }
    }
    var t202 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(negative__12)
    var jp201 int32
    if t202 {
        var t203 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        var t204 int32 = 0 - t203
        jp201 = t204
    } else {
        var t205 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        jp201 = t205
    }
    retv198 = jp201
    return retv198
}

func is_delim(ch__17 rune) bool {
    var retv226 bool
    var t232 bool = ch__17 == 40
    var jp230 bool
    if t232 {
        jp230 = true
    } else {
        var t233 bool = ch__17 == 41
        jp230 = t233
    }
    var jp228 bool
    if jp230 {
        jp228 = true
    } else {
        var t231 bool = ch__17 == 32
        jp228 = t231
    }
    retv226 = jp228
    return retv226
}

func lex_atom(source__18 string, start__19 int32) Tuple2_5Token_5int32 {
    var retv235 Tuple2_5Token_5int32
    var len__20 int32 = _goml_m_inherent_i_string_i_string_i_len(source__18)
    var text__21 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var i__22 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__19)
    var done__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop249:
    for {
        var t262 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__23)
        var t263 bool = !t262
        var jp251 bool
        if t263 {
            var t264 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var t265 bool = t264 < len__20
            jp251 = t265
        } else {
            jp251 = false
        }
        if jp251 {
            var t252 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var ch__24 rune = _goml_m_inherent_i_string_i_string_i_get(source__18, t252)
            var t254 bool = is_delim(ch__24)
            if t254 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__23, true)
            } else {
                var t256 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
                var t257 string = _goml_m_inherent_i_char_i_char_i_to__string(ch__24)
                var t258 string = t256 + t257
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(text__21, t258)
                var t259 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
                var t260 int32 = t259 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__22, t260)
            }
            continue
        } else {
            break Loop_loop249
        }
    }
    var atom__25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
    var jp238 Token
    switch atom__25 {
    case "true":
        var t241 Token = Token_Bool{
            _0: true,
        }
        jp238 = t241
    case "false":
        var t242 Token = Token_Bool{
            _0: false,
        }
        jp238 = t242
    default:
        var t245 bool = is_int_text(atom__25)
        var jp244 Token
        if t245 {
            var t246 int32 = parse_int32(atom__25)
            var t247 Token = Token_Int{
                _0: t246,
            }
            jp244 = t247
        } else {
            var t248 Token = Token_Sym{
                _0: atom__25,
            }
            jp244 = t248
        }
        jp238 = jp244
    }
    var token__26 Token = jp238
    var t239 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
    var t240 Tuple2_5Token_5int32 = Tuple2_5Token_5int32{
        _0: token__26,
        _1: t239,
    }
    retv235 = t240
    return retv235
}

func lex(source__27 string) *_goml_vec_Token {
    var retv267 *_goml_vec_Token
    var len__28 int32 = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var toks__30 *ref_Vec_5Token_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(toks0__29)
    var i__31 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop270:
    for {
        var t271 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
        var t272 bool = t271 < len__28
        if t272 {
            var t273 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
            var ch__32 rune = _goml_m_inherent_i_string_i_string_i_get(source__27, t273)
            var t275 bool = ch__32 == 40
            if t275 {
                var t276 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t277 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t276, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t277)
                var t278 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                var t279 int32 = t278 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t279)
            } else {
                var t282 bool = ch__32 == 41
                if t282 {
                    var t283 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t284 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t283, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t284)
                    var t285 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                    var t286 int32 = t285 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t286)
                } else {
                    var t289 bool = ch__32 == 32
                    if t289 {
                        var t290 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var t291 int32 = t290 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t291)
                    } else {
                        var t293 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var mtmp20 Tuple2_5Token_5int32 = lex_atom(source__27, t293)
                        var x21 Token = mtmp20._0
                        var x22 int32 = mtmp20._1
                        var next__34 int32 = x22
                        var tok__33 Token = x21
                        var t294 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                        var t295 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t294, tok__33)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t295)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop270
        }
    }
    var t269 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
    retv267 = t269
    return retv267
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var retv298 Value
    var t299 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(env__35)
    var t300 int32 = t299 - 1
    var i__37 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t300)
    var result__38 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    var done__39 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop303:
    for {
        var t315 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__39)
        var t316 bool = !t315
        var jp305 bool
        if t316 {
            var t317 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var t318 bool = t317 >= 0
            jp305 = t318
        } else {
            jp305 = false
        }
        if jp305 {
            var t306 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t306)
            var t308 string = binding__40.name
            var t309 bool = t308 == name__36
            if t309 {
                var t310 Value = binding__40.value
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(result__38, t310)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__39, true)
            } else {
                var t312 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
                var t313 int32 = t312 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__37, t313)
            }
            continue
        } else {
            break Loop_loop303
        }
    }
    var t302 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(result__38)
    retv298 = t302
    return retv298
}

func lookup(local__41 *_goml_vec_Binding, global__42 *_goml_vec_Binding, name__43 string) Value {
    var retv320 Value
    var mtmp27 Value = env_lookup(local__41, name__43)
    var jp322 Value
    switch mtmp27.(type) {
    case Value_Int:
        var other__44 Value = mtmp27
        jp322 = other__44
    case Value_Bool:
        var other__44 Value = mtmp27
        jp322 = other__44
    case Func:
        var other__44 Value = mtmp27
        jp322 = other__44
    case Nil:
        var t323 Value = env_lookup(global__42, name__43)
        jp322 = t323
    default:
        panic("non-exhaustive match")
    }
    retv320 = jp322
    return retv320
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int32) Tuple2_10Vec_5SExpr_5int32 {
    var retv325 Tuple2_10Vec_5SExpr_5int32
    var acc__47 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__47)
    var i__49 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__46)
    var done__50 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop330:
    for {
        var t354 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__50)
        var t355 bool = !t354
        var jp332 bool
        if t355 {
            var t356 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var t357 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__45)
            var t358 bool = t356 < t357
            jp332 = t358
        } else {
            jp332 = false
        }
        if jp332 {
            var t333 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var mtmp31 Token = vec_get__Vec_5Token(tokens__45, t333)
            switch mtmp31.(type) {
            case LParen:
                var t335 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp35 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t335)
                var x36 SExpr = mtmp35._0
                var x37 int32 = mtmp35._1
                var next__52 int32 = x37
                var expr__51 SExpr = x36
                var t336 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t337 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t336, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t337)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case RParen:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__50, true)
                var t339 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var t340 int32 = t339 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, t340)
            case Token_Sym:
                var t342 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp40 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t342)
                var x41 SExpr = mtmp40._0
                var x42 int32 = mtmp40._1
                var next__52 int32 = x42
                var expr__51 SExpr = x41
                var t343 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t344 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t343, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t344)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case Token_Int:
                var t346 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp44 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t346)
                var x45 SExpr = mtmp44._0
                var x46 int32 = mtmp44._1
                var next__52 int32 = x46
                var expr__51 SExpr = x45
                var t347 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t348 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t347, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t348)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case Token_Bool:
                var t350 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp48 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t350)
                var x49 SExpr = mtmp48._0
                var x50 int32 = mtmp48._1
                var next__52 int32 = x50
                var expr__51 SExpr = x49
                var t351 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t352 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t351, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t352)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop330
        }
    }
    var t327 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
    var t328 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
    var t329 Tuple2_10Vec_5SExpr_5int32 = Tuple2_10Vec_5SExpr_5int32{
        _0: t327,
        _1: t328,
    }
    retv325 = t329
    return retv325
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int32) Tuple2_5SExpr_5int32 {
    var retv360 Tuple2_5SExpr_5int32
    var mtmp53 Token = vec_get__Vec_5Token(tokens__53, start__54)
    var jp362 Tuple2_5SExpr_5int32
    switch mtmp53.(type) {
    case LParen:
        var t363 int32 = start__54 + 1
        var mtmp57 Tuple2_10Vec_5SExpr_5int32 = parse_list(tokens__53, t363)
        var x58 *_goml_vec_SExpr = mtmp57._0
        var x59 int32 = mtmp57._1
        var next__56 int32 = x59
        var items__55 *_goml_vec_SExpr = x58
        var t364 SExpr = List{
            _0: items__55,
        }
        var t365 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t364,
            _1: next__56,
        }
        jp362 = t365
    case RParen:
        var t366 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t367 int32 = start__54 + 1
        var t368 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t366,
            _1: t367,
        }
        jp362 = t368
    case Token_Sym:
        var x54 string = mtmp53.(Token_Sym)._0
        var name__59 string = x54
        var t369 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t370 int32 = start__54 + 1
        var t371 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t369,
            _1: t370,
        }
        jp362 = t371
    case Token_Int:
        var x55 int32 = mtmp53.(Token_Int)._0
        var n__58 int32 = x55
        var t372 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t373 int32 = start__54 + 1
        var t374 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t372,
            _1: t373,
        }
        jp362 = t374
    case Token_Bool:
        var x56 bool = mtmp53.(Token_Bool)._0
        var b__57 bool = x56
        var t375 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t376 int32 = start__54 + 1
        var t377 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t375,
            _1: t376,
        }
        jp362 = t377
    default:
        panic("non-exhaustive match")
    }
    retv360 = jp362
    return retv360
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var retv379 *_goml_vec_SExpr
    var i__61 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__62 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__62)
    Loop_loop382:
    for {
        var t383 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
        var t384 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__60)
        var t385 bool = t383 < t384
        if t385 {
            var t386 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
            var mtmp60 Tuple2_5SExpr_5int32 = parse_expr(tokens__60, t386)
            var x61 SExpr = mtmp60._0
            var x62 int32 = mtmp60._1
            var next__65 int32 = x62
            var expr__64 SExpr = x61
            var t387 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
            var t388 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t387, expr__64)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__63, t388)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__61, next__65)
            continue
        } else {
            break Loop_loop382
        }
    }
    var t381 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
    retv379 = t381
    return retv379
}

func value_to_string(value__66 Value) string {
    var retv391 string
    var jp393 string
    switch value__66.(type) {
    case Value_Int:
        var x65 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x65
        var t394 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__67)
        jp393 = t394
    case Value_Bool:
        var x66 bool = value__66.(Value_Bool)._0
        var b__68 bool = x66
        var t395 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(b__68)
        jp393 = t395
    case Func:
        jp393 = "<lambda>"
    case Nil:
        jp393 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv391 = jp393
    return retv391
}

func truthy(value__69 Value) bool {
    var retv397 bool
    var jp399 bool
    switch value__69.(type) {
    case Value_Int:
        var x68 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x68
        var t400 bool = n__71 != 0
        jp399 = t400
    case Value_Bool:
        var x69 bool = value__69.(Value_Bool)._0
        var b__70 bool = x69
        jp399 = b__70
    case Func:
        jp399 = true
    case Nil:
        jp399 = false
    default:
        panic("non-exhaustive match")
    }
    retv397 = jp399
    return retv397
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv402 Value
    var jp404 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x71 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x71
        var t405 Value = Value_Int{
            _0: n__75,
        }
        jp404 = t405
    case SExpr_Bool:
        var x72 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x72
        var t406 Value = Value_Bool{
            _0: b__76,
        }
        jp404 = t406
    case SExpr_Sym:
        var x73 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x73
        var t407 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t408 Value = lookup(local__73, t407, name__77)
        jp404 = t408
    case List:
        var x74 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x74
        var t409 Value = eval_list(items__78, local__73, global__74)
        jp404 = t409
    default:
        panic("non-exhaustive match")
    }
    retv402 = jp404
    return retv402
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv411 Value
    var t414 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t415 bool = t414 == 0
    var jp413 Value
    if t415 {
        jp413 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp417 Value
        switch head__82.(type) {
        case SExpr_Int:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t418 Value = apply(f__84, args__85, global__81)
            jp417 = t418
        case SExpr_Bool:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t419 Value = apply(f__84, args__85, global__81)
            jp417 = t419
        case SExpr_Sym:
            var x77 string = head__82.(SExpr_Sym)._0
            var name__83 string = x77
            var t420 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp417 = t420
        case List:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t421 Value = apply(f__84, args__85, global__81)
            jp417 = t421
        default:
            panic("non-exhaustive match")
        }
        jp413 = jp417
    }
    retv411 = jp413
    return retv411
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv423 Value
    var jp425 Value
    switch name__86 {
    case "begin":
        var t426 Value = eval_begin(items__87, 1, local__88, global__89)
        jp425 = t426
    case "define":
        var t429 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t430 bool = t429 == 3
        var jp428 Value
        if t430 {
            var mtmp79 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp432 Value
            switch mtmp79.(type) {
            case SExpr_Int:
                jp432 = Nil{}
            case SExpr_Bool:
                jp432 = Nil{}
            case SExpr_Sym:
                var x82 string = mtmp79.(SExpr_Sym)._0
                var var__90 string = x82
                var t433 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t433, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t434 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t434)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp432 = value__91
            case List:
                jp432 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp428 = jp432
        } else {
            jp428 = Nil{}
        }
        jp425 = jp428
    case "if":
        var t437 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t438 bool = t437 == 4
        var jp436 Value
        if t438 {
            var t439 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t439, local__88, global__89)
            var t442 bool = truthy(cond__94)
            var jp441 Value
            if t442 {
                var t443 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t444 Value = eval(t443, local__88, global__89)
                jp441 = t444
            } else {
                var t445 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t446 Value = eval(t445, local__88, global__89)
                jp441 = t446
            }
            jp436 = jp441
        } else {
            jp436 = Nil{}
        }
        jp425 = jp436
    case "lambda":
        var t449 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t450 bool = t449 == 3
        var jp448 Value
        if t450 {
            var mtmp85 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp452 Value
            switch mtmp85.(type) {
            case SExpr_Int:
                jp452 = Nil{}
            case SExpr_Bool:
                jp452 = Nil{}
            case SExpr_Sym:
                jp452 = Nil{}
            case List:
                var x89 *_goml_vec_SExpr = mtmp85.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x89
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t453 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t454 Value = Func{
                    _0: t453,
                }
                jp452 = t454
            default:
                panic("non-exhaustive match")
            }
            jp448 = jp452
        } else {
            jp448 = Nil{}
        }
        jp425 = jp448
    case "+":
        var t455 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t456 Value = apply_builtin("+", t455)
        jp425 = t456
    case "-":
        var t457 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t458 Value = apply_builtin("-", t457)
        jp425 = t458
    case "*":
        var t459 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t460 Value = apply_builtin("*", t459)
        jp425 = t460
    case "/":
        var t461 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t462 Value = apply_builtin("/", t461)
        jp425 = t462
    case "=":
        var t463 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t464 Value = apply_builtin("=", t463)
        jp425 = t464
    default:
        var t465 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t465, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t466 Value = apply(f__98, args__99, global__89)
        jp425 = t466
    }
    retv423 = jp425
    return retv423
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int32, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv468 Value
    var i__104 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop471:
    for {
        var t472 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
        var t473 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t474 bool = t472 < t473
        if t474 {
            var t475 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t476 SExpr = vec_get__Vec_5SExpr(items__100, t475)
            var v__106 Value = eval(t476, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t477 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t478 int32 = t477 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__104, t478)
            continue
        } else {
            break Loop_loop471
        }
    }
    var t470 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv468 = t470
    return retv468
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv481 *_goml_vec_string
    var i__108 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop484:
    for {
        var t485 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
        var t486 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t487 bool = t485 < t486
        if t487 {
            var t488 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
            var mtmp92 SExpr = vec_get__Vec_5SExpr(items__107, t488)
            switch mtmp92.(type) {
            case SExpr_Int:
                var t490 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t491 int32 = t490 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t491)
            case SExpr_Bool:
                var t493 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t494 int32 = t493 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t494)
            case SExpr_Sym:
                var x95 string = mtmp92.(SExpr_Sym)._0
                var name__111 string = x95
                var t496 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t497 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t496, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t497)
                var t498 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t499 int32 = t498 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t499)
            case List:
                var t501 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t502 int32 = t501 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t502)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop484
        }
    }
    var t483 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv481 = t483
    return retv481
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int32, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv505 *_goml_vec_Value
    var i__116 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop508:
    for {
        var t509 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
        var t510 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t511 bool = t509 < t510
        if t511 {
            var t512 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t513 SExpr = vec_get__Vec_5SExpr(items__112, t512)
            var v__119 Value = eval(t513, local__114, global__115)
            var t514 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t515 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t514, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t515)
            var t516 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t517 int32 = t516 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__116, t517)
            continue
        } else {
            break Loop_loop508
        }
    }
    var t507 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv505 = t507
    return retv505
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv520 Value
    var jp522 Value
    switch name__120 {
    case "=":
        var t525 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t526 bool = t525 == 2
        var jp524 Value
        if t526 {
            var t527 Value = vec_get__Vec_5Value(args__121, 0)
            var t528 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp101 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t527,
                _1: t528,
            }
            var x102 Value = mtmp101._0
            var x103 Value = mtmp101._1
            var jp530 Value
            switch x103.(type) {
            case Value_Int:
                var x104 int32 = x103.(Value_Int)._0
                var jp532 Value
                switch x102.(type) {
                case Value_Int:
                    var x107 int32 = x102.(Value_Int)._0
                    var a__122 int32 = x107
                    var b__123 int32 = x104
                    var t533 bool = a__122 == b__123
                    var t534 Value = Value_Bool{
                        _0: t533,
                    }
                    jp532 = t534
                case Value_Bool:
                    var t535 Value = Value_Bool{
                        _0: false,
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
                jp530 = jp532
            case Value_Bool:
                var x105 bool = x103.(Value_Bool)._0
                var jp539 Value
                switch x102.(type) {
                case Value_Int:
                    var t540 Value = Value_Bool{
                        _0: false,
                    }
                    jp539 = t540
                case Value_Bool:
                    var x111 bool = x102.(Value_Bool)._0
                    var a__124 bool = x111
                    var b__125 bool = x105
                    var t541 bool = a__124 == b__125
                    var t542 Value = Value_Bool{
                        _0: t541,
                    }
                    jp539 = t542
                case Func:
                    var t543 Value = Value_Bool{
                        _0: false,
                    }
                    jp539 = t543
                case Nil:
                    var t544 Value = Value_Bool{
                        _0: false,
                    }
                    jp539 = t544
                default:
                    panic("non-exhaustive match")
                }
                jp530 = jp539
            case Func:
                var t545 Value = Value_Bool{
                    _0: false,
                }
                jp530 = t545
            case Nil:
                var t546 Value = Value_Bool{
                    _0: false,
                }
                jp530 = t546
            default:
                panic("non-exhaustive match")
            }
            jp524 = jp530
        } else {
            var t547 Value = Value_Bool{
                _0: false,
            }
            jp524 = t547
        }
        jp522 = jp524
        retv520 = jp522
        return retv520
    case "+":
        var i__126 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop551:
        for {
            var t552 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
            var t553 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t554 bool = t552 < t553
            if t554 {
                var t555 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                var mtmp113 Value = vec_get__Vec_5Value(args__121, t555)
                switch mtmp113.(type) {
                case Value_Int:
                    var x114 int32 = mtmp113.(Value_Int)._0
                    var n__128 int32 = x114
                    var t557 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t558 int32 = t557 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t558)
                    var t559 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t560 int32 = t559 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t560)
                case Value_Bool:
                    var t562 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t563 int32 = t562 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t563)
                case Func:
                    var t565 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t566 int32 = t565 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t566)
                case Nil:
                    var t568 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t569 int32 = t568 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t569)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop551
            }
        }
        var t549 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t550 Value = Value_Int{
            _0: t549,
        }
        jp522 = t550
        retv520 = jp522
        return retv520
    case "*":
        var i__129 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop574:
        for {
            var t575 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
            var t576 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t577 bool = t575 < t576
            if t577 {
                var t578 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                var mtmp119 Value = vec_get__Vec_5Value(args__121, t578)
                switch mtmp119.(type) {
                case Value_Int:
                    var x120 int32 = mtmp119.(Value_Int)._0
                    var n__131 int32 = x120
                    var t580 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t581 int32 = t580 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t581)
                    var t582 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t583 int32 = t582 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t583)
                case Value_Bool:
                    var t585 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t586 int32 = t585 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t586)
                case Func:
                    var t588 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t589 int32 = t588 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t589)
                case Nil:
                    var t591 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t592 int32 = t591 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t592)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop574
            }
        }
        var t572 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t573 Value = Value_Int{
            _0: t572,
        }
        jp522 = t573
        retv520 = jp522
        return retv520
    case "-":
        var mtmp125 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp595 Value
        switch mtmp125 {
        case 1:
            var mtmp126 Value = vec_get__Vec_5Value(args__121, 0)
            var jp597 Value
            switch mtmp126.(type) {
            case Value_Int:
                var x127 int32 = mtmp126.(Value_Int)._0
                var n__132 int32 = x127
                var t598 int32 = 0 - n__132
                var t599 Value = Value_Int{
                    _0: t598,
                }
                jp597 = t599
            case Value_Bool:
                jp597 = Nil{}
            case Func:
                jp597 = Nil{}
            case Nil:
                jp597 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp595 = jp597
        case 2:
            var t600 Value = vec_get__Vec_5Value(args__121, 0)
            var t601 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp130 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t600,
                _1: t601,
            }
            var x131 Value = mtmp130._0
            var x132 Value = mtmp130._1
            var jp603 Value
            switch x132.(type) {
            case Value_Int:
                var x133 int32 = x132.(Value_Int)._0
                var jp605 Value
                switch x131.(type) {
                case Value_Int:
                    var x136 int32 = x131.(Value_Int)._0
                    var a__133 int32 = x136
                    var b__134 int32 = x133
                    var t606 int32 = a__133 - b__134
                    var t607 Value = Value_Int{
                        _0: t606,
                    }
                    jp605 = t607
                case Value_Bool:
                    jp605 = Nil{}
                case Func:
                    jp605 = Nil{}
                case Nil:
                    jp605 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp603 = jp605
            case Value_Bool:
                jp603 = Nil{}
            case Func:
                jp603 = Nil{}
            case Nil:
                jp603 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp595 = jp603
        default:
            jp595 = Nil{}
        }
        jp522 = jp595
        retv520 = jp522
        return retv520
    case "/":
        var t610 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t611 bool = t610 == 2
        var jp609 Value
        if t611 {
            var t612 Value = vec_get__Vec_5Value(args__121, 0)
            var t613 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp139 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t612,
                _1: t613,
            }
            var x140 Value = mtmp139._0
            var x141 Value = mtmp139._1
            var jp615 Value
            switch x141.(type) {
            case Value_Int:
                var x142 int32 = x141.(Value_Int)._0
                var jp617 Value
                switch x140.(type) {
                case Value_Int:
                    var x145 int32 = x140.(Value_Int)._0
                    var a__135 int32 = x145
                    var b__136 int32 = x142
                    var t618 int32 = a__135 / b__136
                    var t619 Value = Value_Int{
                        _0: t618,
                    }
                    jp617 = t619
                case Value_Bool:
                    jp617 = Nil{}
                case Func:
                    jp617 = Nil{}
                case Nil:
                    jp617 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp615 = jp617
            case Value_Bool:
                jp615 = Nil{}
            case Func:
                jp615 = Nil{}
            case Nil:
                jp615 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp609 = jp615
        } else {
            jp609 = Nil{}
        }
        jp522 = jp609
        retv520 = jp522
        return retv520
    default:
        jp522 = Nil{}
        retv520 = jp522
        return retv520
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv621 Value
    var jp623 Value
    switch func__137.(type) {
    case Value_Int:
        jp623 = Nil{}
    case Value_Bool:
        jp623 = Nil{}
    case Func:
        var x150 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x150
        var t624 Value = apply_lambda(fun__140, args__138)
        jp623 = t624
    case Nil:
        jp623 = Nil{}
    default:
        panic("non-exhaustive match")
    }
    retv621 = jp623
    return retv621
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv626 Value
    var t627 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t627)
    var i__144 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop633:
    for {
        var t644 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
        var t645 *_goml_vec_string = lambda__141.params
        var t646 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t645)
        var t647 bool = t644 < t646
        var jp635 bool
        if t647 {
            var t648 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t649 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t650 bool = t648 < t649
            jp635 = t650
        } else {
            jp635 = false
        }
        if jp635 {
            var t636 *_goml_vec_string = lambda__141.params
            var t637 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var name__145 string = vec_get__Vec_6string(t636, t637)
            var t638 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t638)
            var t639 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t640 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t639, t640)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t641 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t642 int32 = t641 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__144, t642)
            continue
        } else {
            break Loop_loop633
        }
    }
    var t629 SExpr = lambda__141.body
    var t630 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t631 *ref_Vec_7Binding_x = lambda__141.global
    var t632 Value = eval(t629, t630, t631)
    retv626 = t632
    return retv626
}

func main0() struct{} {
    var t652 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t652)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t653 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t653)
    var t654 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t655 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t654, t655, global__148)
    var t656 string = value_to_string(result__151)
    println__T_string(t656)
    var t657 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t657)
    var t658 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t659 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t658, t659, global__148)
    var t660 string = value_to_string(result2__153)
    println__T_string(t660)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__4 string) int32 {
    var retv662 int32
    var t663 int32 = _goml_runtime_core_string_len(self__4)
    retv662 = t663
    return retv662
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv665 *ref_int32_x
    var t666 *ref_int32_x = ref__Ref_5int32(value__114)
    retv665 = t666
    return retv665
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__114 bool) *ref_bool_x {
    var retv668 *ref_bool_x
    var t669 *ref_bool_x = ref__Ref_4bool(value__114)
    retv668 = t669
    return retv668
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__115 *ref_bool_x) bool {
    var retv671 bool
    var t672 bool = ref_get__Ref_4bool(self__115)
    retv671 = t672
    return retv671
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv674 int32
    var t675 int32 = ref_get__Ref_5int32(self__115)
    retv674 = t675
    return retv674
}

func _goml_m_inherent_i_string_i_string_i_get(self__5 string, index__6 int32) rune {
    var retv677 rune
    var t678 rune = _goml_runtime_core_string_get(self__5, index__6)
    retv677 = t678
    return retv677
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__116 *ref_bool_x, value__117 bool) struct{} {
    ref_set__Ref_4bool(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__114 string) *ref_string_x {
    var retv684 *ref_string_x
    var t685 *ref_string_x = ref__Ref_6string(value__114)
    retv684 = t685
    return retv684
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__115 *ref_string_x) string {
    var retv687 string
    var t688 string = ref_get__Ref_6string(self__115)
    retv687 = t688
    return retv687
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv690 string
    var t691 string = _goml_runtime_core_char_to_string(self__3)
    retv690 = t691
    return retv690
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__116 *ref_string_x, value__117 string) struct{} {
    ref_set__Ref_6string(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv695 *_goml_vec_Token
    var t696 *_goml_vec_Token = vec_new__Vec_5Token()
    retv695 = t696
    return retv695
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__114 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv698 *ref_Vec_5Token_x
    var t699 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__114)
    retv698 = t699
    return retv698
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__115 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv701 *_goml_vec_Token
    var t702 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__115)
    retv701 = t702
    return retv701
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__75 *_goml_vec_Token, elem__76 Token) *_goml_vec_Token {
    var retv704 *_goml_vec_Token
    var result__77 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var index__78 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop706:
    for {
        var t707 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
        var t708 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__75)
        var t709 bool = t707 < t708
        if t709 {
            var t710 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t711 Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__75, t710)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__77, t711)
            var t712 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t713 int32 = t712 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__78, t713)
            continue
        } else {
            break Loop_loop706
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__77, elem__76)
    retv704 = result__77
    return retv704
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__116 *ref_Vec_5Token_x, value__117 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__84 *_goml_vec_Binding) int32 {
    var retv717 int32
    var t718 int32 = vec_len__Vec_7Binding(self__84)
    retv717 = t718
    return retv717
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__114 Value) *ref_Value_x {
    var retv720 *ref_Value_x
    var t721 *ref_Value_x = ref__Ref_5Value(value__114)
    retv720 = t721
    return retv720
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__116 *ref_Value_x, value__117 Value) struct{} {
    ref_set__Ref_5Value(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__115 *ref_Value_x) Value {
    var retv725 Value
    var t726 Value = ref_get__Ref_5Value(self__115)
    retv725 = t726
    return retv725
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv728 *_goml_vec_SExpr
    var t729 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv728 = t729
    return retv728
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__114 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv731 *ref_Vec_5SExpr_x
    var t732 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__114)
    retv731 = t732
    return retv731
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__84 *_goml_vec_Token) int32 {
    var retv734 int32
    var t735 int32 = vec_len__Vec_5Token(self__84)
    retv734 = t735
    return retv734
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__115 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv737 *_goml_vec_SExpr
    var t738 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__115)
    retv737 = t738
    return retv737
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__75 *_goml_vec_SExpr, elem__76 SExpr) *_goml_vec_SExpr {
    var retv740 *_goml_vec_SExpr
    var result__77 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var index__78 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop742:
    for {
        var t743 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
        var t744 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__75)
        var t745 bool = t743 < t744
        if t745 {
            var t746 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t747 SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__75, t746)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__77, t747)
            var t748 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t749 int32 = t748 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__78, t749)
            continue
        } else {
            break Loop_loop742
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__77, elem__76)
    retv740 = result__77
    return retv740
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__116 *ref_Vec_5SExpr_x, value__117 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv753 string
    var t754 string = _goml_runtime_core_int32_to_string(self__2)
    retv753 = t754
    return retv753
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv756 string
    var t757 string = _goml_runtime_core_bool_to_string(self__8)
    retv756 = t757
    return retv756
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__115 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv759 *_goml_vec_Binding
    var t760 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__115)
    retv759 = t760
    return retv759
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__84 *_goml_vec_SExpr) int32 {
    var retv762 int32
    var t763 int32 = vec_len__Vec_5SExpr(self__84)
    retv762 = t763
    return retv762
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__75 *_goml_vec_Binding, elem__76 Binding) *_goml_vec_Binding {
    var retv765 *_goml_vec_Binding
    var result__77 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var index__78 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop767:
    for {
        var t768 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
        var t769 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__75)
        var t770 bool = t768 < t769
        if t770 {
            var t771 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t772 Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__75, t771)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__77, t772)
            var t773 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t774 int32 = t773 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__78, t774)
            continue
        } else {
            break Loop_loop767
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__77, elem__76)
    retv765 = result__77
    return retv765
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__116 *ref_Vec_7Binding_x, value__117 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv778 *_goml_vec_string
    var t779 *_goml_vec_string = vec_new__Vec_6string()
    retv778 = t779
    return retv778
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__114 *_goml_vec_string) *ref_Vec_6string_x {
    var retv781 *ref_Vec_6string_x
    var t782 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__114)
    retv781 = t782
    return retv781
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__115 *ref_Vec_6string_x) *_goml_vec_string {
    var retv784 *_goml_vec_string
    var t785 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__115)
    retv784 = t785
    return retv784
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__75 *_goml_vec_string, elem__76 string) *_goml_vec_string {
    var retv787 *_goml_vec_string
    var result__77 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var index__78 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop789:
    for {
        var t790 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
        var t791 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__75)
        var t792 bool = t790 < t791
        if t792 {
            var t793 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t794 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__75, t793)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__77, t794)
            var t795 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t796 int32 = t795 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__78, t796)
            continue
        } else {
            break Loop_loop789
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__77, elem__76)
    retv787 = result__77
    return retv787
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__116 *ref_Vec_6string_x, value__117 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv800 *_goml_vec_Value
    var t801 *_goml_vec_Value = vec_new__Vec_5Value()
    retv800 = t801
    return retv800
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__114 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv803 *ref_Vec_5Value_x
    var t804 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__114)
    retv803 = t804
    return retv803
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__115 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv806 *_goml_vec_Value
    var t807 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__115)
    retv806 = t807
    return retv806
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__75 *_goml_vec_Value, elem__76 Value) *_goml_vec_Value {
    var retv809 *_goml_vec_Value
    var result__77 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var index__78 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop811:
    for {
        var t812 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
        var t813 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__75)
        var t814 bool = t812 < t813
        if t814 {
            var t815 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t816 Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__75, t815)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__77, t816)
            var t817 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t818 int32 = t817 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__78, t818)
            continue
        } else {
            break Loop_loop811
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__77, elem__76)
    retv809 = result__77
    return retv809
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__116 *ref_Vec_5Value_x, value__117 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__84 *_goml_vec_Value) int32 {
    var retv822 int32
    var t823 int32 = vec_len__Vec_5Value(self__84)
    retv822 = t823
    return retv822
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__114 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv825 *ref_Vec_7Binding_x
    var t826 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__114)
    retv825 = t826
    return retv825
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__84 *_goml_vec_string) int32 {
    var retv828 int32
    var t829 int32 = vec_len__Vec_6string(self__84)
    retv828 = t829
    return retv828
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv831 *_goml_vec_Binding
    var t832 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv831 = t832
    return retv831
}

func println__T_string(value__1 string) struct{} {
    var t834 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t834)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__73 *_goml_vec_Token, elem__74 Token) struct{} {
    vec_push__Vec_5Token(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__79 *_goml_vec_Token, index__80 int32) Token {
    var retv839 Token
    var t840 Token = vec_get__Vec_5Token(self__79, index__80)
    retv839 = t840
    return retv839
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__73 *_goml_vec_SExpr, elem__74 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__79 *_goml_vec_SExpr, index__80 int32) SExpr {
    var retv844 SExpr
    var t845 SExpr = vec_get__Vec_5SExpr(self__79, index__80)
    retv844 = t845
    return retv844
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__73 *_goml_vec_Binding, elem__74 Binding) struct{} {
    vec_push__Vec_7Binding(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__79 *_goml_vec_Binding, index__80 int32) Binding {
    var retv849 Binding
    var t850 Binding = vec_get__Vec_7Binding(self__79, index__80)
    retv849 = t850
    return retv849
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__73 *_goml_vec_string, elem__74 string) struct{} {
    vec_push__Vec_6string(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__79 *_goml_vec_string, index__80 int32) string {
    var retv854 string
    var t855 string = vec_get__Vec_6string(self__79, index__80)
    retv854 = t855
    return retv854
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__73 *_goml_vec_Value, elem__74 Value) struct{} {
    vec_push__Vec_5Value(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__79 *_goml_vec_Value, index__80 int32) Value {
    var retv859 Value
    var t860 Value = vec_get__Vec_5Value(self__79, index__80)
    retv859 = t860
    return retv859
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv862 string
    retv862 = self__9
    return retv862
}

func main() {
    main0()
}
