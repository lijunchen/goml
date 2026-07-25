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

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_string_decode_utf8_at_native(s string, i int) (bool, rune, int) {
    if i < 0 || i >= int(len(s)) {
        return false, 0, 0
    }
    var value rune
    var width int
    value, width = _goml_utf8.DecodeRuneInString(s[i:int(len(s))])
    if value == _goml_utf8.RuneError && width == 1 {
        return false, 0, 0
    }
    return true, value, int(width)
}

func _goml_runtime_core_string_get(s string, i int) rune {
    var valid bool
    var value rune
    valid, value, _ = _goml_runtime_string_decode_utf8_at_native(s, i)
    if !valid {
        panic("invalid string byte index")
    }
    return value
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

func vec_get__Vec_5Token(vec *_goml_vec_Token, index int) Token {
    return vec.items[index]
}

func vec_len__Vec_5Token(vec *_goml_vec_Token) int {
    return int(len(vec.items))
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

func vec_get__Vec_7Binding(vec *_goml_vec_Binding, index int) Binding {
    return vec.items[index]
}

func vec_len__Vec_7Binding(vec *_goml_vec_Binding) int {
    return int(len(vec.items))
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

func vec_get__Vec_5SExpr(vec *_goml_vec_SExpr, index int) SExpr {
    return vec.items[index]
}

func vec_len__Vec_5SExpr(vec *_goml_vec_SExpr) int {
    return int(len(vec.items))
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

func vec_get__Vec_5Value(vec *_goml_vec_Value, index int) Value {
    return vec.items[index]
}

func vec_len__Vec_5Value(vec *_goml_vec_Value) int {
    return int(len(vec.items))
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

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
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

type Tuple2_5Token_3int struct {
    _0 Token
    _1 int
}

type Tuple2_10Vec_5SExpr_3int struct {
    _0 *_goml_vec_SExpr
    _1 int
}

type Tuple2_5SExpr_3int struct {
    _0 SExpr
    _1 int
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
    var retv201 bool
    var t204 bool = ch__0 >= 48
    var jp203 bool
    if t204 {
        var t205 bool = ch__0 <= 57
        jp203 = t205
    } else {
        jp203 = false
    }
    retv201 = jp203
    return retv201
}

func digit_value(ch__1 rune) int32 {
    var retv207 int32
    var jp209 int32
    switch ch__1 {
    case 48:
        jp209 = 0
    case 49:
        jp209 = 1
    case 50:
        jp209 = 2
    case 51:
        jp209 = 3
    case 52:
        jp209 = 4
    case 53:
        jp209 = 5
    case 54:
        jp209 = 6
    case 55:
        jp209 = 7
    case 56:
        jp209 = 8
    case 57:
        jp209 = 9
    default:
        jp209 = 0
    }
    retv207 = jp209
    return retv207
}

func is_int_text(text__2 string) bool {
    var retv211 bool
    var len__3 int = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t214 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__3, 0)
    var jp213 bool
    if t214 {
        jp213 = false
        retv211 = jp213
        return retv211
    } else {
        var i__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var saw_digit__5 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        var ok__6 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
        var started__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        Loop_loop220:
        for {
            var t239 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
            var jp222 bool
            if t239 {
                var t240 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                var t241 bool = t240 < len__3
                jp222 = t241
            } else {
                jp222 = false
            }
            if jp222 {
                var t223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                var ch__8 rune = _goml_m_inherent_i_string_i_string_i_get(text__2, t223)
                var t236 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__7)
                var t237 bool = !t236
                var jp226 bool
                if t237 {
                    var t238 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__8, 45)
                    jp226 = t238
                } else {
                    jp226 = false
                }
                if jp226 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                    var t227 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                    var t228 int = t227 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__4, t228)
                } else {
                    var t231 bool = is_digit(ch__8)
                    if t231 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(saw_digit__5, true)
                        var t232 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                        var t233 int = t232 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__4, t233)
                    } else {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop220
            }
        }
        var t218 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
        var jp217 bool
        if t218 {
            var t219 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(saw_digit__5)
            jp217 = t219
        } else {
            jp217 = false
        }
        jp213 = jp217
        retv211 = jp213
        return retv211
    }
}

func parse_int32(text__9 string) int32 {
    var retv243 int32
    var len__10 int = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop251:
    for {
        var t252 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
        var t253 bool = t252 < len__10
        if t253 {
            var t254 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
            var ch__15 rune = _goml_m_inherent_i_string_i_string_i_get(text__9, t254)
            var t267 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__13)
            var t268 bool = !t267
            var jp257 bool
            if t268 {
                var t269 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__15, 45)
                jp257 = t269
            } else {
                jp257 = false
            }
            if jp257 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(negative__12, true)
                var t258 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
                var t259 int = t258 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__11, t259)
            } else {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t261 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
                var t262 int32 = t261 * 10
                var t263 int32 = t262 + d__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__14, t263)
                var t264 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
                var t265 int = t264 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__11, t265)
            }
            continue
        } else {
            break Loop_loop251
        }
    }
    var t247 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(negative__12)
    var jp246 int32
    if t247 {
        var t248 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        var t249 int32 = 0 - t248
        jp246 = t249
    } else {
        var t250 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        jp246 = t250
    }
    retv243 = jp246
    return retv243
}

func is_delim(ch__17 rune) bool {
    var retv271 bool
    var t277 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 40)
    var jp275 bool
    if t277 {
        jp275 = true
    } else {
        var t278 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 41)
        jp275 = t278
    }
    var jp273 bool
    if jp275 {
        jp273 = true
    } else {
        var t276 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 32)
        jp273 = t276
    }
    retv271 = jp273
    return retv271
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var retv280 Tuple2_5Token_3int
    var len__20 int = _goml_m_inherent_i_string_i_string_i_len(source__18)
    var text__21 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var i__22 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__19)
    var done__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop294:
    for {
        var t307 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__23)
        var t308 bool = !t307
        var jp296 bool
        if t308 {
            var t309 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
            var t310 bool = t309 < len__20
            jp296 = t310
        } else {
            jp296 = false
        }
        if jp296 {
            var t297 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
            var ch__24 rune = _goml_m_inherent_i_string_i_string_i_get(source__18, t297)
            var t299 bool = is_delim(ch__24)
            if t299 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__23, true)
            } else {
                var t301 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
                var t302 string = _goml_m_inherent_i_char_i_char_i_to__string(ch__24)
                var t303 string = t301 + t302
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(text__21, t303)
                var t304 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
                var t305 int = t304 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__22, t305)
            }
            continue
        } else {
            break Loop_loop294
        }
    }
    var atom__25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
    var jp283 Token
    switch atom__25 {
    case "true":
        var t286 Token = Token_Bool{
            _0: true,
        }
        jp283 = t286
    case "false":
        var t287 Token = Token_Bool{
            _0: false,
        }
        jp283 = t287
    default:
        var t290 bool = is_int_text(atom__25)
        var jp289 Token
        if t290 {
            var t291 int32 = parse_int32(atom__25)
            var t292 Token = Token_Int{
                _0: t291,
            }
            jp289 = t292
        } else {
            var t293 Token = Token_Sym{
                _0: atom__25,
            }
            jp289 = t293
        }
        jp283 = jp289
    }
    var token__26 Token = jp283
    var t284 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
    var t285 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: token__26,
        _1: t284,
    }
    retv280 = t285
    return retv280
}

func lex(source__27 string) *_goml_vec_Token {
    var retv312 *_goml_vec_Token
    var len__28 int = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var toks__30 *ref_Vec_5Token_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(toks0__29)
    var i__31 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop315:
    for {
        var t316 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
        var t317 bool = t316 < len__28
        if t317 {
            var t318 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
            var ch__32 rune = _goml_m_inherent_i_string_i_string_i_get(source__27, t318)
            var t320 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 40)
            if t320 {
                var t321 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t322 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t321, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t322)
                var t323 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                var t324 int = t323 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t324)
            } else {
                var t327 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 41)
                if t327 {
                    var t328 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t329 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t328, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t329)
                    var t330 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                    var t331 int = t330 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t331)
                } else {
                    var t334 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 32)
                    if t334 {
                        var t335 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                        var t336 int = t335 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t336)
                    } else {
                        var t338 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                        var mtmp77 Tuple2_5Token_3int = lex_atom(source__27, t338)
                        var x78 Token = mtmp77._0
                        var x79 int = mtmp77._1
                        var next__34 int = x79
                        var tok__33 Token = x78
                        var t339 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                        var t340 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t339, tok__33)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t340)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop315
        }
    }
    var t314 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
    retv312 = t314
    return retv312
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var retv343 Value
    var t344 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(env__35)
    var t345 int = t344 - 1
    var i__37 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t345)
    var result__38 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    var done__39 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop348:
    for {
        var t360 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__39)
        var t361 bool = !t360
        var jp350 bool
        if t361 {
            var t362 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
            var t363 bool = t362 >= 0
            jp350 = t363
        } else {
            jp350 = false
        }
        if jp350 {
            var t351 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t351)
            var t353 string = binding__40.name
            var t354 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t353, name__36)
            if t354 {
                var t355 Value = binding__40.value
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(result__38, t355)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__39, true)
            } else {
                var t357 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
                var t358 int = t357 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__37, t358)
            }
            continue
        } else {
            break Loop_loop348
        }
    }
    var t347 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(result__38)
    retv343 = t347
    return retv343
}

func lookup(local__41 *_goml_vec_Binding, global__42 *_goml_vec_Binding, name__43 string) Value {
    var retv365 Value
    var mtmp84 Value = env_lookup(local__41, name__43)
    var jp367 Value
    switch mtmp84.(type) {
    case Nil:
        var t368 Value = env_lookup(global__42, name__43)
        jp367 = t368
    default:
        var other__44 Value = mtmp84
        jp367 = other__44
    }
    retv365 = jp367
    return retv365
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var retv370 Tuple2_10Vec_5SExpr_3int
    var acc__47 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__47)
    var i__49 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__46)
    var done__50 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop375:
    for {
        var t387 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__50)
        var t388 bool = !t387
        var jp377 bool
        if t388 {
            var t389 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
            var t390 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__45)
            var t391 bool = t389 < t390
            jp377 = t391
        } else {
            jp377 = false
        }
        if jp377 {
            var t378 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
            var mtmp88 Token = vec_get__Vec_5Token(tokens__45, t378)
            switch mtmp88.(type) {
            case RParen:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__50, true)
                var t380 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
                var t381 int = t380 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__49, t381)
            default:
                var t383 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
                var mtmp93 Tuple2_5SExpr_3int = parse_expr(tokens__45, t383)
                var x94 SExpr = mtmp93._0
                var x95 int = mtmp93._1
                var next__52 int = x95
                var expr__51 SExpr = x94
                var t384 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t385 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t384, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t385)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__49, next__52)
            }
            continue
        } else {
            break Loop_loop375
        }
    }
    var t372 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
    var t373 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
    var t374 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t372,
        _1: t373,
    }
    retv370 = t374
    return retv370
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var retv393 Tuple2_5SExpr_3int
    var mtmp98 Token = vec_get__Vec_5Token(tokens__53, start__54)
    var jp395 Tuple2_5SExpr_3int
    switch mtmp98.(type) {
    case LParen:
        var t396 int = start__54 + 1
        var mtmp102 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t396)
        var x103 *_goml_vec_SExpr = mtmp102._0
        var x104 int = mtmp102._1
        var next__56 int = x104
        var items__55 *_goml_vec_SExpr = x103
        var t397 SExpr = List{
            _0: items__55,
        }
        var t398 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t397,
            _1: next__56,
        }
        jp395 = t398
    case RParen:
        var t399 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t400 int = start__54 + 1
        var t401 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t399,
            _1: t400,
        }
        jp395 = t401
    case Token_Sym:
        var x99 string = mtmp98.(Token_Sym)._0
        var name__59 string = x99
        var t402 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t403 int = start__54 + 1
        var t404 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t402,
            _1: t403,
        }
        jp395 = t404
    case Token_Int:
        var x100 int32 = mtmp98.(Token_Int)._0
        var n__58 int32 = x100
        var t405 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t406 int = start__54 + 1
        var t407 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t405,
            _1: t406,
        }
        jp395 = t407
    case Token_Bool:
        var x101 bool = mtmp98.(Token_Bool)._0
        var b__57 bool = x101
        var t408 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t409 int = start__54 + 1
        var t410 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t408,
            _1: t409,
        }
        jp395 = t410
    default:
        panic("non-exhaustive match")
    }
    retv393 = jp395
    return retv393
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var retv412 *_goml_vec_SExpr
    var i__61 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var acc__62 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__62)
    Loop_loop415:
    for {
        var t416 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__61)
        var t417 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__60)
        var t418 bool = t416 < t417
        if t418 {
            var t419 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__61)
            var mtmp105 Tuple2_5SExpr_3int = parse_expr(tokens__60, t419)
            var x106 SExpr = mtmp105._0
            var x107 int = mtmp105._1
            var next__65 int = x107
            var expr__64 SExpr = x106
            var t420 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
            var t421 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t420, expr__64)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__63, t421)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__61, next__65)
            continue
        } else {
            break Loop_loop415
        }
    }
    var t414 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
    retv412 = t414
    return retv412
}

func value_to_string(value__66 Value) string {
    var retv424 string
    var jp426 string
    switch value__66.(type) {
    case Value_Int:
        var x110 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x110
        var t427 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__67)
        jp426 = t427
    case Value_Bool:
        var x111 bool = value__66.(Value_Bool)._0
        var b__68 bool = x111
        var t428 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(b__68)
        jp426 = t428
    case Func:
        jp426 = "<lambda>"
    case Nil:
        jp426 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv424 = jp426
    return retv424
}

func truthy(value__69 Value) bool {
    var retv430 bool
    var jp432 bool
    switch value__69.(type) {
    case Value_Int:
        var x113 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x113
        var t433 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(n__71, 0)
        var t434 bool = !t433
        jp432 = t434
    case Value_Bool:
        var x114 bool = value__69.(Value_Bool)._0
        var b__70 bool = x114
        jp432 = b__70
    case Func:
        jp432 = true
    case Nil:
        jp432 = false
    default:
        panic("non-exhaustive match")
    }
    retv430 = jp432
    return retv430
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv436 Value
    var jp438 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x116 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x116
        var t439 Value = Value_Int{
            _0: n__75,
        }
        jp438 = t439
    case SExpr_Bool:
        var x117 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x117
        var t440 Value = Value_Bool{
            _0: b__76,
        }
        jp438 = t440
    case SExpr_Sym:
        var x118 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x118
        var t441 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t442 Value = lookup(local__73, t441, name__77)
        jp438 = t442
    case List:
        var x119 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x119
        var t443 Value = eval_list(items__78, local__73, global__74)
        jp438 = t443
    default:
        panic("non-exhaustive match")
    }
    retv436 = jp438
    return retv436
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv445 Value
    var t448 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t449 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t448, 0)
    var jp447 Value
    if t449 {
        jp447 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp451 Value
        switch head__82.(type) {
        case SExpr_Sym:
            var x122 string = head__82.(SExpr_Sym)._0
            var name__83 string = x122
            var t452 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp451 = t452
        default:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t453 Value = apply(f__84, args__85, global__81)
            jp451 = t453
        }
        jp447 = jp451
    }
    retv445 = jp447
    return retv445
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv455 Value
    var jp457 Value
    switch name__86 {
    case "begin":
        var t458 Value = eval_begin(items__87, 1, local__88, global__89)
        jp457 = t458
    case "define":
        var t461 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t462 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t461, 3)
        var jp460 Value
        if t462 {
            var mtmp124 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp464 Value
            switch mtmp124.(type) {
            case SExpr_Sym:
                var x127 string = mtmp124.(SExpr_Sym)._0
                var var__90 string = x127
                var t465 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t465, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t466 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t466)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp464 = value__91
            default:
                jp464 = Nil{}
            }
            jp460 = jp464
        } else {
            jp460 = Nil{}
        }
        jp457 = jp460
    case "if":
        var t469 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t470 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t469, 4)
        var jp468 Value
        if t470 {
            var t471 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t471, local__88, global__89)
            var t474 bool = truthy(cond__94)
            var jp473 Value
            if t474 {
                var t475 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t476 Value = eval(t475, local__88, global__89)
                jp473 = t476
            } else {
                var t477 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t478 Value = eval(t477, local__88, global__89)
                jp473 = t478
            }
            jp468 = jp473
        } else {
            jp468 = Nil{}
        }
        jp457 = jp468
    case "lambda":
        var t481 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t482 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t481, 3)
        var jp480 Value
        if t482 {
            var mtmp130 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp484 Value
            switch mtmp130.(type) {
            case List:
                var x134 *_goml_vec_SExpr = mtmp130.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x134
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t485 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t486 Value = Func{
                    _0: t485,
                }
                jp484 = t486
            default:
                jp484 = Nil{}
            }
            jp480 = jp484
        } else {
            jp480 = Nil{}
        }
        jp457 = jp480
    case "+":
        var t487 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t488 Value = apply_builtin("+", t487)
        jp457 = t488
    case "-":
        var t489 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t490 Value = apply_builtin("-", t489)
        jp457 = t490
    case "*":
        var t491 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t492 Value = apply_builtin("*", t491)
        jp457 = t492
    case "/":
        var t493 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t494 Value = apply_builtin("/", t493)
        jp457 = t494
    case "=":
        var t495 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t496 Value = apply_builtin("=", t495)
        jp457 = t496
    default:
        var t497 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t497, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t498 Value = apply(f__98, args__99, global__89)
        jp457 = t498
    }
    retv455 = jp457
    return retv455
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv500 Value
    var i__104 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop503:
    for {
        var t504 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
        var t505 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t506 bool = t504 < t505
        if t506 {
            var t507 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
            var t508 SExpr = vec_get__Vec_5SExpr(items__100, t507)
            var v__106 Value = eval(t508, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t509 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
            var t510 int = t509 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__104, t510)
            continue
        } else {
            break Loop_loop503
        }
    }
    var t502 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv500 = t502
    return retv500
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv513 *_goml_vec_string
    var i__108 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop516:
    for {
        var t517 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
        var t518 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t519 bool = t517 < t518
        if t519 {
            var t520 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
            var mtmp137 SExpr = vec_get__Vec_5SExpr(items__107, t520)
            switch mtmp137.(type) {
            case SExpr_Sym:
                var x140 string = mtmp137.(SExpr_Sym)._0
                var name__111 string = x140
                var t522 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t523 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t522, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t523)
                var t524 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
                var t525 int = t524 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__108, t525)
            default:
                var t527 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
                var t528 int = t527 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__108, t528)
            }
            continue
        } else {
            break Loop_loop516
        }
    }
    var t515 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv513 = t515
    return retv513
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv531 *_goml_vec_Value
    var i__116 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop534:
    for {
        var t535 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
        var t536 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t537 bool = t535 < t536
        if t537 {
            var t538 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
            var t539 SExpr = vec_get__Vec_5SExpr(items__112, t538)
            var v__119 Value = eval(t539, local__114, global__115)
            var t540 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t541 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t540, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t541)
            var t542 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
            var t543 int = t542 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__116, t543)
            continue
        } else {
            break Loop_loop534
        }
    }
    var t533 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv531 = t533
    return retv531
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv546 Value
    var jp548 Value
    switch name__120 {
    case "=":
        var t551 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t552 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t551, 2)
        var jp550 Value
        if t552 {
            var t553 Value = vec_get__Vec_5Value(args__121, 0)
            var t554 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp146 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t553,
                _1: t554,
            }
            var x147 Value = mtmp146._0
            var x148 Value = mtmp146._1
            var jp556 Value
            switch x148.(type) {
            case Value_Int:
                var x149 int32 = x148.(Value_Int)._0
                var jp558 Value
                switch x147.(type) {
                case Value_Int:
                    var x152 int32 = x147.(Value_Int)._0
                    var a__122 int32 = x152
                    var b__123 int32 = x149
                    var t559 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__122, b__123)
                    var t560 Value = Value_Bool{
                        _0: t559,
                    }
                    jp558 = t560
                default:
                    var t561 Value = Value_Bool{
                        _0: false,
                    }
                    jp558 = t561
                }
                jp556 = jp558
            case Value_Bool:
                var x150 bool = x148.(Value_Bool)._0
                var jp563 Value
                switch x147.(type) {
                case Value_Bool:
                    var x156 bool = x147.(Value_Bool)._0
                    var a__124 bool = x156
                    var b__125 bool = x150
                    var t564 bool = _goml_m_trait__impl_i_Eq_i_bool_i_eq(a__124, b__125)
                    var t565 Value = Value_Bool{
                        _0: t564,
                    }
                    jp563 = t565
                default:
                    var t566 Value = Value_Bool{
                        _0: false,
                    }
                    jp563 = t566
                }
                jp556 = jp563
            default:
                var t567 Value = Value_Bool{
                    _0: false,
                }
                jp556 = t567
            }
            jp550 = jp556
        } else {
            var t568 Value = Value_Bool{
                _0: false,
            }
            jp550 = t568
        }
        jp548 = jp550
        retv546 = jp548
        return retv546
    case "+":
        var i__126 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop572:
        for {
            var t573 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
            var t574 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t575 bool = t573 < t574
            if t575 {
                var t576 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                var mtmp158 Value = vec_get__Vec_5Value(args__121, t576)
                switch mtmp158.(type) {
                case Value_Int:
                    var x159 int32 = mtmp158.(Value_Int)._0
                    var n__128 int32 = x159
                    var t578 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t579 int32 = t578 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t579)
                    var t580 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                    var t581 int = t580 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__126, t581)
                default:
                    var t583 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                    var t584 int = t583 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__126, t584)
                }
                continue
            } else {
                break Loop_loop572
            }
        }
        var t570 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t571 Value = Value_Int{
            _0: t570,
        }
        jp548 = t571
        retv546 = jp548
        return retv546
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop589:
        for {
            var t590 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
            var t591 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t592 bool = t590 < t591
            if t592 {
                var t593 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                var mtmp164 Value = vec_get__Vec_5Value(args__121, t593)
                switch mtmp164.(type) {
                case Value_Int:
                    var x165 int32 = mtmp164.(Value_Int)._0
                    var n__131 int32 = x165
                    var t595 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t596 int32 = t595 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t596)
                    var t597 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                    var t598 int = t597 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__129, t598)
                default:
                    var t600 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                    var t601 int = t600 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__129, t601)
                }
                continue
            } else {
                break Loop_loop589
            }
        }
        var t587 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t588 Value = Value_Int{
            _0: t587,
        }
        jp548 = t588
        retv546 = jp548
        return retv546
    case "-":
        var mtmp170 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp604 Value
        switch mtmp170 {
        case 1:
            var mtmp171 Value = vec_get__Vec_5Value(args__121, 0)
            var jp606 Value
            switch mtmp171.(type) {
            case Value_Int:
                var x172 int32 = mtmp171.(Value_Int)._0
                var n__132 int32 = x172
                var t607 int32 = 0 - n__132
                var t608 Value = Value_Int{
                    _0: t607,
                }
                jp606 = t608
            default:
                jp606 = Nil{}
            }
            jp604 = jp606
        case 2:
            var t609 Value = vec_get__Vec_5Value(args__121, 0)
            var t610 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp175 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t609,
                _1: t610,
            }
            var x176 Value = mtmp175._0
            var x177 Value = mtmp175._1
            var jp612 Value
            switch x177.(type) {
            case Value_Int:
                var x178 int32 = x177.(Value_Int)._0
                var jp614 Value
                switch x176.(type) {
                case Value_Int:
                    var x181 int32 = x176.(Value_Int)._0
                    var a__133 int32 = x181
                    var b__134 int32 = x178
                    var t615 int32 = a__133 - b__134
                    var t616 Value = Value_Int{
                        _0: t615,
                    }
                    jp614 = t616
                default:
                    jp614 = Nil{}
                }
                jp612 = jp614
            default:
                jp612 = Nil{}
            }
            jp604 = jp612
        default:
            jp604 = Nil{}
        }
        jp548 = jp604
        retv546 = jp548
        return retv546
    case "/":
        var t619 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t620 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t619, 2)
        var jp618 Value
        if t620 {
            var t621 Value = vec_get__Vec_5Value(args__121, 0)
            var t622 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp184 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t621,
                _1: t622,
            }
            var x185 Value = mtmp184._0
            var x186 Value = mtmp184._1
            var jp624 Value
            switch x186.(type) {
            case Value_Int:
                var x187 int32 = x186.(Value_Int)._0
                var jp626 Value
                switch x185.(type) {
                case Value_Int:
                    var x190 int32 = x185.(Value_Int)._0
                    var a__135 int32 = x190
                    var b__136 int32 = x187
                    var t627 int32 = a__135 / b__136
                    var t628 Value = Value_Int{
                        _0: t627,
                    }
                    jp626 = t628
                default:
                    jp626 = Nil{}
                }
                jp624 = jp626
            default:
                jp624 = Nil{}
            }
            jp618 = jp624
        } else {
            jp618 = Nil{}
        }
        jp548 = jp618
        retv546 = jp548
        return retv546
    default:
        jp548 = Nil{}
        retv546 = jp548
        return retv546
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv630 Value
    var jp632 Value
    switch func__137.(type) {
    case Func:
        var x195 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x195
        var t633 Value = apply_lambda(fun__140, args__138)
        jp632 = t633
    default:
        jp632 = Nil{}
    }
    retv630 = jp632
    return retv630
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv635 Value
    var t636 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t636)
    var i__144 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop642:
    for {
        var t653 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
        var t654 *_goml_vec_string = lambda__141.params
        var t655 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t654)
        var t656 bool = t653 < t655
        var jp644 bool
        if t656 {
            var t657 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var t658 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t659 bool = t657 < t658
            jp644 = t659
        } else {
            jp644 = false
        }
        if jp644 {
            var t645 *_goml_vec_string = lambda__141.params
            var t646 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var name__145 string = vec_get__Vec_6string(t645, t646)
            var t647 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t647)
            var t648 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t649 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t648, t649)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t650 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var t651 int = t650 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__144, t651)
            continue
        } else {
            break Loop_loop642
        }
    }
    var t638 SExpr = lambda__141.body
    var t639 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t640 *ref_Vec_7Binding_x = lambda__141.global
    var t641 Value = eval(t638, t639, t640)
    retv635 = t641
    return retv635
}

func main0() struct{} {
    var t661 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t661)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t662 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t662)
    var t663 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t664 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t663, t664, global__148)
    var t665 string = value_to_string(result__151)
    println__T_string(t665)
    var t666 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t666)
    var t667 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t668 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t667, t668, global__148)
    var t669 string = value_to_string(result2__153)
    println__T_string(t669)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv671 int
    var t672 int = _goml_runtime_core_string_len(self__8)
    retv671 = t672
    return retv671
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv674 bool
    var t675 bool = self__59 == other__60
    retv674 = t675
    return retv674
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv677 *ref_int_x
    var t678 *ref_int_x = ref__Ref_3int(value__209)
    retv677 = t678
    return retv677
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__209 bool) *ref_bool_x {
    var retv680 *ref_bool_x
    var t681 *ref_bool_x = ref__Ref_4bool(value__209)
    retv680 = t681
    return retv680
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__210 *ref_bool_x) bool {
    var retv683 bool
    var t684 bool = ref_get__Ref_4bool(self__210)
    retv683 = t684
    return retv683
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv686 int
    var t687 int = ref_get__Ref_3int(self__210)
    retv686 = t687
    return retv686
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv689 rune
    var t690 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv689 = t690
    return retv689
}

func _goml_m_trait__impl_i_Eq_i_char_i_eq(self__57 rune, other__58 rune) bool {
    var retv692 bool
    var t693 bool = self__57 == other__58
    retv692 = t693
    return retv692
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__211 *ref_bool_x, value__212 bool) struct{} {
    ref_set__Ref_4bool(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv699 *ref_int32_x
    var t700 *ref_int32_x = ref__Ref_5int32(value__209)
    retv699 = t700
    return retv699
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv702 int32
    var t703 int32 = ref_get__Ref_5int32(self__210)
    retv702 = t703
    return retv702
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__209 string) *ref_string_x {
    var retv707 *ref_string_x
    var t708 *ref_string_x = ref__Ref_6string(value__209)
    retv707 = t708
    return retv707
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__210 *ref_string_x) string {
    var retv710 string
    var t711 string = ref_get__Ref_6string(self__210)
    retv710 = t711
    return retv710
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv713 string
    var t714 string = _goml_runtime_core_char_to_string(self__7)
    retv713 = t714
    return retv713
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__211 *ref_string_x, value__212 string) struct{} {
    ref_set__Ref_6string(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv718 *_goml_vec_Token
    var t719 *_goml_vec_Token = vec_new__Vec_5Token()
    retv718 = t719
    return retv718
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__209 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv721 *ref_Vec_5Token_x
    var t722 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__209)
    retv721 = t722
    return retv721
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__210 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv724 *_goml_vec_Token
    var t725 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__210)
    retv724 = t725
    return retv724
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__130 *_goml_vec_Token, elem__131 Token) *_goml_vec_Token {
    var retv727 *_goml_vec_Token
    var result__132 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var index__133 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop729:
    for {
        var t730 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
        var t731 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__130)
        var t732 bool = t730 < t731
        if t732 {
            var t733 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t734 Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__130, t733)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__132, t734)
            var t735 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t736 int = t735 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__133, t736)
            continue
        } else {
            break Loop_loop729
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__132, elem__131)
    retv727 = result__132
    return retv727
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__211 *ref_Vec_5Token_x, value__212 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__139 *_goml_vec_Binding) int {
    var retv740 int
    var t741 int = vec_len__Vec_7Binding(self__139)
    retv740 = t741
    return retv740
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__209 Value) *ref_Value_x {
    var retv743 *ref_Value_x
    var t744 *ref_Value_x = ref__Ref_5Value(value__209)
    retv743 = t744
    return retv743
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv746 bool
    var t747 bool = self__55 == other__56
    retv746 = t747
    return retv746
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__211 *ref_Value_x, value__212 Value) struct{} {
    ref_set__Ref_5Value(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__210 *ref_Value_x) Value {
    var retv751 Value
    var t752 Value = ref_get__Ref_5Value(self__210)
    retv751 = t752
    return retv751
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv754 *_goml_vec_SExpr
    var t755 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv754 = t755
    return retv754
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__209 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv757 *ref_Vec_5SExpr_x
    var t758 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__209)
    retv757 = t758
    return retv757
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__139 *_goml_vec_Token) int {
    var retv760 int
    var t761 int = vec_len__Vec_5Token(self__139)
    retv760 = t761
    return retv760
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__210 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv763 *_goml_vec_SExpr
    var t764 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__210)
    retv763 = t764
    return retv763
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__130 *_goml_vec_SExpr, elem__131 SExpr) *_goml_vec_SExpr {
    var retv766 *_goml_vec_SExpr
    var result__132 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var index__133 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop768:
    for {
        var t769 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
        var t770 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__130)
        var t771 bool = t769 < t770
        if t771 {
            var t772 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t773 SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__130, t772)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__132, t773)
            var t774 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t775 int = t774 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__133, t775)
            continue
        } else {
            break Loop_loop768
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__132, elem__131)
    retv766 = result__132
    return retv766
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__211 *ref_Vec_5SExpr_x, value__212 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv779 string
    var t780 string = _goml_runtime_core_int32_to_string(self__6)
    retv779 = t780
    return retv779
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv782 string
    var t783 string = _goml_runtime_core_bool_to_string(self__37)
    retv782 = t783
    return retv782
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv785 bool
    var t786 bool = self__65 == other__66
    retv785 = t786
    return retv785
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__210 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv788 *_goml_vec_Binding
    var t789 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__210)
    retv788 = t789
    return retv788
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__139 *_goml_vec_SExpr) int {
    var retv791 int
    var t792 int = vec_len__Vec_5SExpr(self__139)
    retv791 = t792
    return retv791
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__130 *_goml_vec_Binding, elem__131 Binding) *_goml_vec_Binding {
    var retv794 *_goml_vec_Binding
    var result__132 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var index__133 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop796:
    for {
        var t797 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
        var t798 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__130)
        var t799 bool = t797 < t798
        if t799 {
            var t800 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t801 Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__130, t800)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__132, t801)
            var t802 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t803 int = t802 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__133, t803)
            continue
        } else {
            break Loop_loop796
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__132, elem__131)
    retv794 = result__132
    return retv794
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__211 *ref_Vec_7Binding_x, value__212 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv807 *_goml_vec_string
    var t808 *_goml_vec_string = vec_new__Vec_6string()
    retv807 = t808
    return retv807
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__209 *_goml_vec_string) *ref_Vec_6string_x {
    var retv810 *ref_Vec_6string_x
    var t811 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__209)
    retv810 = t811
    return retv810
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__210 *ref_Vec_6string_x) *_goml_vec_string {
    var retv813 *_goml_vec_string
    var t814 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__210)
    retv813 = t814
    return retv813
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__130 *_goml_vec_string, elem__131 string) *_goml_vec_string {
    var retv816 *_goml_vec_string
    var result__132 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var index__133 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop818:
    for {
        var t819 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
        var t820 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__130)
        var t821 bool = t819 < t820
        if t821 {
            var t822 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t823 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__130, t822)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__132, t823)
            var t824 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t825 int = t824 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__133, t825)
            continue
        } else {
            break Loop_loop818
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__132, elem__131)
    retv816 = result__132
    return retv816
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__211 *ref_Vec_6string_x, value__212 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv829 *_goml_vec_Value
    var t830 *_goml_vec_Value = vec_new__Vec_5Value()
    retv829 = t830
    return retv829
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__209 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv832 *ref_Vec_5Value_x
    var t833 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__209)
    retv832 = t833
    return retv832
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__210 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv835 *_goml_vec_Value
    var t836 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__210)
    retv835 = t836
    return retv835
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__130 *_goml_vec_Value, elem__131 Value) *_goml_vec_Value {
    var retv838 *_goml_vec_Value
    var result__132 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var index__133 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop840:
    for {
        var t841 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
        var t842 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__130)
        var t843 bool = t841 < t842
        if t843 {
            var t844 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t845 Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__130, t844)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__132, t845)
            var t846 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t847 int = t846 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__133, t847)
            continue
        } else {
            break Loop_loop840
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__132, elem__131)
    retv838 = result__132
    return retv838
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__211 *ref_Vec_5Value_x, value__212 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__139 *_goml_vec_Value) int {
    var retv851 int
    var t852 int = vec_len__Vec_5Value(self__139)
    retv851 = t852
    return retv851
}

func _goml_m_trait__impl_i_Eq_i_bool_i_eq(self__53 bool, other__54 bool) bool {
    var retv854 bool
    var t855 bool = self__53 == other__54
    retv854 = t855
    return retv854
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__209 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv857 *ref_Vec_7Binding_x
    var t858 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__209)
    retv857 = t858
    return retv857
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__139 *_goml_vec_string) int {
    var retv860 int
    var t861 int = vec_len__Vec_6string(self__139)
    retv860 = t861
    return retv860
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv863 *_goml_vec_Binding
    var t864 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv863 = t864
    return retv863
}

func println__T_string(value__1 string) struct{} {
    var t866 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t866)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__128 *_goml_vec_Token, elem__129 Token) struct{} {
    vec_push__Vec_5Token(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__134 *_goml_vec_Token, index__135 int) Token {
    var retv871 Token
    var t872 Token = vec_get__Vec_5Token(self__134, index__135)
    retv871 = t872
    return retv871
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__128 *_goml_vec_SExpr, elem__129 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__134 *_goml_vec_SExpr, index__135 int) SExpr {
    var retv876 SExpr
    var t877 SExpr = vec_get__Vec_5SExpr(self__134, index__135)
    retv876 = t877
    return retv876
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__128 *_goml_vec_Binding, elem__129 Binding) struct{} {
    vec_push__Vec_7Binding(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__134 *_goml_vec_Binding, index__135 int) Binding {
    var retv881 Binding
    var t882 Binding = vec_get__Vec_7Binding(self__134, index__135)
    retv881 = t882
    return retv881
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__128 *_goml_vec_string, elem__129 string) struct{} {
    vec_push__Vec_6string(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__134 *_goml_vec_string, index__135 int) string {
    var retv886 string
    var t887 string = vec_get__Vec_6string(self__134, index__135)
    retv886 = t887
    return retv886
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__128 *_goml_vec_Value, elem__129 Value) struct{} {
    vec_push__Vec_5Value(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__134 *_goml_vec_Value, index__135 int) Value {
    var retv891 Value
    var t892 Value = vec_get__Vec_5Value(self__134, index__135)
    retv891 = t892
    return retv891
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv894 string
    retv894 = self__38
    return retv894
}

func main() {
    main0()
}
