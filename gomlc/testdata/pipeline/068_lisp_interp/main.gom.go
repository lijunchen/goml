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
    var retv205 bool
    var t208 bool = ch__0 >= 48
    var jp207 bool
    if t208 {
        var t209 bool = ch__0 <= 57
        jp207 = t209
    } else {
        jp207 = false
    }
    retv205 = jp207
    return retv205
}

func digit_value(ch__1 rune) int32 {
    var retv211 int32
    var jp213 int32
    switch ch__1 {
    case 48:
        jp213 = 0
    case 49:
        jp213 = 1
    case 50:
        jp213 = 2
    case 51:
        jp213 = 3
    case 52:
        jp213 = 4
    case 53:
        jp213 = 5
    case 54:
        jp213 = 6
    case 55:
        jp213 = 7
    case 56:
        jp213 = 8
    case 57:
        jp213 = 9
    default:
        jp213 = 0
    }
    retv211 = jp213
    return retv211
}

func is_int_text(text__2 string) bool {
    var retv215 bool
    var len__3 int = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t218 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__3, 0)
    var jp217 bool
    if t218 {
        jp217 = false
        retv215 = jp217
        return retv215
    } else {
        var i__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var saw_digit__5 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        var ok__6 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
        var started__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        Loop_loop224:
        for {
            var t243 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
            var jp226 bool
            if t243 {
                var t244 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                var t245 bool = t244 < len__3
                jp226 = t245
            } else {
                jp226 = false
            }
            if jp226 {
                var t227 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                var ch__8 rune = _goml_m_inherent_i_string_i_string_i_get(text__2, t227)
                var t240 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__7)
                var t241 bool = !t240
                var jp230 bool
                if t241 {
                    var t242 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__8, 45)
                    jp230 = t242
                } else {
                    jp230 = false
                }
                if jp230 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                    var t231 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                    var t232 int = t231 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__4, t232)
                } else {
                    var t235 bool = is_digit(ch__8)
                    if t235 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(saw_digit__5, true)
                        var t236 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                        var t237 int = t236 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__4, t237)
                    } else {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop224
            }
        }
        var t222 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
        var jp221 bool
        if t222 {
            var t223 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(saw_digit__5)
            jp221 = t223
        } else {
            jp221 = false
        }
        jp217 = jp221
        retv215 = jp217
        return retv215
    }
}

func parse_int32(text__9 string) int32 {
    var retv247 int32
    var len__10 int = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop255:
    for {
        var t256 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
        var t257 bool = t256 < len__10
        if t257 {
            var t258 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
            var ch__15 rune = _goml_m_inherent_i_string_i_string_i_get(text__9, t258)
            var t271 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__13)
            var t272 bool = !t271
            var jp261 bool
            if t272 {
                var t273 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__15, 45)
                jp261 = t273
            } else {
                jp261 = false
            }
            if jp261 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(negative__12, true)
                var t262 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
                var t263 int = t262 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__11, t263)
            } else {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t265 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
                var t266 int32 = t265 * 10
                var t267 int32 = t266 + d__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__14, t267)
                var t268 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
                var t269 int = t268 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__11, t269)
            }
            continue
        } else {
            break Loop_loop255
        }
    }
    var t251 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(negative__12)
    var jp250 int32
    if t251 {
        var t252 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        var t253 int32 = 0 - t252
        jp250 = t253
    } else {
        var t254 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        jp250 = t254
    }
    retv247 = jp250
    return retv247
}

func is_delim(ch__17 rune) bool {
    var retv275 bool
    var t281 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 40)
    var jp279 bool
    if t281 {
        jp279 = true
    } else {
        var t282 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 41)
        jp279 = t282
    }
    var jp277 bool
    if jp279 {
        jp277 = true
    } else {
        var t280 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 32)
        jp277 = t280
    }
    retv275 = jp277
    return retv275
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var retv284 Tuple2_5Token_3int
    var len__20 int = _goml_m_inherent_i_string_i_string_i_len(source__18)
    var text__21 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var i__22 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__19)
    var done__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop298:
    for {
        var t311 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__23)
        var t312 bool = !t311
        var jp300 bool
        if t312 {
            var t313 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
            var t314 bool = t313 < len__20
            jp300 = t314
        } else {
            jp300 = false
        }
        if jp300 {
            var t301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
            var ch__24 rune = _goml_m_inherent_i_string_i_string_i_get(source__18, t301)
            var t303 bool = is_delim(ch__24)
            if t303 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__23, true)
            } else {
                var t305 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
                var t306 string = _goml_m_inherent_i_char_i_char_i_to__string(ch__24)
                var t307 string = t305 + t306
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(text__21, t307)
                var t308 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
                var t309 int = t308 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__22, t309)
            }
            continue
        } else {
            break Loop_loop298
        }
    }
    var atom__25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
    var jp287 Token
    switch atom__25 {
    case "true":
        var t290 Token = Token_Bool{
            _0: true,
        }
        jp287 = t290
    case "false":
        var t291 Token = Token_Bool{
            _0: false,
        }
        jp287 = t291
    default:
        var t294 bool = is_int_text(atom__25)
        var jp293 Token
        if t294 {
            var t295 int32 = parse_int32(atom__25)
            var t296 Token = Token_Int{
                _0: t295,
            }
            jp293 = t296
        } else {
            var t297 Token = Token_Sym{
                _0: atom__25,
            }
            jp293 = t297
        }
        jp287 = jp293
    }
    var token__26 Token = jp287
    var t288 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
    var t289 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: token__26,
        _1: t288,
    }
    retv284 = t289
    return retv284
}

func lex(source__27 string) *_goml_vec_Token {
    var retv316 *_goml_vec_Token
    var len__28 int = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var toks__30 *ref_Vec_5Token_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(toks0__29)
    var i__31 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop319:
    for {
        var t320 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
        var t321 bool = t320 < len__28
        if t321 {
            var t322 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
            var ch__32 rune = _goml_m_inherent_i_string_i_string_i_get(source__27, t322)
            var t324 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 40)
            if t324 {
                var t325 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t326 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t325, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t326)
                var t327 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                var t328 int = t327 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t328)
            } else {
                var t331 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 41)
                if t331 {
                    var t332 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t333 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t332, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t333)
                    var t334 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                    var t335 int = t334 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t335)
                } else {
                    var t338 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 32)
                    if t338 {
                        var t339 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                        var t340 int = t339 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t340)
                    } else {
                        var t342 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                        var mtmp81 Tuple2_5Token_3int = lex_atom(source__27, t342)
                        var x82 Token = mtmp81._0
                        var x83 int = mtmp81._1
                        var next__34 int = x83
                        var tok__33 Token = x82
                        var t343 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                        var t344 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t343, tok__33)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t344)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop319
        }
    }
    var t318 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
    retv316 = t318
    return retv316
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var retv347 Value
    var t348 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(env__35)
    var t349 int = t348 - 1
    var i__37 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t349)
    var result__38 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    var done__39 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop352:
    for {
        var t364 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__39)
        var t365 bool = !t364
        var jp354 bool
        if t365 {
            var t366 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
            var t367 bool = t366 >= 0
            jp354 = t367
        } else {
            jp354 = false
        }
        if jp354 {
            var t355 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t355)
            var t357 string = binding__40.name
            var t358 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t357, name__36)
            if t358 {
                var t359 Value = binding__40.value
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(result__38, t359)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__39, true)
            } else {
                var t361 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
                var t362 int = t361 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__37, t362)
            }
            continue
        } else {
            break Loop_loop352
        }
    }
    var t351 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(result__38)
    retv347 = t351
    return retv347
}

func lookup(local__41 *_goml_vec_Binding, global__42 *_goml_vec_Binding, name__43 string) Value {
    var retv369 Value
    var mtmp88 Value = env_lookup(local__41, name__43)
    var jp371 Value
    switch mtmp88.(type) {
    case Nil:
        var t372 Value = env_lookup(global__42, name__43)
        jp371 = t372
    default:
        var other__44 Value = mtmp88
        jp371 = other__44
    }
    retv369 = jp371
    return retv369
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var retv374 Tuple2_10Vec_5SExpr_3int
    var acc__47 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__47)
    var i__49 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__46)
    var done__50 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop379:
    for {
        var t391 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__50)
        var t392 bool = !t391
        var jp381 bool
        if t392 {
            var t393 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
            var t394 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__45)
            var t395 bool = t393 < t394
            jp381 = t395
        } else {
            jp381 = false
        }
        if jp381 {
            var t382 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
            var mtmp92 Token = vec_get__Vec_5Token(tokens__45, t382)
            switch mtmp92.(type) {
            case RParen:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__50, true)
                var t384 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
                var t385 int = t384 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__49, t385)
            default:
                var t387 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
                var mtmp97 Tuple2_5SExpr_3int = parse_expr(tokens__45, t387)
                var x98 SExpr = mtmp97._0
                var x99 int = mtmp97._1
                var next__52 int = x99
                var expr__51 SExpr = x98
                var t388 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t389 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t388, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t389)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__49, next__52)
            }
            continue
        } else {
            break Loop_loop379
        }
    }
    var t376 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
    var t377 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
    var t378 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t376,
        _1: t377,
    }
    retv374 = t378
    return retv374
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var retv397 Tuple2_5SExpr_3int
    var mtmp102 Token = vec_get__Vec_5Token(tokens__53, start__54)
    var jp399 Tuple2_5SExpr_3int
    switch mtmp102.(type) {
    case LParen:
        var t400 int = start__54 + 1
        var mtmp106 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t400)
        var x107 *_goml_vec_SExpr = mtmp106._0
        var x108 int = mtmp106._1
        var next__56 int = x108
        var items__55 *_goml_vec_SExpr = x107
        var t401 SExpr = List{
            _0: items__55,
        }
        var t402 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t401,
            _1: next__56,
        }
        jp399 = t402
    case RParen:
        var t403 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t404 int = start__54 + 1
        var t405 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t403,
            _1: t404,
        }
        jp399 = t405
    case Token_Sym:
        var x103 string = mtmp102.(Token_Sym)._0
        var name__59 string = x103
        var t406 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t407 int = start__54 + 1
        var t408 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t406,
            _1: t407,
        }
        jp399 = t408
    case Token_Int:
        var x104 int32 = mtmp102.(Token_Int)._0
        var n__58 int32 = x104
        var t409 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t410 int = start__54 + 1
        var t411 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t409,
            _1: t410,
        }
        jp399 = t411
    case Token_Bool:
        var x105 bool = mtmp102.(Token_Bool)._0
        var b__57 bool = x105
        var t412 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t413 int = start__54 + 1
        var t414 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t412,
            _1: t413,
        }
        jp399 = t414
    default:
        panic("non-exhaustive match")
    }
    retv397 = jp399
    return retv397
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var retv416 *_goml_vec_SExpr
    var i__61 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var acc__62 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__62)
    Loop_loop419:
    for {
        var t420 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__61)
        var t421 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__60)
        var t422 bool = t420 < t421
        if t422 {
            var t423 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__61)
            var mtmp109 Tuple2_5SExpr_3int = parse_expr(tokens__60, t423)
            var x110 SExpr = mtmp109._0
            var x111 int = mtmp109._1
            var next__65 int = x111
            var expr__64 SExpr = x110
            var t424 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
            var t425 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t424, expr__64)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__63, t425)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__61, next__65)
            continue
        } else {
            break Loop_loop419
        }
    }
    var t418 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
    retv416 = t418
    return retv416
}

func value_to_string(value__66 Value) string {
    var retv428 string
    var jp430 string
    switch value__66.(type) {
    case Value_Int:
        var x114 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x114
        var t431 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__67)
        jp430 = t431
    case Value_Bool:
        var x115 bool = value__66.(Value_Bool)._0
        var b__68 bool = x115
        var t432 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(b__68)
        jp430 = t432
    case Func:
        jp430 = "<lambda>"
    case Nil:
        jp430 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv428 = jp430
    return retv428
}

func truthy(value__69 Value) bool {
    var retv434 bool
    var jp436 bool
    switch value__69.(type) {
    case Value_Int:
        var x117 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x117
        var t437 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(n__71, 0)
        var t438 bool = !t437
        jp436 = t438
    case Value_Bool:
        var x118 bool = value__69.(Value_Bool)._0
        var b__70 bool = x118
        jp436 = b__70
    case Func:
        jp436 = true
    case Nil:
        jp436 = false
    default:
        panic("non-exhaustive match")
    }
    retv434 = jp436
    return retv434
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv440 Value
    var jp442 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x120 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x120
        var t443 Value = Value_Int{
            _0: n__75,
        }
        jp442 = t443
    case SExpr_Bool:
        var x121 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x121
        var t444 Value = Value_Bool{
            _0: b__76,
        }
        jp442 = t444
    case SExpr_Sym:
        var x122 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x122
        var t445 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t446 Value = lookup(local__73, t445, name__77)
        jp442 = t446
    case List:
        var x123 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x123
        var t447 Value = eval_list(items__78, local__73, global__74)
        jp442 = t447
    default:
        panic("non-exhaustive match")
    }
    retv440 = jp442
    return retv440
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv449 Value
    var t452 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t453 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t452, 0)
    var jp451 Value
    if t453 {
        jp451 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp455 Value
        switch head__82.(type) {
        case SExpr_Sym:
            var x126 string = head__82.(SExpr_Sym)._0
            var name__83 string = x126
            var t456 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp455 = t456
        default:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t457 Value = apply(f__84, args__85, global__81)
            jp455 = t457
        }
        jp451 = jp455
    }
    retv449 = jp451
    return retv449
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv459 Value
    var jp461 Value
    switch name__86 {
    case "begin":
        var t462 Value = eval_begin(items__87, 1, local__88, global__89)
        jp461 = t462
    case "define":
        var t465 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t466 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t465, 3)
        var jp464 Value
        if t466 {
            var mtmp128 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp468 Value
            switch mtmp128.(type) {
            case SExpr_Sym:
                var x131 string = mtmp128.(SExpr_Sym)._0
                var var__90 string = x131
                var t469 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t469, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t470 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t470)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp468 = value__91
            default:
                jp468 = Nil{}
            }
            jp464 = jp468
        } else {
            jp464 = Nil{}
        }
        jp461 = jp464
    case "if":
        var t473 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t474 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t473, 4)
        var jp472 Value
        if t474 {
            var t475 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t475, local__88, global__89)
            var t478 bool = truthy(cond__94)
            var jp477 Value
            if t478 {
                var t479 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t480 Value = eval(t479, local__88, global__89)
                jp477 = t480
            } else {
                var t481 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t482 Value = eval(t481, local__88, global__89)
                jp477 = t482
            }
            jp472 = jp477
        } else {
            jp472 = Nil{}
        }
        jp461 = jp472
    case "lambda":
        var t485 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t486 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t485, 3)
        var jp484 Value
        if t486 {
            var mtmp134 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp488 Value
            switch mtmp134.(type) {
            case List:
                var x138 *_goml_vec_SExpr = mtmp134.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x138
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t489 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t490 Value = Func{
                    _0: t489,
                }
                jp488 = t490
            default:
                jp488 = Nil{}
            }
            jp484 = jp488
        } else {
            jp484 = Nil{}
        }
        jp461 = jp484
    case "+":
        var t491 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t492 Value = apply_builtin("+", t491)
        jp461 = t492
    case "-":
        var t493 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t494 Value = apply_builtin("-", t493)
        jp461 = t494
    case "*":
        var t495 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t496 Value = apply_builtin("*", t495)
        jp461 = t496
    case "/":
        var t497 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t498 Value = apply_builtin("/", t497)
        jp461 = t498
    case "=":
        var t499 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t500 Value = apply_builtin("=", t499)
        jp461 = t500
    default:
        var t501 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t501, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t502 Value = apply(f__98, args__99, global__89)
        jp461 = t502
    }
    retv459 = jp461
    return retv459
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv504 Value
    var i__104 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop507:
    for {
        var t508 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
        var t509 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t510 bool = t508 < t509
        if t510 {
            var t511 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
            var t512 SExpr = vec_get__Vec_5SExpr(items__100, t511)
            var v__106 Value = eval(t512, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t513 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
            var t514 int = t513 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__104, t514)
            continue
        } else {
            break Loop_loop507
        }
    }
    var t506 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv504 = t506
    return retv504
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv517 *_goml_vec_string
    var i__108 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop520:
    for {
        var t521 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
        var t522 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t523 bool = t521 < t522
        if t523 {
            var t524 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
            var mtmp141 SExpr = vec_get__Vec_5SExpr(items__107, t524)
            switch mtmp141.(type) {
            case SExpr_Sym:
                var x144 string = mtmp141.(SExpr_Sym)._0
                var name__111 string = x144
                var t526 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t527 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t526, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t527)
                var t528 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
                var t529 int = t528 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__108, t529)
            default:
                var t531 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
                var t532 int = t531 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__108, t532)
            }
            continue
        } else {
            break Loop_loop520
        }
    }
    var t519 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv517 = t519
    return retv517
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv535 *_goml_vec_Value
    var i__116 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop538:
    for {
        var t539 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
        var t540 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t541 bool = t539 < t540
        if t541 {
            var t542 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
            var t543 SExpr = vec_get__Vec_5SExpr(items__112, t542)
            var v__119 Value = eval(t543, local__114, global__115)
            var t544 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t545 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t544, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t545)
            var t546 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
            var t547 int = t546 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__116, t547)
            continue
        } else {
            break Loop_loop538
        }
    }
    var t537 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv535 = t537
    return retv535
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv550 Value
    var jp552 Value
    switch name__120 {
    case "=":
        var t555 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t556 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t555, 2)
        var jp554 Value
        if t556 {
            var t557 Value = vec_get__Vec_5Value(args__121, 0)
            var t558 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp150 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t557,
                _1: t558,
            }
            var x151 Value = mtmp150._0
            var x152 Value = mtmp150._1
            var jp560 Value
            switch x152.(type) {
            case Value_Int:
                var x153 int32 = x152.(Value_Int)._0
                var jp562 Value
                switch x151.(type) {
                case Value_Int:
                    var x156 int32 = x151.(Value_Int)._0
                    var a__122 int32 = x156
                    var b__123 int32 = x153
                    var t563 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__122, b__123)
                    var t564 Value = Value_Bool{
                        _0: t563,
                    }
                    jp562 = t564
                default:
                    var t565 Value = Value_Bool{
                        _0: false,
                    }
                    jp562 = t565
                }
                jp560 = jp562
            case Value_Bool:
                var x154 bool = x152.(Value_Bool)._0
                var jp567 Value
                switch x151.(type) {
                case Value_Bool:
                    var x160 bool = x151.(Value_Bool)._0
                    var a__124 bool = x160
                    var b__125 bool = x154
                    var t568 bool = _goml_m_trait__impl_i_Eq_i_bool_i_eq(a__124, b__125)
                    var t569 Value = Value_Bool{
                        _0: t568,
                    }
                    jp567 = t569
                default:
                    var t570 Value = Value_Bool{
                        _0: false,
                    }
                    jp567 = t570
                }
                jp560 = jp567
            default:
                var t571 Value = Value_Bool{
                    _0: false,
                }
                jp560 = t571
            }
            jp554 = jp560
        } else {
            var t572 Value = Value_Bool{
                _0: false,
            }
            jp554 = t572
        }
        jp552 = jp554
        retv550 = jp552
        return retv550
    case "+":
        var i__126 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop576:
        for {
            var t577 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
            var t578 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t579 bool = t577 < t578
            if t579 {
                var t580 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                var mtmp162 Value = vec_get__Vec_5Value(args__121, t580)
                switch mtmp162.(type) {
                case Value_Int:
                    var x163 int32 = mtmp162.(Value_Int)._0
                    var n__128 int32 = x163
                    var t582 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t583 int32 = t582 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t583)
                    var t584 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                    var t585 int = t584 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__126, t585)
                default:
                    var t587 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                    var t588 int = t587 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__126, t588)
                }
                continue
            } else {
                break Loop_loop576
            }
        }
        var t574 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t575 Value = Value_Int{
            _0: t574,
        }
        jp552 = t575
        retv550 = jp552
        return retv550
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop593:
        for {
            var t594 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
            var t595 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t596 bool = t594 < t595
            if t596 {
                var t597 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                var mtmp168 Value = vec_get__Vec_5Value(args__121, t597)
                switch mtmp168.(type) {
                case Value_Int:
                    var x169 int32 = mtmp168.(Value_Int)._0
                    var n__131 int32 = x169
                    var t599 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t600 int32 = t599 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t600)
                    var t601 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                    var t602 int = t601 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__129, t602)
                default:
                    var t604 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                    var t605 int = t604 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__129, t605)
                }
                continue
            } else {
                break Loop_loop593
            }
        }
        var t591 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t592 Value = Value_Int{
            _0: t591,
        }
        jp552 = t592
        retv550 = jp552
        return retv550
    case "-":
        var mtmp174 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp608 Value
        switch mtmp174 {
        case 1:
            var mtmp175 Value = vec_get__Vec_5Value(args__121, 0)
            var jp610 Value
            switch mtmp175.(type) {
            case Value_Int:
                var x176 int32 = mtmp175.(Value_Int)._0
                var n__132 int32 = x176
                var t611 int32 = 0 - n__132
                var t612 Value = Value_Int{
                    _0: t611,
                }
                jp610 = t612
            default:
                jp610 = Nil{}
            }
            jp608 = jp610
        case 2:
            var t613 Value = vec_get__Vec_5Value(args__121, 0)
            var t614 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp179 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t613,
                _1: t614,
            }
            var x180 Value = mtmp179._0
            var x181 Value = mtmp179._1
            var jp616 Value
            switch x181.(type) {
            case Value_Int:
                var x182 int32 = x181.(Value_Int)._0
                var jp618 Value
                switch x180.(type) {
                case Value_Int:
                    var x185 int32 = x180.(Value_Int)._0
                    var a__133 int32 = x185
                    var b__134 int32 = x182
                    var t619 int32 = a__133 - b__134
                    var t620 Value = Value_Int{
                        _0: t619,
                    }
                    jp618 = t620
                default:
                    jp618 = Nil{}
                }
                jp616 = jp618
            default:
                jp616 = Nil{}
            }
            jp608 = jp616
        default:
            jp608 = Nil{}
        }
        jp552 = jp608
        retv550 = jp552
        return retv550
    case "/":
        var t623 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t624 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t623, 2)
        var jp622 Value
        if t624 {
            var t625 Value = vec_get__Vec_5Value(args__121, 0)
            var t626 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp188 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t625,
                _1: t626,
            }
            var x189 Value = mtmp188._0
            var x190 Value = mtmp188._1
            var jp628 Value
            switch x190.(type) {
            case Value_Int:
                var x191 int32 = x190.(Value_Int)._0
                var jp630 Value
                switch x189.(type) {
                case Value_Int:
                    var x194 int32 = x189.(Value_Int)._0
                    var a__135 int32 = x194
                    var b__136 int32 = x191
                    var t631 int32 = a__135 / b__136
                    var t632 Value = Value_Int{
                        _0: t631,
                    }
                    jp630 = t632
                default:
                    jp630 = Nil{}
                }
                jp628 = jp630
            default:
                jp628 = Nil{}
            }
            jp622 = jp628
        } else {
            jp622 = Nil{}
        }
        jp552 = jp622
        retv550 = jp552
        return retv550
    default:
        jp552 = Nil{}
        retv550 = jp552
        return retv550
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv634 Value
    var jp636 Value
    switch func__137.(type) {
    case Func:
        var x199 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x199
        var t637 Value = apply_lambda(fun__140, args__138)
        jp636 = t637
    default:
        jp636 = Nil{}
    }
    retv634 = jp636
    return retv634
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv639 Value
    var t640 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t640)
    var i__144 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop646:
    for {
        var t657 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
        var t658 *_goml_vec_string = lambda__141.params
        var t659 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t658)
        var t660 bool = t657 < t659
        var jp648 bool
        if t660 {
            var t661 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var t662 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t663 bool = t661 < t662
            jp648 = t663
        } else {
            jp648 = false
        }
        if jp648 {
            var t649 *_goml_vec_string = lambda__141.params
            var t650 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var name__145 string = vec_get__Vec_6string(t649, t650)
            var t651 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t651)
            var t652 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t653 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t652, t653)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t654 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var t655 int = t654 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__144, t655)
            continue
        } else {
            break Loop_loop646
        }
    }
    var t642 SExpr = lambda__141.body
    var t643 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t644 *ref_Vec_7Binding_x = lambda__141.global
    var t645 Value = eval(t642, t643, t644)
    retv639 = t645
    return retv639
}

func main0() struct{} {
    var t665 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t665)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t666 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t666)
    var t667 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t668 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t667, t668, global__148)
    var t669 string = value_to_string(result__151)
    println__T_string(t669)
    var t670 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t670)
    var t671 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t672 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t671, t672, global__148)
    var t673 string = value_to_string(result2__153)
    println__T_string(t673)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv675 int
    var t676 int = _goml_runtime_core_string_len(self__8)
    retv675 = t676
    return retv675
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv678 bool
    var t679 bool = self__59 == other__60
    retv678 = t679
    return retv678
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv681 *ref_int_x
    var t682 *ref_int_x = ref__Ref_3int(value__207)
    retv681 = t682
    return retv681
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv684 *ref_bool_x
    var t685 *ref_bool_x = ref__Ref_4bool(value__207)
    retv684 = t685
    return retv684
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv687 bool
    var t688 bool = ref_get__Ref_4bool(self__208)
    retv687 = t688
    return retv687
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv690 int
    var t691 int = ref_get__Ref_3int(self__208)
    retv690 = t691
    return retv690
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv693 rune
    var t694 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv693 = t694
    return retv693
}

func _goml_m_trait__impl_i_Eq_i_char_i_eq(self__57 rune, other__58 rune) bool {
    var retv696 bool
    var t697 bool = self__57 == other__58
    retv696 = t697
    return retv696
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__209 *ref_bool_x, value__210 bool) struct{} {
    ref_set__Ref_4bool(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv703 *ref_int32_x
    var t704 *ref_int32_x = ref__Ref_5int32(value__207)
    retv703 = t704
    return retv703
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv706 int32
    var t707 int32 = ref_get__Ref_5int32(self__208)
    retv706 = t707
    return retv706
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__207 string) *ref_string_x {
    var retv711 *ref_string_x
    var t712 *ref_string_x = ref__Ref_6string(value__207)
    retv711 = t712
    return retv711
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__208 *ref_string_x) string {
    var retv714 string
    var t715 string = ref_get__Ref_6string(self__208)
    retv714 = t715
    return retv714
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv717 string
    var t718 string = _goml_runtime_core_char_to_string(self__7)
    retv717 = t718
    return retv717
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__209 *ref_string_x, value__210 string) struct{} {
    ref_set__Ref_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv722 *_goml_vec_Token
    var t723 *_goml_vec_Token = vec_new__Vec_5Token()
    retv722 = t723
    return retv722
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__207 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv725 *ref_Vec_5Token_x
    var t726 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__207)
    retv725 = t726
    return retv725
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__208 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv728 *_goml_vec_Token
    var t729 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__208)
    retv728 = t729
    return retv728
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__128 *_goml_vec_Token, elem__129 Token) *_goml_vec_Token {
    var retv731 *_goml_vec_Token
    var result__130 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop733:
    for {
        var t734 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t735 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__128)
        var t736 bool = t734 < t735
        if t736 {
            var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t738 Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__128, t737)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__130, t738)
            var t739 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t740 int = t739 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t740)
            continue
        } else {
            break Loop_loop733
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__130, elem__129)
    retv731 = result__130
    return retv731
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__209 *ref_Vec_5Token_x, value__210 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__137 *_goml_vec_Binding) int {
    var retv744 int
    var t745 int = vec_len__Vec_7Binding(self__137)
    retv744 = t745
    return retv744
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__207 Value) *ref_Value_x {
    var retv747 *ref_Value_x
    var t748 *ref_Value_x = ref__Ref_5Value(value__207)
    retv747 = t748
    return retv747
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv750 bool
    var t751 bool = self__55 == other__56
    retv750 = t751
    return retv750
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__209 *ref_Value_x, value__210 Value) struct{} {
    ref_set__Ref_5Value(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__208 *ref_Value_x) Value {
    var retv755 Value
    var t756 Value = ref_get__Ref_5Value(self__208)
    retv755 = t756
    return retv755
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv758 *_goml_vec_SExpr
    var t759 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv758 = t759
    return retv758
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__207 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv761 *ref_Vec_5SExpr_x
    var t762 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__207)
    retv761 = t762
    return retv761
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__137 *_goml_vec_Token) int {
    var retv764 int
    var t765 int = vec_len__Vec_5Token(self__137)
    retv764 = t765
    return retv764
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__208 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv767 *_goml_vec_SExpr
    var t768 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__208)
    retv767 = t768
    return retv767
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__128 *_goml_vec_SExpr, elem__129 SExpr) *_goml_vec_SExpr {
    var retv770 *_goml_vec_SExpr
    var result__130 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop772:
    for {
        var t773 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t774 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__128)
        var t775 bool = t773 < t774
        if t775 {
            var t776 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t777 SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__128, t776)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__130, t777)
            var t778 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t779 int = t778 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t779)
            continue
        } else {
            break Loop_loop772
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__130, elem__129)
    retv770 = result__130
    return retv770
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__209 *ref_Vec_5SExpr_x, value__210 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv783 string
    var t784 string = _goml_runtime_core_int32_to_string(self__6)
    retv783 = t784
    return retv783
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv786 string
    var t787 string = _goml_runtime_core_bool_to_string(self__37)
    retv786 = t787
    return retv786
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv789 bool
    var t790 bool = self__65 == other__66
    retv789 = t790
    return retv789
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__208 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv792 *_goml_vec_Binding
    var t793 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__208)
    retv792 = t793
    return retv792
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__137 *_goml_vec_SExpr) int {
    var retv795 int
    var t796 int = vec_len__Vec_5SExpr(self__137)
    retv795 = t796
    return retv795
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__128 *_goml_vec_Binding, elem__129 Binding) *_goml_vec_Binding {
    var retv798 *_goml_vec_Binding
    var result__130 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop800:
    for {
        var t801 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t802 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__128)
        var t803 bool = t801 < t802
        if t803 {
            var t804 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t805 Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__128, t804)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__130, t805)
            var t806 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t807 int = t806 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t807)
            continue
        } else {
            break Loop_loop800
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__130, elem__129)
    retv798 = result__130
    return retv798
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__209 *ref_Vec_7Binding_x, value__210 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv811 *_goml_vec_string
    var t812 *_goml_vec_string = vec_new__Vec_6string()
    retv811 = t812
    return retv811
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__207 *_goml_vec_string) *ref_Vec_6string_x {
    var retv814 *ref_Vec_6string_x
    var t815 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__207)
    retv814 = t815
    return retv814
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__208 *ref_Vec_6string_x) *_goml_vec_string {
    var retv817 *_goml_vec_string
    var t818 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__208)
    retv817 = t818
    return retv817
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__128 *_goml_vec_string, elem__129 string) *_goml_vec_string {
    var retv820 *_goml_vec_string
    var result__130 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop822:
    for {
        var t823 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t824 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__128)
        var t825 bool = t823 < t824
        if t825 {
            var t826 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t827 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__128, t826)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__130, t827)
            var t828 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t829 int = t828 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t829)
            continue
        } else {
            break Loop_loop822
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__130, elem__129)
    retv820 = result__130
    return retv820
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__209 *ref_Vec_6string_x, value__210 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv833 *_goml_vec_Value
    var t834 *_goml_vec_Value = vec_new__Vec_5Value()
    retv833 = t834
    return retv833
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__207 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv836 *ref_Vec_5Value_x
    var t837 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__207)
    retv836 = t837
    return retv836
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__208 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv839 *_goml_vec_Value
    var t840 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__208)
    retv839 = t840
    return retv839
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__128 *_goml_vec_Value, elem__129 Value) *_goml_vec_Value {
    var retv842 *_goml_vec_Value
    var result__130 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop844:
    for {
        var t845 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t846 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__128)
        var t847 bool = t845 < t846
        if t847 {
            var t848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t849 Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__128, t848)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__130, t849)
            var t850 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t851 int = t850 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t851)
            continue
        } else {
            break Loop_loop844
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__130, elem__129)
    retv842 = result__130
    return retv842
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__209 *ref_Vec_5Value_x, value__210 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__137 *_goml_vec_Value) int {
    var retv855 int
    var t856 int = vec_len__Vec_5Value(self__137)
    retv855 = t856
    return retv855
}

func _goml_m_trait__impl_i_Eq_i_bool_i_eq(self__53 bool, other__54 bool) bool {
    var retv858 bool
    var t859 bool = self__53 == other__54
    retv858 = t859
    return retv858
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__207 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv861 *ref_Vec_7Binding_x
    var t862 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__207)
    retv861 = t862
    return retv861
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__137 *_goml_vec_string) int {
    var retv864 int
    var t865 int = vec_len__Vec_6string(self__137)
    retv864 = t865
    return retv864
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv867 *_goml_vec_Binding
    var t868 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv867 = t868
    return retv867
}

func println__T_string(value__1 string) struct{} {
    var t870 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t870)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__126 *_goml_vec_Token, elem__127 Token) struct{} {
    vec_push__Vec_5Token(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__132 *_goml_vec_Token, index__133 int) Token {
    var retv875 Token
    var t876 Token = vec_get__Vec_5Token(self__132, index__133)
    retv875 = t876
    return retv875
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__126 *_goml_vec_SExpr, elem__127 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__132 *_goml_vec_SExpr, index__133 int) SExpr {
    var retv880 SExpr
    var t881 SExpr = vec_get__Vec_5SExpr(self__132, index__133)
    retv880 = t881
    return retv880
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__126 *_goml_vec_Binding, elem__127 Binding) struct{} {
    vec_push__Vec_7Binding(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__132 *_goml_vec_Binding, index__133 int) Binding {
    var retv885 Binding
    var t886 Binding = vec_get__Vec_7Binding(self__132, index__133)
    retv885 = t886
    return retv885
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__132 *_goml_vec_string, index__133 int) string {
    var retv890 string
    var t891 string = vec_get__Vec_6string(self__132, index__133)
    retv890 = t891
    return retv890
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__126 *_goml_vec_Value, elem__127 Value) struct{} {
    vec_push__Vec_5Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__132 *_goml_vec_Value, index__133 int) Value {
    var retv895 Value
    var t896 Value = vec_get__Vec_5Value(self__132, index__133)
    retv895 = t896
    return retv895
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv898 string
    retv898 = self__38
    return retv898
}

func main() {
    main0()
}
