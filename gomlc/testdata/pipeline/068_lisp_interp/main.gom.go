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
    var retv245 bool
    var t248 bool = ch__0 >= 48
    var jp247 bool
    if t248 {
        var t249 bool = ch__0 <= 57
        jp247 = t249
    } else {
        jp247 = false
    }
    retv245 = jp247
    return retv245
}

func digit_value(ch__1 rune) int32 {
    var retv251 int32
    var jp253 int32
    switch ch__1 {
    case 48:
        jp253 = 0
    case 49:
        jp253 = 1
    case 50:
        jp253 = 2
    case 51:
        jp253 = 3
    case 52:
        jp253 = 4
    case 53:
        jp253 = 5
    case 54:
        jp253 = 6
    case 55:
        jp253 = 7
    case 56:
        jp253 = 8
    case 57:
        jp253 = 9
    default:
        jp253 = 0
    }
    retv251 = jp253
    return retv251
}

func is_int_text(text__2 string) bool {
    var retv255 bool
    var len__3 int = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t258 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__3, 0)
    var jp257 bool
    if t258 {
        jp257 = false
        retv255 = jp257
        return retv255
    } else {
        var i__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var saw_digit__5 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        var ok__6 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
        var started__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        Loop_loop264:
        for {
            var t283 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
            var jp266 bool
            if t283 {
                var t284 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                var t285 bool = t284 < len__3
                jp266 = t285
            } else {
                jp266 = false
            }
            if jp266 {
                var t267 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                var ch__8 rune = _goml_m_inherent_i_string_i_string_i_get(text__2, t267)
                var t280 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__7)
                var t281 bool = !t280
                var jp270 bool
                if t281 {
                    var t282 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__8, 45)
                    jp270 = t282
                } else {
                    jp270 = false
                }
                if jp270 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                    var t271 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                    var t272 int = t271 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__4, t272)
                } else {
                    var t275 bool = is_digit(ch__8)
                    if t275 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(saw_digit__5, true)
                        var t276 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                        var t277 int = t276 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__4, t277)
                    } else {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop264
            }
        }
        var t262 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
        var jp261 bool
        if t262 {
            var t263 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(saw_digit__5)
            jp261 = t263
        } else {
            jp261 = false
        }
        jp257 = jp261
        retv255 = jp257
        return retv255
    }
}

func parse_int32(text__9 string) int32 {
    var retv287 int32
    var len__10 int = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop295:
    for {
        var t296 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
        var t297 bool = t296 < len__10
        if t297 {
            var t298 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
            var ch__15 rune = _goml_m_inherent_i_string_i_string_i_get(text__9, t298)
            var t311 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__13)
            var t312 bool = !t311
            var jp301 bool
            if t312 {
                var t313 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__15, 45)
                jp301 = t313
            } else {
                jp301 = false
            }
            if jp301 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(negative__12, true)
                var t302 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
                var t303 int = t302 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__11, t303)
            } else {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t305 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
                var t306 int32 = t305 * 10
                var t307 int32 = t306 + d__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__14, t307)
                var t308 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
                var t309 int = t308 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__11, t309)
            }
            continue
        } else {
            break Loop_loop295
        }
    }
    var t291 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(negative__12)
    var jp290 int32
    if t291 {
        var t292 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        var t293 int32 = 0 - t292
        jp290 = t293
    } else {
        var t294 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        jp290 = t294
    }
    retv287 = jp290
    return retv287
}

func is_delim(ch__17 rune) bool {
    var retv315 bool
    var t321 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 40)
    var jp319 bool
    if t321 {
        jp319 = true
    } else {
        var t322 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 41)
        jp319 = t322
    }
    var jp317 bool
    if jp319 {
        jp317 = true
    } else {
        var t320 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 32)
        jp317 = t320
    }
    retv315 = jp317
    return retv315
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var retv324 Tuple2_5Token_3int
    var len__20 int = _goml_m_inherent_i_string_i_string_i_len(source__18)
    var text__21 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var i__22 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__19)
    var done__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop338:
    for {
        var t351 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__23)
        var t352 bool = !t351
        var jp340 bool
        if t352 {
            var t353 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
            var t354 bool = t353 < len__20
            jp340 = t354
        } else {
            jp340 = false
        }
        if jp340 {
            var t341 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
            var ch__24 rune = _goml_m_inherent_i_string_i_string_i_get(source__18, t341)
            var t343 bool = is_delim(ch__24)
            if t343 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__23, true)
            } else {
                var t345 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
                var t346 string = _goml_m_inherent_i_char_i_char_i_to__string(ch__24)
                var t347 string = t345 + t346
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(text__21, t347)
                var t348 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
                var t349 int = t348 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__22, t349)
            }
            continue
        } else {
            break Loop_loop338
        }
    }
    var atom__25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
    var jp327 Token
    switch atom__25 {
    case "true":
        var t330 Token = Token_Bool{
            _0: true,
        }
        jp327 = t330
    case "false":
        var t331 Token = Token_Bool{
            _0: false,
        }
        jp327 = t331
    default:
        var t334 bool = is_int_text(atom__25)
        var jp333 Token
        if t334 {
            var t335 int32 = parse_int32(atom__25)
            var t336 Token = Token_Int{
                _0: t335,
            }
            jp333 = t336
        } else {
            var t337 Token = Token_Sym{
                _0: atom__25,
            }
            jp333 = t337
        }
        jp327 = jp333
    }
    var token__26 Token = jp327
    var t328 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
    var t329 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: token__26,
        _1: t328,
    }
    retv324 = t329
    return retv324
}

func lex(source__27 string) *_goml_vec_Token {
    var retv356 *_goml_vec_Token
    var len__28 int = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var toks__30 *ref_Vec_5Token_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(toks0__29)
    var i__31 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop359:
    for {
        var t360 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
        var t361 bool = t360 < len__28
        if t361 {
            var t362 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
            var ch__32 rune = _goml_m_inherent_i_string_i_string_i_get(source__27, t362)
            var t364 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 40)
            if t364 {
                var t365 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t366 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t365, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t366)
                var t367 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                var t368 int = t367 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t368)
            } else {
                var t371 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 41)
                if t371 {
                    var t372 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t373 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t372, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t373)
                    var t374 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                    var t375 int = t374 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t375)
                } else {
                    var t378 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 32)
                    if t378 {
                        var t379 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                        var t380 int = t379 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t380)
                    } else {
                        var t382 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                        var mtmp121 Tuple2_5Token_3int = lex_atom(source__27, t382)
                        var x122 Token = mtmp121._0
                        var x123 int = mtmp121._1
                        var next__34 int = x123
                        var tok__33 Token = x122
                        var t383 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                        var t384 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t383, tok__33)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t384)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop359
        }
    }
    var t358 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
    retv356 = t358
    return retv356
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var retv387 Value
    var t388 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(env__35)
    var t389 int = t388 - 1
    var i__37 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t389)
    var result__38 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    var done__39 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop392:
    for {
        var t404 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__39)
        var t405 bool = !t404
        var jp394 bool
        if t405 {
            var t406 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
            var t407 bool = t406 >= 0
            jp394 = t407
        } else {
            jp394 = false
        }
        if jp394 {
            var t395 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t395)
            var t397 string = binding__40.name
            var t398 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t397, name__36)
            if t398 {
                var t399 Value = binding__40.value
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(result__38, t399)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__39, true)
            } else {
                var t401 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
                var t402 int = t401 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__37, t402)
            }
            continue
        } else {
            break Loop_loop392
        }
    }
    var t391 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(result__38)
    retv387 = t391
    return retv387
}

func lookup(local__41 *_goml_vec_Binding, global__42 *_goml_vec_Binding, name__43 string) Value {
    var retv409 Value
    var mtmp128 Value = env_lookup(local__41, name__43)
    var jp411 Value
    switch mtmp128.(type) {
    case Nil:
        var t412 Value = env_lookup(global__42, name__43)
        jp411 = t412
    default:
        var other__44 Value = mtmp128
        jp411 = other__44
    }
    retv409 = jp411
    return retv409
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var retv414 Tuple2_10Vec_5SExpr_3int
    var acc__47 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__47)
    var i__49 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__46)
    var done__50 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop419:
    for {
        var t431 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__50)
        var t432 bool = !t431
        var jp421 bool
        if t432 {
            var t433 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
            var t434 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__45)
            var t435 bool = t433 < t434
            jp421 = t435
        } else {
            jp421 = false
        }
        if jp421 {
            var t422 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
            var mtmp132 Token = vec_get__Vec_5Token(tokens__45, t422)
            switch mtmp132.(type) {
            case RParen:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__50, true)
                var t424 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
                var t425 int = t424 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__49, t425)
            default:
                var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
                var mtmp137 Tuple2_5SExpr_3int = parse_expr(tokens__45, t427)
                var x138 SExpr = mtmp137._0
                var x139 int = mtmp137._1
                var next__52 int = x139
                var expr__51 SExpr = x138
                var t428 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t429 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t428, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t429)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__49, next__52)
            }
            continue
        } else {
            break Loop_loop419
        }
    }
    var t416 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
    var t417 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
    var t418 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t416,
        _1: t417,
    }
    retv414 = t418
    return retv414
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var retv437 Tuple2_5SExpr_3int
    var mtmp142 Token = vec_get__Vec_5Token(tokens__53, start__54)
    var jp439 Tuple2_5SExpr_3int
    switch mtmp142.(type) {
    case LParen:
        var t440 int = start__54 + 1
        var mtmp146 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t440)
        var x147 *_goml_vec_SExpr = mtmp146._0
        var x148 int = mtmp146._1
        var next__56 int = x148
        var items__55 *_goml_vec_SExpr = x147
        var t441 SExpr = List{
            _0: items__55,
        }
        var t442 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t441,
            _1: next__56,
        }
        jp439 = t442
    case RParen:
        var t443 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t444 int = start__54 + 1
        var t445 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t443,
            _1: t444,
        }
        jp439 = t445
    case Token_Sym:
        var x143 string = mtmp142.(Token_Sym)._0
        var name__59 string = x143
        var t446 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t447 int = start__54 + 1
        var t448 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t446,
            _1: t447,
        }
        jp439 = t448
    case Token_Int:
        var x144 int32 = mtmp142.(Token_Int)._0
        var n__58 int32 = x144
        var t449 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t450 int = start__54 + 1
        var t451 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t449,
            _1: t450,
        }
        jp439 = t451
    case Token_Bool:
        var x145 bool = mtmp142.(Token_Bool)._0
        var b__57 bool = x145
        var t452 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t453 int = start__54 + 1
        var t454 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t452,
            _1: t453,
        }
        jp439 = t454
    default:
        panic("non-exhaustive match")
    }
    retv437 = jp439
    return retv437
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var retv456 *_goml_vec_SExpr
    var i__61 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var acc__62 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__62)
    Loop_loop459:
    for {
        var t460 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__61)
        var t461 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__60)
        var t462 bool = t460 < t461
        if t462 {
            var t463 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__61)
            var mtmp149 Tuple2_5SExpr_3int = parse_expr(tokens__60, t463)
            var x150 SExpr = mtmp149._0
            var x151 int = mtmp149._1
            var next__65 int = x151
            var expr__64 SExpr = x150
            var t464 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
            var t465 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t464, expr__64)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__63, t465)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__61, next__65)
            continue
        } else {
            break Loop_loop459
        }
    }
    var t458 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
    retv456 = t458
    return retv456
}

func value_to_string(value__66 Value) string {
    var retv468 string
    var jp470 string
    switch value__66.(type) {
    case Value_Int:
        var x154 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x154
        var t471 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__67)
        jp470 = t471
    case Value_Bool:
        var x155 bool = value__66.(Value_Bool)._0
        var b__68 bool = x155
        var t472 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(b__68)
        jp470 = t472
    case Func:
        jp470 = "<lambda>"
    case Nil:
        jp470 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv468 = jp470
    return retv468
}

func truthy(value__69 Value) bool {
    var retv474 bool
    var jp476 bool
    switch value__69.(type) {
    case Value_Int:
        var x157 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x157
        var t477 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(n__71, 0)
        var t478 bool = !t477
        jp476 = t478
    case Value_Bool:
        var x158 bool = value__69.(Value_Bool)._0
        var b__70 bool = x158
        jp476 = b__70
    case Func:
        jp476 = true
    case Nil:
        jp476 = false
    default:
        panic("non-exhaustive match")
    }
    retv474 = jp476
    return retv474
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv480 Value
    var jp482 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x160 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x160
        var t483 Value = Value_Int{
            _0: n__75,
        }
        jp482 = t483
    case SExpr_Bool:
        var x161 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x161
        var t484 Value = Value_Bool{
            _0: b__76,
        }
        jp482 = t484
    case SExpr_Sym:
        var x162 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x162
        var t485 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t486 Value = lookup(local__73, t485, name__77)
        jp482 = t486
    case List:
        var x163 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x163
        var t487 Value = eval_list(items__78, local__73, global__74)
        jp482 = t487
    default:
        panic("non-exhaustive match")
    }
    retv480 = jp482
    return retv480
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv489 Value
    var t492 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t493 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t492, 0)
    var jp491 Value
    if t493 {
        jp491 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp495 Value
        switch head__82.(type) {
        case SExpr_Sym:
            var x166 string = head__82.(SExpr_Sym)._0
            var name__83 string = x166
            var t496 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp495 = t496
        default:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t497 Value = apply(f__84, args__85, global__81)
            jp495 = t497
        }
        jp491 = jp495
    }
    retv489 = jp491
    return retv489
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv499 Value
    var jp501 Value
    switch name__86 {
    case "begin":
        var t502 Value = eval_begin(items__87, 1, local__88, global__89)
        jp501 = t502
    case "define":
        var t505 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t506 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t505, 3)
        var jp504 Value
        if t506 {
            var mtmp168 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp508 Value
            switch mtmp168.(type) {
            case SExpr_Sym:
                var x171 string = mtmp168.(SExpr_Sym)._0
                var var__90 string = x171
                var t509 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t509, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t510 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t510)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp508 = value__91
            default:
                jp508 = Nil{}
            }
            jp504 = jp508
        } else {
            jp504 = Nil{}
        }
        jp501 = jp504
    case "if":
        var t513 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t514 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t513, 4)
        var jp512 Value
        if t514 {
            var t515 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t515, local__88, global__89)
            var t518 bool = truthy(cond__94)
            var jp517 Value
            if t518 {
                var t519 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t520 Value = eval(t519, local__88, global__89)
                jp517 = t520
            } else {
                var t521 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t522 Value = eval(t521, local__88, global__89)
                jp517 = t522
            }
            jp512 = jp517
        } else {
            jp512 = Nil{}
        }
        jp501 = jp512
    case "lambda":
        var t525 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t526 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t525, 3)
        var jp524 Value
        if t526 {
            var mtmp174 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp528 Value
            switch mtmp174.(type) {
            case List:
                var x178 *_goml_vec_SExpr = mtmp174.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x178
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t529 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t530 Value = Func{
                    _0: t529,
                }
                jp528 = t530
            default:
                jp528 = Nil{}
            }
            jp524 = jp528
        } else {
            jp524 = Nil{}
        }
        jp501 = jp524
    case "+":
        var t531 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t532 Value = apply_builtin("+", t531)
        jp501 = t532
    case "-":
        var t533 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t534 Value = apply_builtin("-", t533)
        jp501 = t534
    case "*":
        var t535 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t536 Value = apply_builtin("*", t535)
        jp501 = t536
    case "/":
        var t537 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t538 Value = apply_builtin("/", t537)
        jp501 = t538
    case "=":
        var t539 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t540 Value = apply_builtin("=", t539)
        jp501 = t540
    default:
        var t541 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t541, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t542 Value = apply(f__98, args__99, global__89)
        jp501 = t542
    }
    retv499 = jp501
    return retv499
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv544 Value
    var i__104 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop547:
    for {
        var t548 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
        var t549 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t550 bool = t548 < t549
        if t550 {
            var t551 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
            var t552 SExpr = vec_get__Vec_5SExpr(items__100, t551)
            var v__106 Value = eval(t552, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t553 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
            var t554 int = t553 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__104, t554)
            continue
        } else {
            break Loop_loop547
        }
    }
    var t546 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv544 = t546
    return retv544
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv557 *_goml_vec_string
    var i__108 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop560:
    for {
        var t561 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
        var t562 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t563 bool = t561 < t562
        if t563 {
            var t564 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
            var mtmp181 SExpr = vec_get__Vec_5SExpr(items__107, t564)
            switch mtmp181.(type) {
            case SExpr_Sym:
                var x184 string = mtmp181.(SExpr_Sym)._0
                var name__111 string = x184
                var t566 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t567 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t566, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t567)
                var t568 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
                var t569 int = t568 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__108, t569)
            default:
                var t571 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
                var t572 int = t571 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__108, t572)
            }
            continue
        } else {
            break Loop_loop560
        }
    }
    var t559 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv557 = t559
    return retv557
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv575 *_goml_vec_Value
    var i__116 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop578:
    for {
        var t579 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
        var t580 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t581 bool = t579 < t580
        if t581 {
            var t582 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
            var t583 SExpr = vec_get__Vec_5SExpr(items__112, t582)
            var v__119 Value = eval(t583, local__114, global__115)
            var t584 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t585 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t584, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t585)
            var t586 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
            var t587 int = t586 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__116, t587)
            continue
        } else {
            break Loop_loop578
        }
    }
    var t577 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv575 = t577
    return retv575
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv590 Value
    var jp592 Value
    switch name__120 {
    case "=":
        var t595 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t596 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t595, 2)
        var jp594 Value
        if t596 {
            var t597 Value = vec_get__Vec_5Value(args__121, 0)
            var t598 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp190 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t597,
                _1: t598,
            }
            var x191 Value = mtmp190._0
            var x192 Value = mtmp190._1
            var jp600 Value
            switch x192.(type) {
            case Value_Int:
                var x193 int32 = x192.(Value_Int)._0
                var jp602 Value
                switch x191.(type) {
                case Value_Int:
                    var x196 int32 = x191.(Value_Int)._0
                    var a__122 int32 = x196
                    var b__123 int32 = x193
                    var t603 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__122, b__123)
                    var t604 Value = Value_Bool{
                        _0: t603,
                    }
                    jp602 = t604
                default:
                    var t605 Value = Value_Bool{
                        _0: false,
                    }
                    jp602 = t605
                }
                jp600 = jp602
            case Value_Bool:
                var x194 bool = x192.(Value_Bool)._0
                var jp607 Value
                switch x191.(type) {
                case Value_Bool:
                    var x200 bool = x191.(Value_Bool)._0
                    var a__124 bool = x200
                    var b__125 bool = x194
                    var t608 bool = _goml_m_trait__impl_i_Eq_i_bool_i_eq(a__124, b__125)
                    var t609 Value = Value_Bool{
                        _0: t608,
                    }
                    jp607 = t609
                default:
                    var t610 Value = Value_Bool{
                        _0: false,
                    }
                    jp607 = t610
                }
                jp600 = jp607
            default:
                var t611 Value = Value_Bool{
                    _0: false,
                }
                jp600 = t611
            }
            jp594 = jp600
        } else {
            var t612 Value = Value_Bool{
                _0: false,
            }
            jp594 = t612
        }
        jp592 = jp594
        retv590 = jp592
        return retv590
    case "+":
        var i__126 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop616:
        for {
            var t617 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
            var t618 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t619 bool = t617 < t618
            if t619 {
                var t620 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                var mtmp202 Value = vec_get__Vec_5Value(args__121, t620)
                switch mtmp202.(type) {
                case Value_Int:
                    var x203 int32 = mtmp202.(Value_Int)._0
                    var n__128 int32 = x203
                    var t622 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t623 int32 = t622 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t623)
                    var t624 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                    var t625 int = t624 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__126, t625)
                default:
                    var t627 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                    var t628 int = t627 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__126, t628)
                }
                continue
            } else {
                break Loop_loop616
            }
        }
        var t614 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t615 Value = Value_Int{
            _0: t614,
        }
        jp592 = t615
        retv590 = jp592
        return retv590
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop633:
        for {
            var t634 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
            var t635 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t636 bool = t634 < t635
            if t636 {
                var t637 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                var mtmp208 Value = vec_get__Vec_5Value(args__121, t637)
                switch mtmp208.(type) {
                case Value_Int:
                    var x209 int32 = mtmp208.(Value_Int)._0
                    var n__131 int32 = x209
                    var t639 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t640 int32 = t639 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t640)
                    var t641 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                    var t642 int = t641 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__129, t642)
                default:
                    var t644 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                    var t645 int = t644 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__129, t645)
                }
                continue
            } else {
                break Loop_loop633
            }
        }
        var t631 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t632 Value = Value_Int{
            _0: t631,
        }
        jp592 = t632
        retv590 = jp592
        return retv590
    case "-":
        var mtmp214 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp648 Value
        switch mtmp214 {
        case 1:
            var mtmp215 Value = vec_get__Vec_5Value(args__121, 0)
            var jp650 Value
            switch mtmp215.(type) {
            case Value_Int:
                var x216 int32 = mtmp215.(Value_Int)._0
                var n__132 int32 = x216
                var t651 int32 = 0 - n__132
                var t652 Value = Value_Int{
                    _0: t651,
                }
                jp650 = t652
            default:
                jp650 = Nil{}
            }
            jp648 = jp650
        case 2:
            var t653 Value = vec_get__Vec_5Value(args__121, 0)
            var t654 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp219 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t653,
                _1: t654,
            }
            var x220 Value = mtmp219._0
            var x221 Value = mtmp219._1
            var jp656 Value
            switch x221.(type) {
            case Value_Int:
                var x222 int32 = x221.(Value_Int)._0
                var jp658 Value
                switch x220.(type) {
                case Value_Int:
                    var x225 int32 = x220.(Value_Int)._0
                    var a__133 int32 = x225
                    var b__134 int32 = x222
                    var t659 int32 = a__133 - b__134
                    var t660 Value = Value_Int{
                        _0: t659,
                    }
                    jp658 = t660
                default:
                    jp658 = Nil{}
                }
                jp656 = jp658
            default:
                jp656 = Nil{}
            }
            jp648 = jp656
        default:
            jp648 = Nil{}
        }
        jp592 = jp648
        retv590 = jp592
        return retv590
    case "/":
        var t663 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t664 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t663, 2)
        var jp662 Value
        if t664 {
            var t665 Value = vec_get__Vec_5Value(args__121, 0)
            var t666 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp228 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t665,
                _1: t666,
            }
            var x229 Value = mtmp228._0
            var x230 Value = mtmp228._1
            var jp668 Value
            switch x230.(type) {
            case Value_Int:
                var x231 int32 = x230.(Value_Int)._0
                var jp670 Value
                switch x229.(type) {
                case Value_Int:
                    var x234 int32 = x229.(Value_Int)._0
                    var a__135 int32 = x234
                    var b__136 int32 = x231
                    var t671 int32 = a__135 / b__136
                    var t672 Value = Value_Int{
                        _0: t671,
                    }
                    jp670 = t672
                default:
                    jp670 = Nil{}
                }
                jp668 = jp670
            default:
                jp668 = Nil{}
            }
            jp662 = jp668
        } else {
            jp662 = Nil{}
        }
        jp592 = jp662
        retv590 = jp592
        return retv590
    default:
        jp592 = Nil{}
        retv590 = jp592
        return retv590
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv674 Value
    var jp676 Value
    switch func__137.(type) {
    case Func:
        var x239 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x239
        var t677 Value = apply_lambda(fun__140, args__138)
        jp676 = t677
    default:
        jp676 = Nil{}
    }
    retv674 = jp676
    return retv674
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv679 Value
    var t680 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t680)
    var i__144 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop686:
    for {
        var t697 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
        var t698 *_goml_vec_string = lambda__141.params
        var t699 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t698)
        var t700 bool = t697 < t699
        var jp688 bool
        if t700 {
            var t701 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var t702 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t703 bool = t701 < t702
            jp688 = t703
        } else {
            jp688 = false
        }
        if jp688 {
            var t689 *_goml_vec_string = lambda__141.params
            var t690 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var name__145 string = vec_get__Vec_6string(t689, t690)
            var t691 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t691)
            var t692 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t693 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t692, t693)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t694 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var t695 int = t694 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__144, t695)
            continue
        } else {
            break Loop_loop686
        }
    }
    var t682 SExpr = lambda__141.body
    var t683 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t684 *ref_Vec_7Binding_x = lambda__141.global
    var t685 Value = eval(t682, t683, t684)
    retv679 = t685
    return retv679
}

func main0() struct{} {
    var t705 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t705)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t706 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t706)
    var t707 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t708 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t707, t708, global__148)
    var t709 string = value_to_string(result__151)
    println__T_string(t709)
    var t710 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t710)
    var t711 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t712 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t711, t712, global__148)
    var t713 string = value_to_string(result2__153)
    println__T_string(t713)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv715 int
    var t716 int = _goml_runtime_core_string_len(self__8)
    retv715 = t716
    return retv715
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv718 bool
    var t719 bool = self__59 == other__60
    retv718 = t719
    return retv718
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv721 *ref_int_x
    var t722 *ref_int_x = ref__Ref_3int(value__207)
    retv721 = t722
    return retv721
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv724 *ref_bool_x
    var t725 *ref_bool_x = ref__Ref_4bool(value__207)
    retv724 = t725
    return retv724
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv727 bool
    var t728 bool = ref_get__Ref_4bool(self__208)
    retv727 = t728
    return retv727
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv730 int
    var t731 int = ref_get__Ref_3int(self__208)
    retv730 = t731
    return retv730
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv733 rune
    var t734 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv733 = t734
    return retv733
}

func _goml_m_trait__impl_i_Eq_i_char_i_eq(self__57 rune, other__58 rune) bool {
    var retv736 bool
    var t737 bool = self__57 == other__58
    retv736 = t737
    return retv736
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
    var retv743 *ref_int32_x
    var t744 *ref_int32_x = ref__Ref_5int32(value__207)
    retv743 = t744
    return retv743
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv746 int32
    var t747 int32 = ref_get__Ref_5int32(self__208)
    retv746 = t747
    return retv746
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__207 string) *ref_string_x {
    var retv751 *ref_string_x
    var t752 *ref_string_x = ref__Ref_6string(value__207)
    retv751 = t752
    return retv751
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__208 *ref_string_x) string {
    var retv754 string
    var t755 string = ref_get__Ref_6string(self__208)
    retv754 = t755
    return retv754
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv757 string
    var t758 string = _goml_runtime_core_char_to_string(self__7)
    retv757 = t758
    return retv757
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__209 *ref_string_x, value__210 string) struct{} {
    ref_set__Ref_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv762 *_goml_vec_Token
    var t763 *_goml_vec_Token = vec_new__Vec_5Token()
    retv762 = t763
    return retv762
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__207 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv765 *ref_Vec_5Token_x
    var t766 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__207)
    retv765 = t766
    return retv765
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__208 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv768 *_goml_vec_Token
    var t769 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__208)
    retv768 = t769
    return retv768
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__128 *_goml_vec_Token, elem__129 Token) *_goml_vec_Token {
    var retv771 *_goml_vec_Token
    var result__130 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop773:
    for {
        var t774 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t775 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__128)
        var t776 bool = t774 < t775
        if t776 {
            var t777 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t778 Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__128, t777)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__130, t778)
            var t779 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t780 int = t779 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t780)
            continue
        } else {
            break Loop_loop773
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__130, elem__129)
    retv771 = result__130
    return retv771
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__209 *ref_Vec_5Token_x, value__210 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__137 *_goml_vec_Binding) int {
    var retv784 int
    var t785 int = vec_len__Vec_7Binding(self__137)
    retv784 = t785
    return retv784
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__207 Value) *ref_Value_x {
    var retv787 *ref_Value_x
    var t788 *ref_Value_x = ref__Ref_5Value(value__207)
    retv787 = t788
    return retv787
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv790 bool
    var t791 bool = self__55 == other__56
    retv790 = t791
    return retv790
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__209 *ref_Value_x, value__210 Value) struct{} {
    ref_set__Ref_5Value(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__208 *ref_Value_x) Value {
    var retv795 Value
    var t796 Value = ref_get__Ref_5Value(self__208)
    retv795 = t796
    return retv795
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv798 *_goml_vec_SExpr
    var t799 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv798 = t799
    return retv798
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__207 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv801 *ref_Vec_5SExpr_x
    var t802 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__207)
    retv801 = t802
    return retv801
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__137 *_goml_vec_Token) int {
    var retv804 int
    var t805 int = vec_len__Vec_5Token(self__137)
    retv804 = t805
    return retv804
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__208 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv807 *_goml_vec_SExpr
    var t808 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__208)
    retv807 = t808
    return retv807
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__128 *_goml_vec_SExpr, elem__129 SExpr) *_goml_vec_SExpr {
    var retv810 *_goml_vec_SExpr
    var result__130 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop812:
    for {
        var t813 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t814 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__128)
        var t815 bool = t813 < t814
        if t815 {
            var t816 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t817 SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__128, t816)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__130, t817)
            var t818 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t819 int = t818 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t819)
            continue
        } else {
            break Loop_loop812
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__130, elem__129)
    retv810 = result__130
    return retv810
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__209 *ref_Vec_5SExpr_x, value__210 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv823 string
    var t824 string = _goml_runtime_core_int32_to_string(self__6)
    retv823 = t824
    return retv823
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv826 string
    var t827 string = _goml_runtime_core_bool_to_string(self__37)
    retv826 = t827
    return retv826
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv829 bool
    var t830 bool = self__65 == other__66
    retv829 = t830
    return retv829
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__208 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv832 *_goml_vec_Binding
    var t833 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__208)
    retv832 = t833
    return retv832
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__137 *_goml_vec_SExpr) int {
    var retv835 int
    var t836 int = vec_len__Vec_5SExpr(self__137)
    retv835 = t836
    return retv835
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__128 *_goml_vec_Binding, elem__129 Binding) *_goml_vec_Binding {
    var retv838 *_goml_vec_Binding
    var result__130 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop840:
    for {
        var t841 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t842 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__128)
        var t843 bool = t841 < t842
        if t843 {
            var t844 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t845 Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__128, t844)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__130, t845)
            var t846 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t847 int = t846 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t847)
            continue
        } else {
            break Loop_loop840
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__130, elem__129)
    retv838 = result__130
    return retv838
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__209 *ref_Vec_7Binding_x, value__210 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv851 *_goml_vec_string
    var t852 *_goml_vec_string = vec_new__Vec_6string()
    retv851 = t852
    return retv851
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__207 *_goml_vec_string) *ref_Vec_6string_x {
    var retv854 *ref_Vec_6string_x
    var t855 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__207)
    retv854 = t855
    return retv854
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__208 *ref_Vec_6string_x) *_goml_vec_string {
    var retv857 *_goml_vec_string
    var t858 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__208)
    retv857 = t858
    return retv857
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__128 *_goml_vec_string, elem__129 string) *_goml_vec_string {
    var retv860 *_goml_vec_string
    var result__130 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop862:
    for {
        var t863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t864 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__128)
        var t865 bool = t863 < t864
        if t865 {
            var t866 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t867 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__128, t866)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__130, t867)
            var t868 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t869 int = t868 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t869)
            continue
        } else {
            break Loop_loop862
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__130, elem__129)
    retv860 = result__130
    return retv860
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__209 *ref_Vec_6string_x, value__210 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv873 *_goml_vec_Value
    var t874 *_goml_vec_Value = vec_new__Vec_5Value()
    retv873 = t874
    return retv873
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__207 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv876 *ref_Vec_5Value_x
    var t877 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__207)
    retv876 = t877
    return retv876
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__208 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv879 *_goml_vec_Value
    var t880 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__208)
    retv879 = t880
    return retv879
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__128 *_goml_vec_Value, elem__129 Value) *_goml_vec_Value {
    var retv882 *_goml_vec_Value
    var result__130 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop884:
    for {
        var t885 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t886 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__128)
        var t887 bool = t885 < t886
        if t887 {
            var t888 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t889 Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__128, t888)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__130, t889)
            var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t891 int = t890 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t891)
            continue
        } else {
            break Loop_loop884
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__130, elem__129)
    retv882 = result__130
    return retv882
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__209 *ref_Vec_5Value_x, value__210 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__137 *_goml_vec_Value) int {
    var retv895 int
    var t896 int = vec_len__Vec_5Value(self__137)
    retv895 = t896
    return retv895
}

func _goml_m_trait__impl_i_Eq_i_bool_i_eq(self__53 bool, other__54 bool) bool {
    var retv898 bool
    var t899 bool = self__53 == other__54
    retv898 = t899
    return retv898
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__207 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv901 *ref_Vec_7Binding_x
    var t902 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__207)
    retv901 = t902
    return retv901
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__137 *_goml_vec_string) int {
    var retv904 int
    var t905 int = vec_len__Vec_6string(self__137)
    retv904 = t905
    return retv904
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv907 *_goml_vec_Binding
    var t908 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv907 = t908
    return retv907
}

func println__T_string(value__1 string) struct{} {
    var t910 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t910)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__126 *_goml_vec_Token, elem__127 Token) struct{} {
    vec_push__Vec_5Token(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__132 *_goml_vec_Token, index__133 int) Token {
    var retv915 Token
    var t916 Token = vec_get__Vec_5Token(self__132, index__133)
    retv915 = t916
    return retv915
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__126 *_goml_vec_SExpr, elem__127 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__132 *_goml_vec_SExpr, index__133 int) SExpr {
    var retv920 SExpr
    var t921 SExpr = vec_get__Vec_5SExpr(self__132, index__133)
    retv920 = t921
    return retv920
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__126 *_goml_vec_Binding, elem__127 Binding) struct{} {
    vec_push__Vec_7Binding(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__132 *_goml_vec_Binding, index__133 int) Binding {
    var retv925 Binding
    var t926 Binding = vec_get__Vec_7Binding(self__132, index__133)
    retv925 = t926
    return retv925
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__132 *_goml_vec_string, index__133 int) string {
    var retv930 string
    var t931 string = vec_get__Vec_6string(self__132, index__133)
    retv930 = t931
    return retv930
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__126 *_goml_vec_Value, elem__127 Value) struct{} {
    vec_push__Vec_5Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__132 *_goml_vec_Value, index__133 int) Value {
    var retv935 Value
    var t936 Value = vec_get__Vec_5Value(self__132, index__133)
    retv935 = t936
    return retv935
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv938 string
    retv938 = self__38
    return retv938
}

func main() {
    main0()
}
