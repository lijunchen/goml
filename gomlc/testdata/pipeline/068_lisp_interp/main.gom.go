package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
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

func vec_with_capacity__Vec_5Token(capacity int) *_goml_vec_Token {
    return &_goml_vec_Token{
        items: _goml_slices.Grow([]Token{}, int(capacity)),
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

func vec_with_capacity__Vec_7Binding(capacity int) *_goml_vec_Binding {
    return &_goml_vec_Binding{
        items: _goml_slices.Grow([]Binding{}, int(capacity)),
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

func vec_with_capacity__Vec_5SExpr(capacity int) *_goml_vec_SExpr {
    return &_goml_vec_SExpr{
        items: _goml_slices.Grow([]SExpr{}, int(capacity)),
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

func vec_with_capacity__Vec_5Value(capacity int) *_goml_vec_Value {
    return &_goml_vec_Value{
        items: _goml_slices.Grow([]Value{}, int(capacity)),
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

func vec_with_capacity__Vec_6string(capacity int) *_goml_vec_string {
    return &_goml_vec_string{
        items: _goml_slices.Grow([]string{}, int(capacity)),
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
    var retv292 bool
    var t295 bool = ch__0 >= 48
    var jp294 bool
    if t295 {
        var t296 bool = ch__0 <= 57
        jp294 = t296
    } else {
        jp294 = false
    }
    retv292 = jp294
    return retv292
}

func digit_value(ch__1 rune) int32 {
    var retv298 int32
    var jp300 int32
    switch ch__1 {
    case 48:
        jp300 = 0
    case 49:
        jp300 = 1
    case 50:
        jp300 = 2
    case 51:
        jp300 = 3
    case 52:
        jp300 = 4
    case 53:
        jp300 = 5
    case 54:
        jp300 = 6
    case 55:
        jp300 = 7
    case 56:
        jp300 = 8
    case 57:
        jp300 = 9
    default:
        jp300 = 0
    }
    retv298 = jp300
    return retv298
}

func is_int_text(text__2 string) bool {
    var retv302 bool
    var len__3 int = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t305 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__3, 0)
    var jp304 bool
    if t305 {
        jp304 = false
        retv302 = jp304
        return retv302
    } else {
        var i__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var saw_digit__5 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        var ok__6 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
        var started__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        Loop_loop311:
        for {
            var t330 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
            var jp313 bool
            if t330 {
                var t331 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                var t332 bool = t331 < len__3
                jp313 = t332
            } else {
                jp313 = false
            }
            if jp313 {
                var t314 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                var ch__8 rune = _goml_m_inherent_i_string_i_string_i_get(text__2, t314)
                var t327 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__7)
                var t328 bool = !t327
                var jp317 bool
                if t328 {
                    var t329 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__8, 45)
                    jp317 = t329
                } else {
                    jp317 = false
                }
                if jp317 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                    var t318 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                    var t319 int = t318 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__4, t319)
                } else {
                    var t322 bool = is_digit(ch__8)
                    if t322 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(saw_digit__5, true)
                        var t323 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                        var t324 int = t323 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__4, t324)
                    } else {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop311
            }
        }
        var t309 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
        var jp308 bool
        if t309 {
            var t310 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(saw_digit__5)
            jp308 = t310
        } else {
            jp308 = false
        }
        jp304 = jp308
        retv302 = jp304
        return retv302
    }
}

func parse_int32(text__9 string) int32 {
    var retv334 int32
    var len__10 int = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop342:
    for {
        var t343 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
        var t344 bool = t343 < len__10
        if t344 {
            var t345 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
            var ch__15 rune = _goml_m_inherent_i_string_i_string_i_get(text__9, t345)
            var t358 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__13)
            var t359 bool = !t358
            var jp348 bool
            if t359 {
                var t360 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__15, 45)
                jp348 = t360
            } else {
                jp348 = false
            }
            if jp348 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(negative__12, true)
                var t349 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
                var t350 int = t349 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__11, t350)
            } else {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t352 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
                var t353 int32 = t352 * 10
                var t354 int32 = t353 + d__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__14, t354)
                var t355 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
                var t356 int = t355 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__11, t356)
            }
            continue
        } else {
            break Loop_loop342
        }
    }
    var t338 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(negative__12)
    var jp337 int32
    if t338 {
        var t339 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        var t340 int32 = 0 - t339
        jp337 = t340
    } else {
        var t341 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        jp337 = t341
    }
    retv334 = jp337
    return retv334
}

func is_delim(ch__17 rune) bool {
    var retv362 bool
    var t368 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 40)
    var jp366 bool
    if t368 {
        jp366 = true
    } else {
        var t369 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 41)
        jp366 = t369
    }
    var jp364 bool
    if jp366 {
        jp364 = true
    } else {
        var t367 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 32)
        jp364 = t367
    }
    retv362 = jp364
    return retv362
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var retv371 Tuple2_5Token_3int
    var len__20 int = _goml_m_inherent_i_string_i_string_i_len(source__18)
    var text__21 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var i__22 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__19)
    var done__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop385:
    for {
        var t398 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__23)
        var t399 bool = !t398
        var jp387 bool
        if t399 {
            var t400 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
            var t401 bool = t400 < len__20
            jp387 = t401
        } else {
            jp387 = false
        }
        if jp387 {
            var t388 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
            var ch__24 rune = _goml_m_inherent_i_string_i_string_i_get(source__18, t388)
            var t390 bool = is_delim(ch__24)
            if t390 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__23, true)
            } else {
                var t392 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
                var t393 string = _goml_m_inherent_i_char_i_char_i_to__string(ch__24)
                var t394 string = t392 + t393
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(text__21, t394)
                var t395 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
                var t396 int = t395 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__22, t396)
            }
            continue
        } else {
            break Loop_loop385
        }
    }
    var atom__25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
    var jp374 Token
    switch atom__25 {
    case "true":
        var t377 Token = Token_Bool{
            _0: true,
        }
        jp374 = t377
    case "false":
        var t378 Token = Token_Bool{
            _0: false,
        }
        jp374 = t378
    default:
        var t381 bool = is_int_text(atom__25)
        var jp380 Token
        if t381 {
            var t382 int32 = parse_int32(atom__25)
            var t383 Token = Token_Int{
                _0: t382,
            }
            jp380 = t383
        } else {
            var t384 Token = Token_Sym{
                _0: atom__25,
            }
            jp380 = t384
        }
        jp374 = jp380
    }
    var token__26 Token = jp374
    var t375 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
    var t376 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: token__26,
        _1: t375,
    }
    retv371 = t376
    return retv371
}

func lex(source__27 string) *_goml_vec_Token {
    var retv403 *_goml_vec_Token
    var len__28 int = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var toks__30 *ref_Vec_5Token_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(toks0__29)
    var i__31 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop406:
    for {
        var t407 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
        var t408 bool = t407 < len__28
        if t408 {
            var t409 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
            var ch__32 rune = _goml_m_inherent_i_string_i_string_i_get(source__27, t409)
            var t411 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 40)
            if t411 {
                var t412 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t413 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t412, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t413)
                var t414 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                var t415 int = t414 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t415)
            } else {
                var t418 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 41)
                if t418 {
                    var t419 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t420 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t419, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t420)
                    var t421 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                    var t422 int = t421 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t422)
                } else {
                    var t425 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 32)
                    if t425 {
                        var t426 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                        var t427 int = t426 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t427)
                    } else {
                        var t429 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                        var mtmp168 Tuple2_5Token_3int = lex_atom(source__27, t429)
                        var x169 Token = mtmp168._0
                        var x170 int = mtmp168._1
                        var next__34 int = x170
                        var tok__33 Token = x169
                        var t430 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                        var t431 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t430, tok__33)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t431)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop406
        }
    }
    var t405 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
    retv403 = t405
    return retv403
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var retv434 Value
    var t435 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(env__35)
    var t436 int = t435 - 1
    var i__37 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t436)
    var result__38 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    var done__39 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop439:
    for {
        var t451 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__39)
        var t452 bool = !t451
        var jp441 bool
        if t452 {
            var t453 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
            var t454 bool = t453 >= 0
            jp441 = t454
        } else {
            jp441 = false
        }
        if jp441 {
            var t442 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t442)
            var t444 string = binding__40.name
            var t445 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t444, name__36)
            if t445 {
                var t446 Value = binding__40.value
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(result__38, t446)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__39, true)
            } else {
                var t448 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
                var t449 int = t448 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__37, t449)
            }
            continue
        } else {
            break Loop_loop439
        }
    }
    var t438 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(result__38)
    retv434 = t438
    return retv434
}

func lookup(local__41 *_goml_vec_Binding, global__42 *_goml_vec_Binding, name__43 string) Value {
    var retv456 Value
    var mtmp175 Value = env_lookup(local__41, name__43)
    var jp458 Value
    switch mtmp175.(type) {
    case Nil:
        var t459 Value = env_lookup(global__42, name__43)
        jp458 = t459
    default:
        var other__44 Value = mtmp175
        jp458 = other__44
    }
    retv456 = jp458
    return retv456
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var retv461 Tuple2_10Vec_5SExpr_3int
    var acc__47 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__47)
    var i__49 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__46)
    var done__50 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop466:
    for {
        var t478 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__50)
        var t479 bool = !t478
        var jp468 bool
        if t479 {
            var t480 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
            var t481 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__45)
            var t482 bool = t480 < t481
            jp468 = t482
        } else {
            jp468 = false
        }
        if jp468 {
            var t469 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
            var mtmp179 Token = vec_get__Vec_5Token(tokens__45, t469)
            switch mtmp179.(type) {
            case RParen:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__50, true)
                var t471 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
                var t472 int = t471 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__49, t472)
            default:
                var t474 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
                var mtmp184 Tuple2_5SExpr_3int = parse_expr(tokens__45, t474)
                var x185 SExpr = mtmp184._0
                var x186 int = mtmp184._1
                var next__52 int = x186
                var expr__51 SExpr = x185
                var t475 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t476 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t475, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t476)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__49, next__52)
            }
            continue
        } else {
            break Loop_loop466
        }
    }
    var t463 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
    var t464 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
    var t465 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t463,
        _1: t464,
    }
    retv461 = t465
    return retv461
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var retv484 Tuple2_5SExpr_3int
    var mtmp189 Token = vec_get__Vec_5Token(tokens__53, start__54)
    var jp486 Tuple2_5SExpr_3int
    switch mtmp189.(type) {
    case LParen:
        var t487 int = start__54 + 1
        var mtmp193 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t487)
        var x194 *_goml_vec_SExpr = mtmp193._0
        var x195 int = mtmp193._1
        var next__56 int = x195
        var items__55 *_goml_vec_SExpr = x194
        var t488 SExpr = List{
            _0: items__55,
        }
        var t489 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t488,
            _1: next__56,
        }
        jp486 = t489
    case RParen:
        var t490 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t491 int = start__54 + 1
        var t492 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t490,
            _1: t491,
        }
        jp486 = t492
    case Token_Sym:
        var x190 string = mtmp189.(Token_Sym)._0
        var name__59 string = x190
        var t493 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t494 int = start__54 + 1
        var t495 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t493,
            _1: t494,
        }
        jp486 = t495
    case Token_Int:
        var x191 int32 = mtmp189.(Token_Int)._0
        var n__58 int32 = x191
        var t496 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t497 int = start__54 + 1
        var t498 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t496,
            _1: t497,
        }
        jp486 = t498
    case Token_Bool:
        var x192 bool = mtmp189.(Token_Bool)._0
        var b__57 bool = x192
        var t499 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t500 int = start__54 + 1
        var t501 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t499,
            _1: t500,
        }
        jp486 = t501
    default:
        panic("non-exhaustive match")
    }
    retv484 = jp486
    return retv484
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var retv503 *_goml_vec_SExpr
    var i__61 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var acc__62 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__62)
    Loop_loop506:
    for {
        var t507 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__61)
        var t508 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__60)
        var t509 bool = t507 < t508
        if t509 {
            var t510 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__61)
            var mtmp196 Tuple2_5SExpr_3int = parse_expr(tokens__60, t510)
            var x197 SExpr = mtmp196._0
            var x198 int = mtmp196._1
            var next__65 int = x198
            var expr__64 SExpr = x197
            var t511 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
            var t512 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t511, expr__64)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__63, t512)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__61, next__65)
            continue
        } else {
            break Loop_loop506
        }
    }
    var t505 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
    retv503 = t505
    return retv503
}

func value_to_string(value__66 Value) string {
    var retv515 string
    var jp517 string
    switch value__66.(type) {
    case Value_Int:
        var x201 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x201
        var t518 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__67)
        jp517 = t518
    case Value_Bool:
        var x202 bool = value__66.(Value_Bool)._0
        var b__68 bool = x202
        var t519 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(b__68)
        jp517 = t519
    case Func:
        jp517 = "<lambda>"
    case Nil:
        jp517 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv515 = jp517
    return retv515
}

func truthy(value__69 Value) bool {
    var retv521 bool
    var jp523 bool
    switch value__69.(type) {
    case Value_Int:
        var x204 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x204
        var t524 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(n__71, 0)
        var t525 bool = !t524
        jp523 = t525
    case Value_Bool:
        var x205 bool = value__69.(Value_Bool)._0
        var b__70 bool = x205
        jp523 = b__70
    case Func:
        jp523 = true
    case Nil:
        jp523 = false
    default:
        panic("non-exhaustive match")
    }
    retv521 = jp523
    return retv521
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv527 Value
    var jp529 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x207 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x207
        var t530 Value = Value_Int{
            _0: n__75,
        }
        jp529 = t530
    case SExpr_Bool:
        var x208 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x208
        var t531 Value = Value_Bool{
            _0: b__76,
        }
        jp529 = t531
    case SExpr_Sym:
        var x209 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x209
        var t532 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t533 Value = lookup(local__73, t532, name__77)
        jp529 = t533
    case List:
        var x210 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x210
        var t534 Value = eval_list(items__78, local__73, global__74)
        jp529 = t534
    default:
        panic("non-exhaustive match")
    }
    retv527 = jp529
    return retv527
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv536 Value
    var t539 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t540 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t539, 0)
    var jp538 Value
    if t540 {
        jp538 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp542 Value
        switch head__82.(type) {
        case SExpr_Sym:
            var x213 string = head__82.(SExpr_Sym)._0
            var name__83 string = x213
            var t543 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp542 = t543
        default:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t544 Value = apply(f__84, args__85, global__81)
            jp542 = t544
        }
        jp538 = jp542
    }
    retv536 = jp538
    return retv536
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv546 Value
    var jp548 Value
    switch name__86 {
    case "begin":
        var t549 Value = eval_begin(items__87, 1, local__88, global__89)
        jp548 = t549
    case "define":
        var t552 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t553 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t552, 3)
        var jp551 Value
        if t553 {
            var mtmp215 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp555 Value
            switch mtmp215.(type) {
            case SExpr_Sym:
                var x218 string = mtmp215.(SExpr_Sym)._0
                var var__90 string = x218
                var t556 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t556, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t557 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t557)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp555 = value__91
            default:
                jp555 = Nil{}
            }
            jp551 = jp555
        } else {
            jp551 = Nil{}
        }
        jp548 = jp551
    case "if":
        var t560 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t561 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t560, 4)
        var jp559 Value
        if t561 {
            var t562 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t562, local__88, global__89)
            var t565 bool = truthy(cond__94)
            var jp564 Value
            if t565 {
                var t566 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t567 Value = eval(t566, local__88, global__89)
                jp564 = t567
            } else {
                var t568 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t569 Value = eval(t568, local__88, global__89)
                jp564 = t569
            }
            jp559 = jp564
        } else {
            jp559 = Nil{}
        }
        jp548 = jp559
    case "lambda":
        var t572 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t573 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t572, 3)
        var jp571 Value
        if t573 {
            var mtmp221 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp575 Value
            switch mtmp221.(type) {
            case List:
                var x225 *_goml_vec_SExpr = mtmp221.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x225
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t576 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t577 Value = Func{
                    _0: t576,
                }
                jp575 = t577
            default:
                jp575 = Nil{}
            }
            jp571 = jp575
        } else {
            jp571 = Nil{}
        }
        jp548 = jp571
    case "+":
        var t578 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t579 Value = apply_builtin("+", t578)
        jp548 = t579
    case "-":
        var t580 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t581 Value = apply_builtin("-", t580)
        jp548 = t581
    case "*":
        var t582 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t583 Value = apply_builtin("*", t582)
        jp548 = t583
    case "/":
        var t584 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t585 Value = apply_builtin("/", t584)
        jp548 = t585
    case "=":
        var t586 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t587 Value = apply_builtin("=", t586)
        jp548 = t587
    default:
        var t588 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t588, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t589 Value = apply(f__98, args__99, global__89)
        jp548 = t589
    }
    retv546 = jp548
    return retv546
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv591 Value
    var i__104 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop594:
    for {
        var t595 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
        var t596 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t597 bool = t595 < t596
        if t597 {
            var t598 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
            var t599 SExpr = vec_get__Vec_5SExpr(items__100, t598)
            var v__106 Value = eval(t599, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t600 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
            var t601 int = t600 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__104, t601)
            continue
        } else {
            break Loop_loop594
        }
    }
    var t593 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv591 = t593
    return retv591
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv604 *_goml_vec_string
    var i__108 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop607:
    for {
        var t608 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
        var t609 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t610 bool = t608 < t609
        if t610 {
            var t611 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
            var mtmp228 SExpr = vec_get__Vec_5SExpr(items__107, t611)
            switch mtmp228.(type) {
            case SExpr_Sym:
                var x231 string = mtmp228.(SExpr_Sym)._0
                var name__111 string = x231
                var t613 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t614 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t613, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t614)
                var t615 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
                var t616 int = t615 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__108, t616)
            default:
                var t618 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
                var t619 int = t618 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__108, t619)
            }
            continue
        } else {
            break Loop_loop607
        }
    }
    var t606 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv604 = t606
    return retv604
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv622 *_goml_vec_Value
    var i__116 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop625:
    for {
        var t626 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
        var t627 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t628 bool = t626 < t627
        if t628 {
            var t629 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
            var t630 SExpr = vec_get__Vec_5SExpr(items__112, t629)
            var v__119 Value = eval(t630, local__114, global__115)
            var t631 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t632 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t631, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t632)
            var t633 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
            var t634 int = t633 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__116, t634)
            continue
        } else {
            break Loop_loop625
        }
    }
    var t624 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv622 = t624
    return retv622
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv637 Value
    var jp639 Value
    switch name__120 {
    case "=":
        var t642 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t643 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t642, 2)
        var jp641 Value
        if t643 {
            var t644 Value = vec_get__Vec_5Value(args__121, 0)
            var t645 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp237 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t644,
                _1: t645,
            }
            var x238 Value = mtmp237._0
            var x239 Value = mtmp237._1
            var jp647 Value
            switch x239.(type) {
            case Value_Int:
                var x240 int32 = x239.(Value_Int)._0
                var jp649 Value
                switch x238.(type) {
                case Value_Int:
                    var x243 int32 = x238.(Value_Int)._0
                    var a__122 int32 = x243
                    var b__123 int32 = x240
                    var t650 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__122, b__123)
                    var t651 Value = Value_Bool{
                        _0: t650,
                    }
                    jp649 = t651
                default:
                    var t652 Value = Value_Bool{
                        _0: false,
                    }
                    jp649 = t652
                }
                jp647 = jp649
            case Value_Bool:
                var x241 bool = x239.(Value_Bool)._0
                var jp654 Value
                switch x238.(type) {
                case Value_Bool:
                    var x247 bool = x238.(Value_Bool)._0
                    var a__124 bool = x247
                    var b__125 bool = x241
                    var t655 bool = _goml_m_trait__impl_i_Eq_i_bool_i_eq(a__124, b__125)
                    var t656 Value = Value_Bool{
                        _0: t655,
                    }
                    jp654 = t656
                default:
                    var t657 Value = Value_Bool{
                        _0: false,
                    }
                    jp654 = t657
                }
                jp647 = jp654
            default:
                var t658 Value = Value_Bool{
                    _0: false,
                }
                jp647 = t658
            }
            jp641 = jp647
        } else {
            var t659 Value = Value_Bool{
                _0: false,
            }
            jp641 = t659
        }
        jp639 = jp641
        retv637 = jp639
        return retv637
    case "+":
        var i__126 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop663:
        for {
            var t664 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
            var t665 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t666 bool = t664 < t665
            if t666 {
                var t667 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                var mtmp249 Value = vec_get__Vec_5Value(args__121, t667)
                switch mtmp249.(type) {
                case Value_Int:
                    var x250 int32 = mtmp249.(Value_Int)._0
                    var n__128 int32 = x250
                    var t669 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t670 int32 = t669 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t670)
                    var t671 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                    var t672 int = t671 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__126, t672)
                default:
                    var t674 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                    var t675 int = t674 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__126, t675)
                }
                continue
            } else {
                break Loop_loop663
            }
        }
        var t661 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t662 Value = Value_Int{
            _0: t661,
        }
        jp639 = t662
        retv637 = jp639
        return retv637
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop680:
        for {
            var t681 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
            var t682 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t683 bool = t681 < t682
            if t683 {
                var t684 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                var mtmp255 Value = vec_get__Vec_5Value(args__121, t684)
                switch mtmp255.(type) {
                case Value_Int:
                    var x256 int32 = mtmp255.(Value_Int)._0
                    var n__131 int32 = x256
                    var t686 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t687 int32 = t686 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t687)
                    var t688 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                    var t689 int = t688 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__129, t689)
                default:
                    var t691 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                    var t692 int = t691 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__129, t692)
                }
                continue
            } else {
                break Loop_loop680
            }
        }
        var t678 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t679 Value = Value_Int{
            _0: t678,
        }
        jp639 = t679
        retv637 = jp639
        return retv637
    case "-":
        var mtmp261 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp695 Value
        switch mtmp261 {
        case 1:
            var mtmp262 Value = vec_get__Vec_5Value(args__121, 0)
            var jp697 Value
            switch mtmp262.(type) {
            case Value_Int:
                var x263 int32 = mtmp262.(Value_Int)._0
                var n__132 int32 = x263
                var t698 int32 = 0 - n__132
                var t699 Value = Value_Int{
                    _0: t698,
                }
                jp697 = t699
            default:
                jp697 = Nil{}
            }
            jp695 = jp697
        case 2:
            var t700 Value = vec_get__Vec_5Value(args__121, 0)
            var t701 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp266 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t700,
                _1: t701,
            }
            var x267 Value = mtmp266._0
            var x268 Value = mtmp266._1
            var jp703 Value
            switch x268.(type) {
            case Value_Int:
                var x269 int32 = x268.(Value_Int)._0
                var jp705 Value
                switch x267.(type) {
                case Value_Int:
                    var x272 int32 = x267.(Value_Int)._0
                    var a__133 int32 = x272
                    var b__134 int32 = x269
                    var t706 int32 = a__133 - b__134
                    var t707 Value = Value_Int{
                        _0: t706,
                    }
                    jp705 = t707
                default:
                    jp705 = Nil{}
                }
                jp703 = jp705
            default:
                jp703 = Nil{}
            }
            jp695 = jp703
        default:
            jp695 = Nil{}
        }
        jp639 = jp695
        retv637 = jp639
        return retv637
    case "/":
        var t710 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t711 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t710, 2)
        var jp709 Value
        if t711 {
            var t712 Value = vec_get__Vec_5Value(args__121, 0)
            var t713 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp275 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t712,
                _1: t713,
            }
            var x276 Value = mtmp275._0
            var x277 Value = mtmp275._1
            var jp715 Value
            switch x277.(type) {
            case Value_Int:
                var x278 int32 = x277.(Value_Int)._0
                var jp717 Value
                switch x276.(type) {
                case Value_Int:
                    var x281 int32 = x276.(Value_Int)._0
                    var a__135 int32 = x281
                    var b__136 int32 = x278
                    var t718 int32 = a__135 / b__136
                    var t719 Value = Value_Int{
                        _0: t718,
                    }
                    jp717 = t719
                default:
                    jp717 = Nil{}
                }
                jp715 = jp717
            default:
                jp715 = Nil{}
            }
            jp709 = jp715
        } else {
            jp709 = Nil{}
        }
        jp639 = jp709
        retv637 = jp639
        return retv637
    default:
        jp639 = Nil{}
        retv637 = jp639
        return retv637
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv721 Value
    var jp723 Value
    switch func__137.(type) {
    case Func:
        var x286 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x286
        var t724 Value = apply_lambda(fun__140, args__138)
        jp723 = t724
    default:
        jp723 = Nil{}
    }
    retv721 = jp723
    return retv721
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv726 Value
    var t727 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t727)
    var i__144 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop733:
    for {
        var t744 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
        var t745 *_goml_vec_string = lambda__141.params
        var t746 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t745)
        var t747 bool = t744 < t746
        var jp735 bool
        if t747 {
            var t748 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var t749 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t750 bool = t748 < t749
            jp735 = t750
        } else {
            jp735 = false
        }
        if jp735 {
            var t736 *_goml_vec_string = lambda__141.params
            var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var name__145 string = vec_get__Vec_6string(t736, t737)
            var t738 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t738)
            var t739 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t740 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t739, t740)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t741 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var t742 int = t741 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__144, t742)
            continue
        } else {
            break Loop_loop733
        }
    }
    var t729 SExpr = lambda__141.body
    var t730 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t731 *ref_Vec_7Binding_x = lambda__141.global
    var t732 Value = eval(t729, t730, t731)
    retv726 = t732
    return retv726
}

func main0() struct{} {
    var t752 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t752)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t753 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t753)
    var t754 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t755 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t754, t755, global__148)
    var t756 string = value_to_string(result__151)
    println__T_string(t756)
    var t757 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t757)
    var t758 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t759 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t758, t759, global__148)
    var t760 string = value_to_string(result2__153)
    println__T_string(t760)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv762 int
    var t763 int = _goml_runtime_core_string_len(self__8)
    retv762 = t763
    return retv762
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv765 bool
    var t766 bool = self__59 == other__60
    retv765 = t766
    return retv765
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv768 *ref_int_x
    var t769 *ref_int_x = ref__Ref_3int(value__207)
    retv768 = t769
    return retv768
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv771 *ref_bool_x
    var t772 *ref_bool_x = ref__Ref_4bool(value__207)
    retv771 = t772
    return retv771
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv774 bool
    var t775 bool = ref_get__Ref_4bool(self__208)
    retv774 = t775
    return retv774
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv777 int
    var t778 int = ref_get__Ref_3int(self__208)
    retv777 = t778
    return retv777
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv780 rune
    var t781 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv780 = t781
    return retv780
}

func _goml_m_trait__impl_i_Eq_i_char_i_eq(self__57 rune, other__58 rune) bool {
    var retv783 bool
    var t784 bool = self__57 == other__58
    retv783 = t784
    return retv783
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
    var retv790 *ref_int32_x
    var t791 *ref_int32_x = ref__Ref_5int32(value__207)
    retv790 = t791
    return retv790
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv793 int32
    var t794 int32 = ref_get__Ref_5int32(self__208)
    retv793 = t794
    return retv793
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__207 string) *ref_string_x {
    var retv798 *ref_string_x
    var t799 *ref_string_x = ref__Ref_6string(value__207)
    retv798 = t799
    return retv798
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__208 *ref_string_x) string {
    var retv801 string
    var t802 string = ref_get__Ref_6string(self__208)
    retv801 = t802
    return retv801
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv804 string
    var t805 string = _goml_runtime_core_char_to_string(self__7)
    retv804 = t805
    return retv804
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__209 *ref_string_x, value__210 string) struct{} {
    ref_set__Ref_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv809 *_goml_vec_Token
    var t810 *_goml_vec_Token = vec_new__Vec_5Token()
    retv809 = t810
    return retv809
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__207 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv812 *ref_Vec_5Token_x
    var t813 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__207)
    retv812 = t813
    return retv812
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__208 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv815 *_goml_vec_Token
    var t816 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__208)
    retv815 = t816
    return retv815
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__128 *_goml_vec_Token, elem__129 Token) *_goml_vec_Token {
    var retv818 *_goml_vec_Token
    var t819 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__128)
    var t820 int = t819 + 1
    var result__130 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Token(t820)
    var index__131 int = 0
    Loop_loop822:
    for {
        var t823 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__128)
        var t824 bool = index__131 < t823
        if t824 {
            var t825 Token = vec_get__Vec_5Token(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__130, t825)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t826 int = compound_old38 + compound_value39
            index__131 = t826
            continue
        } else {
            break Loop_loop822
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__130, elem__129)
    retv818 = result__130
    return retv818
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__209 *ref_Vec_5Token_x, value__210 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__137 *_goml_vec_Binding) int {
    var retv831 int
    var t832 int = vec_len__Vec_7Binding(self__137)
    retv831 = t832
    return retv831
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__207 Value) *ref_Value_x {
    var retv834 *ref_Value_x
    var t835 *ref_Value_x = ref__Ref_5Value(value__207)
    retv834 = t835
    return retv834
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv837 bool
    var t838 bool = self__55 == other__56
    retv837 = t838
    return retv837
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__209 *ref_Value_x, value__210 Value) struct{} {
    ref_set__Ref_5Value(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__208 *ref_Value_x) Value {
    var retv842 Value
    var t843 Value = ref_get__Ref_5Value(self__208)
    retv842 = t843
    return retv842
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv845 *_goml_vec_SExpr
    var t846 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv845 = t846
    return retv845
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__207 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv848 *ref_Vec_5SExpr_x
    var t849 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__207)
    retv848 = t849
    return retv848
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__137 *_goml_vec_Token) int {
    var retv851 int
    var t852 int = vec_len__Vec_5Token(self__137)
    retv851 = t852
    return retv851
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__208 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv854 *_goml_vec_SExpr
    var t855 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__208)
    retv854 = t855
    return retv854
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__128 *_goml_vec_SExpr, elem__129 SExpr) *_goml_vec_SExpr {
    var retv857 *_goml_vec_SExpr
    var t858 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__128)
    var t859 int = t858 + 1
    var result__130 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__SExpr(t859)
    var index__131 int = 0
    Loop_loop861:
    for {
        var t862 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__128)
        var t863 bool = index__131 < t862
        if t863 {
            var t864 SExpr = vec_get__Vec_5SExpr(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__130, t864)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t865 int = compound_old38 + compound_value39
            index__131 = t865
            continue
        } else {
            break Loop_loop861
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__130, elem__129)
    retv857 = result__130
    return retv857
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__209 *ref_Vec_5SExpr_x, value__210 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv870 string
    var t871 string = _goml_runtime_core_int32_to_string(self__6)
    retv870 = t871
    return retv870
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv873 string
    var t874 string = _goml_runtime_core_bool_to_string(self__37)
    retv873 = t874
    return retv873
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv876 bool
    var t877 bool = self__65 == other__66
    retv876 = t877
    return retv876
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__208 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv879 *_goml_vec_Binding
    var t880 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__208)
    retv879 = t880
    return retv879
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__137 *_goml_vec_SExpr) int {
    var retv882 int
    var t883 int = vec_len__Vec_5SExpr(self__137)
    retv882 = t883
    return retv882
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__128 *_goml_vec_Binding, elem__129 Binding) *_goml_vec_Binding {
    var retv885 *_goml_vec_Binding
    var t886 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__128)
    var t887 int = t886 + 1
    var result__130 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Binding(t887)
    var index__131 int = 0
    Loop_loop889:
    for {
        var t890 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__128)
        var t891 bool = index__131 < t890
        if t891 {
            var t892 Binding = vec_get__Vec_7Binding(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__130, t892)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t893 int = compound_old38 + compound_value39
            index__131 = t893
            continue
        } else {
            break Loop_loop889
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__130, elem__129)
    retv885 = result__130
    return retv885
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__209 *ref_Vec_7Binding_x, value__210 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv898 *_goml_vec_string
    var t899 *_goml_vec_string = vec_new__Vec_6string()
    retv898 = t899
    return retv898
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__207 *_goml_vec_string) *ref_Vec_6string_x {
    var retv901 *ref_Vec_6string_x
    var t902 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__207)
    retv901 = t902
    return retv901
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__208 *ref_Vec_6string_x) *_goml_vec_string {
    var retv904 *_goml_vec_string
    var t905 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__208)
    retv904 = t905
    return retv904
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__128 *_goml_vec_string, elem__129 string) *_goml_vec_string {
    var retv907 *_goml_vec_string
    var t908 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__128)
    var t909 int = t908 + 1
    var result__130 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(t909)
    var index__131 int = 0
    Loop_loop911:
    for {
        var t912 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__128)
        var t913 bool = index__131 < t912
        if t913 {
            var t914 string = vec_get__Vec_6string(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__130, t914)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t915 int = compound_old38 + compound_value39
            index__131 = t915
            continue
        } else {
            break Loop_loop911
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__130, elem__129)
    retv907 = result__130
    return retv907
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__209 *ref_Vec_6string_x, value__210 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv920 *_goml_vec_Value
    var t921 *_goml_vec_Value = vec_new__Vec_5Value()
    retv920 = t921
    return retv920
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__207 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv923 *ref_Vec_5Value_x
    var t924 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__207)
    retv923 = t924
    return retv923
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__208 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv926 *_goml_vec_Value
    var t927 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__208)
    retv926 = t927
    return retv926
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__128 *_goml_vec_Value, elem__129 Value) *_goml_vec_Value {
    var retv929 *_goml_vec_Value
    var t930 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__128)
    var t931 int = t930 + 1
    var result__130 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Value(t931)
    var index__131 int = 0
    Loop_loop933:
    for {
        var t934 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__128)
        var t935 bool = index__131 < t934
        if t935 {
            var t936 Value = vec_get__Vec_5Value(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__130, t936)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t937 int = compound_old38 + compound_value39
            index__131 = t937
            continue
        } else {
            break Loop_loop933
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__130, elem__129)
    retv929 = result__130
    return retv929
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__209 *ref_Vec_5Value_x, value__210 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__137 *_goml_vec_Value) int {
    var retv942 int
    var t943 int = vec_len__Vec_5Value(self__137)
    retv942 = t943
    return retv942
}

func _goml_m_trait__impl_i_Eq_i_bool_i_eq(self__53 bool, other__54 bool) bool {
    var retv945 bool
    var t946 bool = self__53 == other__54
    retv945 = t946
    return retv945
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__207 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv948 *ref_Vec_7Binding_x
    var t949 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__207)
    retv948 = t949
    return retv948
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__137 *_goml_vec_string) int {
    var retv951 int
    var t952 int = vec_len__Vec_6string(self__137)
    retv951 = t952
    return retv951
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv954 *_goml_vec_Binding
    var t955 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv954 = t955
    return retv954
}

func println__T_string(value__1 string) struct{} {
    var t957 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t957)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Token(capacity__125 int) *_goml_vec_Token {
    var retv960 *_goml_vec_Token
    var t961 *_goml_vec_Token = vec_with_capacity__Vec_5Token(capacity__125)
    retv960 = t961
    return retv960
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__126 *_goml_vec_Token, elem__127 Token) struct{} {
    vec_push__Vec_5Token(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__SExpr(capacity__125 int) *_goml_vec_SExpr {
    var retv965 *_goml_vec_SExpr
    var t966 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(capacity__125)
    retv965 = t966
    return retv965
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__126 *_goml_vec_SExpr, elem__127 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Binding(capacity__125 int) *_goml_vec_Binding {
    var retv970 *_goml_vec_Binding
    var t971 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(capacity__125)
    retv970 = t971
    return retv970
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__126 *_goml_vec_Binding, elem__127 Binding) struct{} {
    vec_push__Vec_7Binding(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__125 int) *_goml_vec_string {
    var retv975 *_goml_vec_string
    var t976 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__125)
    retv975 = t976
    return retv975
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Value(capacity__125 int) *_goml_vec_Value {
    var retv980 *_goml_vec_Value
    var t981 *_goml_vec_Value = vec_with_capacity__Vec_5Value(capacity__125)
    retv980 = t981
    return retv980
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__126 *_goml_vec_Value, elem__127 Value) struct{} {
    vec_push__Vec_5Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv985 string
    retv985 = self__38
    return retv985
}

func main() {
    main0()
}
