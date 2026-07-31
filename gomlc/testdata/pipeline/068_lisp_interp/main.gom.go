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
    var retv289 bool
    var t292 bool = ch__0 >= 48
    var jp291 bool
    if t292 {
        var t293 bool = ch__0 <= 57
        jp291 = t293
    } else {
        jp291 = false
    }
    retv289 = jp291
    return retv289
}

func digit_value(ch__1 rune) int32 {
    var retv295 int32
    var jp297 int32
    switch ch__1 {
    case 48:
        jp297 = 0
    case 49:
        jp297 = 1
    case 50:
        jp297 = 2
    case 51:
        jp297 = 3
    case 52:
        jp297 = 4
    case 53:
        jp297 = 5
    case 54:
        jp297 = 6
    case 55:
        jp297 = 7
    case 56:
        jp297 = 8
    case 57:
        jp297 = 9
    default:
        jp297 = 0
    }
    retv295 = jp297
    return retv295
}

func is_int_text(text__2 string) bool {
    var retv299 bool
    var len__3 int = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t302 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__3, 0)
    var jp301 bool
    if t302 {
        jp301 = false
        retv299 = jp301
        return retv299
    } else {
        var i__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var saw_digit__5 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        var ok__6 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
        var started__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        Loop_loop308:
        for {
            var t327 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
            var jp310 bool
            if t327 {
                var t328 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                var t329 bool = t328 < len__3
                jp310 = t329
            } else {
                jp310 = false
            }
            if jp310 {
                var t311 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                var ch__8 rune = _goml_m_inherent_i_string_i_string_i_get(text__2, t311)
                var t324 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__7)
                var t325 bool = !t324
                var jp314 bool
                if t325 {
                    var t326 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__8, 45)
                    jp314 = t326
                } else {
                    jp314 = false
                }
                if jp314 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                    var t315 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                    var t316 int = t315 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__4, t316)
                } else {
                    var t319 bool = is_digit(ch__8)
                    if t319 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(saw_digit__5, true)
                        var t320 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__4)
                        var t321 int = t320 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__4, t321)
                    } else {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop308
            }
        }
        var t306 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
        var jp305 bool
        if t306 {
            var t307 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(saw_digit__5)
            jp305 = t307
        } else {
            jp305 = false
        }
        jp301 = jp305
        retv299 = jp301
        return retv299
    }
}

func parse_int32(text__9 string) int32 {
    var retv331 int32
    var len__10 int = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop339:
    for {
        var t340 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
        var t341 bool = t340 < len__10
        if t341 {
            var t342 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
            var ch__15 rune = _goml_m_inherent_i_string_i_string_i_get(text__9, t342)
            var t355 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__13)
            var t356 bool = !t355
            var jp345 bool
            if t356 {
                var t357 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__15, 45)
                jp345 = t357
            } else {
                jp345 = false
            }
            if jp345 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(negative__12, true)
                var t346 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
                var t347 int = t346 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__11, t347)
            } else {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t349 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
                var t350 int32 = t349 * 10
                var t351 int32 = t350 + d__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__14, t351)
                var t352 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__11)
                var t353 int = t352 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__11, t353)
            }
            continue
        } else {
            break Loop_loop339
        }
    }
    var t335 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(negative__12)
    var jp334 int32
    if t335 {
        var t336 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        var t337 int32 = 0 - t336
        jp334 = t337
    } else {
        var t338 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        jp334 = t338
    }
    retv331 = jp334
    return retv331
}

func is_delim(ch__17 rune) bool {
    var retv359 bool
    var t365 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 40)
    var jp363 bool
    if t365 {
        jp363 = true
    } else {
        var t366 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 41)
        jp363 = t366
    }
    var jp361 bool
    if jp363 {
        jp361 = true
    } else {
        var t364 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 32)
        jp361 = t364
    }
    retv359 = jp361
    return retv359
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var retv368 Tuple2_5Token_3int
    var len__20 int = _goml_m_inherent_i_string_i_string_i_len(source__18)
    var text__21 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var i__22 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__19)
    var done__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop382:
    for {
        var t395 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__23)
        var t396 bool = !t395
        var jp384 bool
        if t396 {
            var t397 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
            var t398 bool = t397 < len__20
            jp384 = t398
        } else {
            jp384 = false
        }
        if jp384 {
            var t385 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
            var ch__24 rune = _goml_m_inherent_i_string_i_string_i_get(source__18, t385)
            var t387 bool = is_delim(ch__24)
            if t387 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__23, true)
            } else {
                var t389 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
                var t390 string = _goml_m_inherent_i_char_i_char_i_to__string(ch__24)
                var t391 string = t389 + t390
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(text__21, t391)
                var t392 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
                var t393 int = t392 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__22, t393)
            }
            continue
        } else {
            break Loop_loop382
        }
    }
    var atom__25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
    var jp371 Token
    switch atom__25 {
    case "true":
        var t374 Token = Token_Bool{
            _0: true,
        }
        jp371 = t374
    case "false":
        var t375 Token = Token_Bool{
            _0: false,
        }
        jp371 = t375
    default:
        var t378 bool = is_int_text(atom__25)
        var jp377 Token
        if t378 {
            var t379 int32 = parse_int32(atom__25)
            var t380 Token = Token_Int{
                _0: t379,
            }
            jp377 = t380
        } else {
            var t381 Token = Token_Sym{
                _0: atom__25,
            }
            jp377 = t381
        }
        jp371 = jp377
    }
    var token__26 Token = jp371
    var t372 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__22)
    var t373 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: token__26,
        _1: t372,
    }
    retv368 = t373
    return retv368
}

func lex(source__27 string) *_goml_vec_Token {
    var retv400 *_goml_vec_Token
    var len__28 int = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var toks__30 *ref_Vec_5Token_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(toks0__29)
    var i__31 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop403:
    for {
        var t404 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
        var t405 bool = t404 < len__28
        if t405 {
            var t406 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
            var ch__32 rune = _goml_m_inherent_i_string_i_string_i_get(source__27, t406)
            var t408 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 40)
            if t408 {
                var t409 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t410 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t409, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t410)
                var t411 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                var t412 int = t411 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t412)
            } else {
                var t415 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 41)
                if t415 {
                    var t416 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t417 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t416, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t417)
                    var t418 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                    var t419 int = t418 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t419)
                } else {
                    var t422 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 32)
                    if t422 {
                        var t423 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                        var t424 int = t423 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, t424)
                    } else {
                        var t426 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__31)
                        var mtmp165 Tuple2_5Token_3int = lex_atom(source__27, t426)
                        var x166 Token = mtmp165._0
                        var x167 int = mtmp165._1
                        var next__34 int = x167
                        var tok__33 Token = x166
                        var t427 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                        var t428 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t427, tok__33)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t428)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop403
        }
    }
    var t402 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
    retv400 = t402
    return retv400
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var retv431 Value
    var t432 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(env__35)
    var t433 int = t432 - 1
    var i__37 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t433)
    var result__38 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    var done__39 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop436:
    for {
        var t448 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__39)
        var t449 bool = !t448
        var jp438 bool
        if t449 {
            var t450 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
            var t451 bool = t450 >= 0
            jp438 = t451
        } else {
            jp438 = false
        }
        if jp438 {
            var t439 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t439)
            var t441 string = binding__40.name
            var t442 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t441, name__36)
            if t442 {
                var t443 Value = binding__40.value
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(result__38, t443)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__39, true)
            } else {
                var t445 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__37)
                var t446 int = t445 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__37, t446)
            }
            continue
        } else {
            break Loop_loop436
        }
    }
    var t435 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(result__38)
    retv431 = t435
    return retv431
}

func lookup(local__41 *_goml_vec_Binding, global__42 *_goml_vec_Binding, name__43 string) Value {
    var retv453 Value
    var mtmp172 Value = env_lookup(local__41, name__43)
    var jp455 Value
    switch mtmp172.(type) {
    case Nil:
        var t456 Value = env_lookup(global__42, name__43)
        jp455 = t456
    default:
        var other__44 Value = mtmp172
        jp455 = other__44
    }
    retv453 = jp455
    return retv453
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var retv458 Tuple2_10Vec_5SExpr_3int
    var acc__47 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__47)
    var i__49 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__46)
    var done__50 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop463:
    for {
        var t475 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__50)
        var t476 bool = !t475
        var jp465 bool
        if t476 {
            var t477 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
            var t478 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__45)
            var t479 bool = t477 < t478
            jp465 = t479
        } else {
            jp465 = false
        }
        if jp465 {
            var t466 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
            var mtmp176 Token = vec_get__Vec_5Token(tokens__45, t466)
            switch mtmp176.(type) {
            case RParen:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__50, true)
                var t468 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
                var t469 int = t468 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__49, t469)
            default:
                var t471 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
                var mtmp181 Tuple2_5SExpr_3int = parse_expr(tokens__45, t471)
                var x182 SExpr = mtmp181._0
                var x183 int = mtmp181._1
                var next__52 int = x183
                var expr__51 SExpr = x182
                var t472 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t473 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t472, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t473)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__49, next__52)
            }
            continue
        } else {
            break Loop_loop463
        }
    }
    var t460 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
    var t461 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__49)
    var t462 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t460,
        _1: t461,
    }
    retv458 = t462
    return retv458
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var retv481 Tuple2_5SExpr_3int
    var mtmp186 Token = vec_get__Vec_5Token(tokens__53, start__54)
    var jp483 Tuple2_5SExpr_3int
    switch mtmp186.(type) {
    case LParen:
        var t484 int = start__54 + 1
        var mtmp190 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t484)
        var x191 *_goml_vec_SExpr = mtmp190._0
        var x192 int = mtmp190._1
        var next__56 int = x192
        var items__55 *_goml_vec_SExpr = x191
        var t485 SExpr = List{
            _0: items__55,
        }
        var t486 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t485,
            _1: next__56,
        }
        jp483 = t486
    case RParen:
        var t487 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t488 int = start__54 + 1
        var t489 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t487,
            _1: t488,
        }
        jp483 = t489
    case Token_Sym:
        var x187 string = mtmp186.(Token_Sym)._0
        var name__59 string = x187
        var t490 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t491 int = start__54 + 1
        var t492 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t490,
            _1: t491,
        }
        jp483 = t492
    case Token_Int:
        var x188 int32 = mtmp186.(Token_Int)._0
        var n__58 int32 = x188
        var t493 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t494 int = start__54 + 1
        var t495 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t493,
            _1: t494,
        }
        jp483 = t495
    case Token_Bool:
        var x189 bool = mtmp186.(Token_Bool)._0
        var b__57 bool = x189
        var t496 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t497 int = start__54 + 1
        var t498 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t496,
            _1: t497,
        }
        jp483 = t498
    default:
        panic("non-exhaustive match")
    }
    retv481 = jp483
    return retv481
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var retv500 *_goml_vec_SExpr
    var i__61 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var acc__62 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__62)
    Loop_loop503:
    for {
        var t504 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__61)
        var t505 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__60)
        var t506 bool = t504 < t505
        if t506 {
            var t507 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__61)
            var mtmp193 Tuple2_5SExpr_3int = parse_expr(tokens__60, t507)
            var x194 SExpr = mtmp193._0
            var x195 int = mtmp193._1
            var next__65 int = x195
            var expr__64 SExpr = x194
            var t508 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
            var t509 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t508, expr__64)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__63, t509)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__61, next__65)
            continue
        } else {
            break Loop_loop503
        }
    }
    var t502 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
    retv500 = t502
    return retv500
}

func value_to_string(value__66 Value) string {
    var retv512 string
    var jp514 string
    switch value__66.(type) {
    case Value_Int:
        var x198 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x198
        var t515 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__67)
        jp514 = t515
    case Value_Bool:
        var x199 bool = value__66.(Value_Bool)._0
        var b__68 bool = x199
        var t516 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(b__68)
        jp514 = t516
    case Func:
        jp514 = "<lambda>"
    case Nil:
        jp514 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv512 = jp514
    return retv512
}

func truthy(value__69 Value) bool {
    var retv518 bool
    var jp520 bool
    switch value__69.(type) {
    case Value_Int:
        var x201 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x201
        var t521 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(n__71, 0)
        var t522 bool = !t521
        jp520 = t522
    case Value_Bool:
        var x202 bool = value__69.(Value_Bool)._0
        var b__70 bool = x202
        jp520 = b__70
    case Func:
        jp520 = true
    case Nil:
        jp520 = false
    default:
        panic("non-exhaustive match")
    }
    retv518 = jp520
    return retv518
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv524 Value
    var jp526 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x204 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x204
        var t527 Value = Value_Int{
            _0: n__75,
        }
        jp526 = t527
    case SExpr_Bool:
        var x205 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x205
        var t528 Value = Value_Bool{
            _0: b__76,
        }
        jp526 = t528
    case SExpr_Sym:
        var x206 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x206
        var t529 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t530 Value = lookup(local__73, t529, name__77)
        jp526 = t530
    case List:
        var x207 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x207
        var t531 Value = eval_list(items__78, local__73, global__74)
        jp526 = t531
    default:
        panic("non-exhaustive match")
    }
    retv524 = jp526
    return retv524
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv533 Value
    var t536 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t537 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t536, 0)
    var jp535 Value
    if t537 {
        jp535 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp539 Value
        switch head__82.(type) {
        case SExpr_Sym:
            var x210 string = head__82.(SExpr_Sym)._0
            var name__83 string = x210
            var t540 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp539 = t540
        default:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t541 Value = apply(f__84, args__85, global__81)
            jp539 = t541
        }
        jp535 = jp539
    }
    retv533 = jp535
    return retv533
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv543 Value
    var jp545 Value
    switch name__86 {
    case "begin":
        var t546 Value = eval_begin(items__87, 1, local__88, global__89)
        jp545 = t546
    case "define":
        var t549 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t550 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t549, 3)
        var jp548 Value
        if t550 {
            var mtmp212 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp552 Value
            switch mtmp212.(type) {
            case SExpr_Sym:
                var x215 string = mtmp212.(SExpr_Sym)._0
                var var__90 string = x215
                var t553 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t553, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t554 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t554)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp552 = value__91
            default:
                jp552 = Nil{}
            }
            jp548 = jp552
        } else {
            jp548 = Nil{}
        }
        jp545 = jp548
    case "if":
        var t557 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t558 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t557, 4)
        var jp556 Value
        if t558 {
            var t559 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t559, local__88, global__89)
            var t562 bool = truthy(cond__94)
            var jp561 Value
            if t562 {
                var t563 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t564 Value = eval(t563, local__88, global__89)
                jp561 = t564
            } else {
                var t565 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t566 Value = eval(t565, local__88, global__89)
                jp561 = t566
            }
            jp556 = jp561
        } else {
            jp556 = Nil{}
        }
        jp545 = jp556
    case "lambda":
        var t569 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t570 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t569, 3)
        var jp568 Value
        if t570 {
            var mtmp218 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp572 Value
            switch mtmp218.(type) {
            case List:
                var x222 *_goml_vec_SExpr = mtmp218.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x222
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t573 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t574 Value = Func{
                    _0: t573,
                }
                jp572 = t574
            default:
                jp572 = Nil{}
            }
            jp568 = jp572
        } else {
            jp568 = Nil{}
        }
        jp545 = jp568
    case "+":
        var t575 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t576 Value = apply_builtin("+", t575)
        jp545 = t576
    case "-":
        var t577 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t578 Value = apply_builtin("-", t577)
        jp545 = t578
    case "*":
        var t579 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t580 Value = apply_builtin("*", t579)
        jp545 = t580
    case "/":
        var t581 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t582 Value = apply_builtin("/", t581)
        jp545 = t582
    case "=":
        var t583 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t584 Value = apply_builtin("=", t583)
        jp545 = t584
    default:
        var t585 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t585, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t586 Value = apply(f__98, args__99, global__89)
        jp545 = t586
    }
    retv543 = jp545
    return retv543
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv588 Value
    var i__104 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop591:
    for {
        var t592 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
        var t593 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t594 bool = t592 < t593
        if t594 {
            var t595 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
            var t596 SExpr = vec_get__Vec_5SExpr(items__100, t595)
            var v__106 Value = eval(t596, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t597 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__104)
            var t598 int = t597 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__104, t598)
            continue
        } else {
            break Loop_loop591
        }
    }
    var t590 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv588 = t590
    return retv588
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv601 *_goml_vec_string
    var i__108 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop604:
    for {
        var t605 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
        var t606 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t607 bool = t605 < t606
        if t607 {
            var t608 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
            var mtmp225 SExpr = vec_get__Vec_5SExpr(items__107, t608)
            switch mtmp225.(type) {
            case SExpr_Sym:
                var x228 string = mtmp225.(SExpr_Sym)._0
                var name__111 string = x228
                var t610 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t611 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t610, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t611)
                var t612 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
                var t613 int = t612 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__108, t613)
            default:
                var t615 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__108)
                var t616 int = t615 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__108, t616)
            }
            continue
        } else {
            break Loop_loop604
        }
    }
    var t603 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv601 = t603
    return retv601
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv619 *_goml_vec_Value
    var i__116 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop622:
    for {
        var t623 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
        var t624 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t625 bool = t623 < t624
        if t625 {
            var t626 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
            var t627 SExpr = vec_get__Vec_5SExpr(items__112, t626)
            var v__119 Value = eval(t627, local__114, global__115)
            var t628 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t629 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t628, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t629)
            var t630 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__116)
            var t631 int = t630 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__116, t631)
            continue
        } else {
            break Loop_loop622
        }
    }
    var t621 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv619 = t621
    return retv619
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv634 Value
    var jp636 Value
    switch name__120 {
    case "=":
        var t639 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t640 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t639, 2)
        var jp638 Value
        if t640 {
            var t641 Value = vec_get__Vec_5Value(args__121, 0)
            var t642 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp234 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t641,
                _1: t642,
            }
            var x235 Value = mtmp234._0
            var x236 Value = mtmp234._1
            var jp644 Value
            switch x236.(type) {
            case Value_Int:
                var x237 int32 = x236.(Value_Int)._0
                var jp646 Value
                switch x235.(type) {
                case Value_Int:
                    var x240 int32 = x235.(Value_Int)._0
                    var a__122 int32 = x240
                    var b__123 int32 = x237
                    var t647 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__122, b__123)
                    var t648 Value = Value_Bool{
                        _0: t647,
                    }
                    jp646 = t648
                default:
                    var t649 Value = Value_Bool{
                        _0: false,
                    }
                    jp646 = t649
                }
                jp644 = jp646
            case Value_Bool:
                var x238 bool = x236.(Value_Bool)._0
                var jp651 Value
                switch x235.(type) {
                case Value_Bool:
                    var x244 bool = x235.(Value_Bool)._0
                    var a__124 bool = x244
                    var b__125 bool = x238
                    var t652 bool = _goml_m_trait__impl_i_Eq_i_bool_i_eq(a__124, b__125)
                    var t653 Value = Value_Bool{
                        _0: t652,
                    }
                    jp651 = t653
                default:
                    var t654 Value = Value_Bool{
                        _0: false,
                    }
                    jp651 = t654
                }
                jp644 = jp651
            default:
                var t655 Value = Value_Bool{
                    _0: false,
                }
                jp644 = t655
            }
            jp638 = jp644
        } else {
            var t656 Value = Value_Bool{
                _0: false,
            }
            jp638 = t656
        }
        jp636 = jp638
        retv634 = jp636
        return retv634
    case "+":
        var i__126 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop660:
        for {
            var t661 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
            var t662 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t663 bool = t661 < t662
            if t663 {
                var t664 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                var mtmp246 Value = vec_get__Vec_5Value(args__121, t664)
                switch mtmp246.(type) {
                case Value_Int:
                    var x247 int32 = mtmp246.(Value_Int)._0
                    var n__128 int32 = x247
                    var t666 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t667 int32 = t666 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t667)
                    var t668 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                    var t669 int = t668 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__126, t669)
                default:
                    var t671 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__126)
                    var t672 int = t671 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__126, t672)
                }
                continue
            } else {
                break Loop_loop660
            }
        }
        var t658 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t659 Value = Value_Int{
            _0: t658,
        }
        jp636 = t659
        retv634 = jp636
        return retv634
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop677:
        for {
            var t678 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
            var t679 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t680 bool = t678 < t679
            if t680 {
                var t681 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                var mtmp252 Value = vec_get__Vec_5Value(args__121, t681)
                switch mtmp252.(type) {
                case Value_Int:
                    var x253 int32 = mtmp252.(Value_Int)._0
                    var n__131 int32 = x253
                    var t683 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t684 int32 = t683 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t684)
                    var t685 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                    var t686 int = t685 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__129, t686)
                default:
                    var t688 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
                    var t689 int = t688 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__129, t689)
                }
                continue
            } else {
                break Loop_loop677
            }
        }
        var t675 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t676 Value = Value_Int{
            _0: t675,
        }
        jp636 = t676
        retv634 = jp636
        return retv634
    case "-":
        var mtmp258 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp692 Value
        switch mtmp258 {
        case 1:
            var mtmp259 Value = vec_get__Vec_5Value(args__121, 0)
            var jp694 Value
            switch mtmp259.(type) {
            case Value_Int:
                var x260 int32 = mtmp259.(Value_Int)._0
                var n__132 int32 = x260
                var t695 int32 = 0 - n__132
                var t696 Value = Value_Int{
                    _0: t695,
                }
                jp694 = t696
            default:
                jp694 = Nil{}
            }
            jp692 = jp694
        case 2:
            var t697 Value = vec_get__Vec_5Value(args__121, 0)
            var t698 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp263 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t697,
                _1: t698,
            }
            var x264 Value = mtmp263._0
            var x265 Value = mtmp263._1
            var jp700 Value
            switch x265.(type) {
            case Value_Int:
                var x266 int32 = x265.(Value_Int)._0
                var jp702 Value
                switch x264.(type) {
                case Value_Int:
                    var x269 int32 = x264.(Value_Int)._0
                    var a__133 int32 = x269
                    var b__134 int32 = x266
                    var t703 int32 = a__133 - b__134
                    var t704 Value = Value_Int{
                        _0: t703,
                    }
                    jp702 = t704
                default:
                    jp702 = Nil{}
                }
                jp700 = jp702
            default:
                jp700 = Nil{}
            }
            jp692 = jp700
        default:
            jp692 = Nil{}
        }
        jp636 = jp692
        retv634 = jp636
        return retv634
    case "/":
        var t707 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t708 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t707, 2)
        var jp706 Value
        if t708 {
            var t709 Value = vec_get__Vec_5Value(args__121, 0)
            var t710 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp272 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t709,
                _1: t710,
            }
            var x273 Value = mtmp272._0
            var x274 Value = mtmp272._1
            var jp712 Value
            switch x274.(type) {
            case Value_Int:
                var x275 int32 = x274.(Value_Int)._0
                var jp714 Value
                switch x273.(type) {
                case Value_Int:
                    var x278 int32 = x273.(Value_Int)._0
                    var a__135 int32 = x278
                    var b__136 int32 = x275
                    var t715 int32 = a__135 / b__136
                    var t716 Value = Value_Int{
                        _0: t715,
                    }
                    jp714 = t716
                default:
                    jp714 = Nil{}
                }
                jp712 = jp714
            default:
                jp712 = Nil{}
            }
            jp706 = jp712
        } else {
            jp706 = Nil{}
        }
        jp636 = jp706
        retv634 = jp636
        return retv634
    default:
        jp636 = Nil{}
        retv634 = jp636
        return retv634
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv718 Value
    var jp720 Value
    switch func__137.(type) {
    case Func:
        var x283 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x283
        var t721 Value = apply_lambda(fun__140, args__138)
        jp720 = t721
    default:
        jp720 = Nil{}
    }
    retv718 = jp720
    return retv718
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv723 Value
    var t724 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t724)
    var i__144 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop730:
    for {
        var t741 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
        var t742 *_goml_vec_string = lambda__141.params
        var t743 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t742)
        var t744 bool = t741 < t743
        var jp732 bool
        if t744 {
            var t745 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var t746 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t747 bool = t745 < t746
            jp732 = t747
        } else {
            jp732 = false
        }
        if jp732 {
            var t733 *_goml_vec_string = lambda__141.params
            var t734 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var name__145 string = vec_get__Vec_6string(t733, t734)
            var t735 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t735)
            var t736 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t737 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t736, t737)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t738 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__144)
            var t739 int = t738 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__144, t739)
            continue
        } else {
            break Loop_loop730
        }
    }
    var t726 SExpr = lambda__141.body
    var t727 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t728 *ref_Vec_7Binding_x = lambda__141.global
    var t729 Value = eval(t726, t727, t728)
    retv723 = t729
    return retv723
}

func main0() struct{} {
    var t749 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t749)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t750 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t750)
    var t751 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t752 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t751, t752, global__148)
    var t753 string = value_to_string(result__151)
    println__T_string(t753)
    var t754 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t754)
    var t755 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t756 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t755, t756, global__148)
    var t757 string = value_to_string(result2__153)
    println__T_string(t757)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv759 int
    var t760 int = _goml_runtime_core_string_len(self__8)
    retv759 = t760
    return retv759
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv762 bool
    var t763 bool = self__59 == other__60
    retv762 = t763
    return retv762
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv765 *ref_int_x
    var t766 *ref_int_x = ref__Ref_3int(value__207)
    retv765 = t766
    return retv765
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv768 *ref_bool_x
    var t769 *ref_bool_x = ref__Ref_4bool(value__207)
    retv768 = t769
    return retv768
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv771 bool
    var t772 bool = ref_get__Ref_4bool(self__208)
    retv771 = t772
    return retv771
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv774 int
    var t775 int = ref_get__Ref_3int(self__208)
    retv774 = t775
    return retv774
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv777 rune
    var t778 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv777 = t778
    return retv777
}

func _goml_m_trait__impl_i_Eq_i_char_i_eq(self__57 rune, other__58 rune) bool {
    var retv780 bool
    var t781 bool = self__57 == other__58
    retv780 = t781
    return retv780
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
    var retv787 *ref_int32_x
    var t788 *ref_int32_x = ref__Ref_5int32(value__207)
    retv787 = t788
    return retv787
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv790 int32
    var t791 int32 = ref_get__Ref_5int32(self__208)
    retv790 = t791
    return retv790
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__207 string) *ref_string_x {
    var retv795 *ref_string_x
    var t796 *ref_string_x = ref__Ref_6string(value__207)
    retv795 = t796
    return retv795
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__208 *ref_string_x) string {
    var retv798 string
    var t799 string = ref_get__Ref_6string(self__208)
    retv798 = t799
    return retv798
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv801 string
    var t802 string = _goml_runtime_core_char_to_string(self__7)
    retv801 = t802
    return retv801
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__209 *ref_string_x, value__210 string) struct{} {
    ref_set__Ref_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv806 *_goml_vec_Token
    var t807 *_goml_vec_Token = vec_new__Vec_5Token()
    retv806 = t807
    return retv806
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__207 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv809 *ref_Vec_5Token_x
    var t810 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__207)
    retv809 = t810
    return retv809
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__208 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv812 *_goml_vec_Token
    var t813 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__208)
    retv812 = t813
    return retv812
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__128 *_goml_vec_Token, elem__129 Token) *_goml_vec_Token {
    var retv815 *_goml_vec_Token
    var t816 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__128)
    var t817 int = t816 + 1
    var result__130 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Token(t817)
    var index__131 int = 0
    Loop_loop819:
    for {
        var t820 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__128)
        var t821 bool = index__131 < t820
        if t821 {
            var t822 Token = vec_get__Vec_5Token(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__130, t822)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t823 int = compound_old38 + compound_value39
            index__131 = t823
            continue
        } else {
            break Loop_loop819
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__130, elem__129)
    retv815 = result__130
    return retv815
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__209 *ref_Vec_5Token_x, value__210 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__137 *_goml_vec_Binding) int {
    var retv828 int
    var t829 int = vec_len__Vec_7Binding(self__137)
    retv828 = t829
    return retv828
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__207 Value) *ref_Value_x {
    var retv831 *ref_Value_x
    var t832 *ref_Value_x = ref__Ref_5Value(value__207)
    retv831 = t832
    return retv831
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv834 bool
    var t835 bool = self__55 == other__56
    retv834 = t835
    return retv834
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__209 *ref_Value_x, value__210 Value) struct{} {
    ref_set__Ref_5Value(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__208 *ref_Value_x) Value {
    var retv839 Value
    var t840 Value = ref_get__Ref_5Value(self__208)
    retv839 = t840
    return retv839
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv842 *_goml_vec_SExpr
    var t843 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv842 = t843
    return retv842
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__207 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv845 *ref_Vec_5SExpr_x
    var t846 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__207)
    retv845 = t846
    return retv845
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__137 *_goml_vec_Token) int {
    var retv848 int
    var t849 int = vec_len__Vec_5Token(self__137)
    retv848 = t849
    return retv848
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__208 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv851 *_goml_vec_SExpr
    var t852 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__208)
    retv851 = t852
    return retv851
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__128 *_goml_vec_SExpr, elem__129 SExpr) *_goml_vec_SExpr {
    var retv854 *_goml_vec_SExpr
    var t855 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__128)
    var t856 int = t855 + 1
    var result__130 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__SExpr(t856)
    var index__131 int = 0
    Loop_loop858:
    for {
        var t859 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__128)
        var t860 bool = index__131 < t859
        if t860 {
            var t861 SExpr = vec_get__Vec_5SExpr(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__130, t861)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t862 int = compound_old38 + compound_value39
            index__131 = t862
            continue
        } else {
            break Loop_loop858
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__130, elem__129)
    retv854 = result__130
    return retv854
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__209 *ref_Vec_5SExpr_x, value__210 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv867 string
    var t868 string = _goml_runtime_core_int32_to_string(self__6)
    retv867 = t868
    return retv867
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv870 string
    var t871 string = _goml_runtime_core_bool_to_string(self__37)
    retv870 = t871
    return retv870
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv873 bool
    var t874 bool = self__65 == other__66
    retv873 = t874
    return retv873
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__208 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv876 *_goml_vec_Binding
    var t877 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__208)
    retv876 = t877
    return retv876
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__137 *_goml_vec_SExpr) int {
    var retv879 int
    var t880 int = vec_len__Vec_5SExpr(self__137)
    retv879 = t880
    return retv879
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__128 *_goml_vec_Binding, elem__129 Binding) *_goml_vec_Binding {
    var retv882 *_goml_vec_Binding
    var t883 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__128)
    var t884 int = t883 + 1
    var result__130 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Binding(t884)
    var index__131 int = 0
    Loop_loop886:
    for {
        var t887 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__128)
        var t888 bool = index__131 < t887
        if t888 {
            var t889 Binding = vec_get__Vec_7Binding(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__130, t889)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t890 int = compound_old38 + compound_value39
            index__131 = t890
            continue
        } else {
            break Loop_loop886
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__130, elem__129)
    retv882 = result__130
    return retv882
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__209 *ref_Vec_7Binding_x, value__210 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv895 *_goml_vec_string
    var t896 *_goml_vec_string = vec_new__Vec_6string()
    retv895 = t896
    return retv895
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__207 *_goml_vec_string) *ref_Vec_6string_x {
    var retv898 *ref_Vec_6string_x
    var t899 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__207)
    retv898 = t899
    return retv898
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__208 *ref_Vec_6string_x) *_goml_vec_string {
    var retv901 *_goml_vec_string
    var t902 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__208)
    retv901 = t902
    return retv901
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__128 *_goml_vec_string, elem__129 string) *_goml_vec_string {
    var retv904 *_goml_vec_string
    var t905 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__128)
    var t906 int = t905 + 1
    var result__130 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(t906)
    var index__131 int = 0
    Loop_loop908:
    for {
        var t909 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__128)
        var t910 bool = index__131 < t909
        if t910 {
            var t911 string = vec_get__Vec_6string(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__130, t911)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t912 int = compound_old38 + compound_value39
            index__131 = t912
            continue
        } else {
            break Loop_loop908
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__130, elem__129)
    retv904 = result__130
    return retv904
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__209 *ref_Vec_6string_x, value__210 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv917 *_goml_vec_Value
    var t918 *_goml_vec_Value = vec_new__Vec_5Value()
    retv917 = t918
    return retv917
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__207 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv920 *ref_Vec_5Value_x
    var t921 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__207)
    retv920 = t921
    return retv920
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__208 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv923 *_goml_vec_Value
    var t924 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__208)
    retv923 = t924
    return retv923
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__128 *_goml_vec_Value, elem__129 Value) *_goml_vec_Value {
    var retv926 *_goml_vec_Value
    var t927 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__128)
    var t928 int = t927 + 1
    var result__130 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Value(t928)
    var index__131 int = 0
    Loop_loop930:
    for {
        var t931 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__128)
        var t932 bool = index__131 < t931
        if t932 {
            var t933 Value = vec_get__Vec_5Value(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__130, t933)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t934 int = compound_old38 + compound_value39
            index__131 = t934
            continue
        } else {
            break Loop_loop930
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__130, elem__129)
    retv926 = result__130
    return retv926
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__209 *ref_Vec_5Value_x, value__210 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__137 *_goml_vec_Value) int {
    var retv939 int
    var t940 int = vec_len__Vec_5Value(self__137)
    retv939 = t940
    return retv939
}

func _goml_m_trait__impl_i_Eq_i_bool_i_eq(self__53 bool, other__54 bool) bool {
    var retv942 bool
    var t943 bool = self__53 == other__54
    retv942 = t943
    return retv942
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__207 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv945 *ref_Vec_7Binding_x
    var t946 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__207)
    retv945 = t946
    return retv945
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__137 *_goml_vec_string) int {
    var retv948 int
    var t949 int = vec_len__Vec_6string(self__137)
    retv948 = t949
    return retv948
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv951 *_goml_vec_Binding
    var t952 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv951 = t952
    return retv951
}

func println__T_string(value__1 string) struct{} {
    var t954 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t954)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Token(capacity__125 int) *_goml_vec_Token {
    var retv957 *_goml_vec_Token
    var t958 *_goml_vec_Token = vec_with_capacity__Vec_5Token(capacity__125)
    retv957 = t958
    return retv957
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__126 *_goml_vec_Token, elem__127 Token) struct{} {
    vec_push__Vec_5Token(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__SExpr(capacity__125 int) *_goml_vec_SExpr {
    var retv962 *_goml_vec_SExpr
    var t963 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(capacity__125)
    retv962 = t963
    return retv962
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__126 *_goml_vec_SExpr, elem__127 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Binding(capacity__125 int) *_goml_vec_Binding {
    var retv967 *_goml_vec_Binding
    var t968 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(capacity__125)
    retv967 = t968
    return retv967
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__126 *_goml_vec_Binding, elem__127 Binding) struct{} {
    vec_push__Vec_7Binding(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__125 int) *_goml_vec_string {
    var retv972 *_goml_vec_string
    var t973 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__125)
    retv972 = t973
    return retv972
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__Value(capacity__125 int) *_goml_vec_Value {
    var retv977 *_goml_vec_Value
    var t978 *_goml_vec_Value = vec_with_capacity__Vec_5Value(capacity__125)
    retv977 = t978
    return retv977
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__126 *_goml_vec_Value, elem__127 Value) struct{} {
    vec_push__Vec_5Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv982 string
    retv982 = self__38
    return retv982
}

func main() {
    main0()
}
