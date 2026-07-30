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

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    if !_goml_runtime_core_string_is_char_boundary(s, start) && _goml_runtime_core_string_is_char_boundary(s, end) {
        panic("invalid string byte slice")
    }
    return s[start:end]
}

func _goml_runtime_core_string_is_char_boundary(s string, i int) bool {
    if i < 0 || i > int(len(s)) {
        return false
    }
    if i == int(len(s)) {
        return true
    }
    return _goml_utf8.RuneStart(s[i])
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    if !_goml_utf8.Valid(bytes.items) {
        return Tuple2_4bool_6string{
            _0: false,
            _1: "",
        }
    }
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    if value > 1114111 || value >= 55296 && value <= 57343 {
        return Tuple2_4bool_4char{
            _0: false,
            _1: 0,
        }
    }
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_new__Vec_5uint8() *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: nil,
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_reserve__Vec_5uint8(vec *_goml_vec_uint8, additional int) struct{} {
    vec.items = _goml_slices.Grow(vec.items, int(additional))
    return struct{}{}
}

type _goml_vec_string struct {
    items []string
}

type _goml_vec__goml_m_std_p_json_p_Value struct {
    items []_goml_m_std_p_json_p_Value
}

func vec_new___goml_m_Vec__16std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    return &_goml_vec__goml_m_std_p_json_p_Value{
        items: nil,
    }
}

func vec_push___goml_m_Vec__16std_p_json_p_Value(vec *_goml_vec__goml_m_std_p_json_p_Value, elem _goml_m_std_p_json_p_Value) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get___goml_m_Vec__16std_p_json_p_Value(vec *_goml_vec__goml_m_std_p_json_p_Value, index int) _goml_m_std_p_json_p_Value {
    return vec.items[index]
}

func vec_len___goml_m_Vec__16std_p_json_p_Value(vec *_goml_vec__goml_m_std_p_json_p_Value) int {
    return int(len(vec.items))
}

type _goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value struct {
    items []Tuple2_6string_26_goml_m_std_p_json_p_Value
}

func vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    return &_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value{
        items: nil,
    }
}

func vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(vec *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(vec *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, index int) Tuple2_6string_26_goml_m_std_p_json_p_Value {
    return vec.items[index]
}

func vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(vec *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value) int {
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

type ref_uint32_x struct {
    value uint32
}

func ref__Ref_6uint32(value uint32) *ref_uint32_x {
    return &ref_uint32_x{
        value: value,
    }
}

func ref_get__Ref_6uint32(reference *ref_uint32_x) uint32 {
    return reference.value
}

func ref_set__Ref_6uint32(reference *ref_uint32_x, value uint32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref__goml_m_Option____std_p_json_p_Value_x struct {
    value _goml_m_Option____std_p_json_p_Value
}

func ref___goml_m_Ref__24Option____std_p_json_p_Value(value _goml_m_Option____std_p_json_p_Value) *ref__goml_m_Option____std_p_json_p_Value_x {
    return &ref__goml_m_Option____std_p_json_p_Value_x{
        value: value,
    }
}

func ref_get___goml_m_Ref__24Option____std_p_json_p_Value(reference *ref__goml_m_Option____std_p_json_p_Value_x) _goml_m_Option____std_p_json_p_Value {
    return reference.value
}

func ref_set___goml_m_Ref__24Option____std_p_json_p_Value(reference *ref__goml_m_Option____std_p_json_p_Value_x, value _goml_m_Option____std_p_json_p_Value) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type Tuple2_6string_26_goml_m_std_p_json_p_Value struct {
    _0 string
    _1 _goml_m_std_p_json_p_Value
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type _goml_m_std_p_bytes_p_Bytes struct {
    values *_goml_vec_uint8
}

type _goml_m_std_p_text_p_StringBuilder struct {
    values *_goml_vec_uint8
}

type _goml_m_std_p_json_p_JsonParser struct {
    input string
    index *ref_int_x
}

type _goml_m_std_p_json_p_Value interface {
    is_goml_m_std_p_json_p_Value()
}

type Object struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
}

func (_ Object) is_goml_m_std_p_json_p_Value() {}

type Array struct {
    _0 *_goml_vec__goml_m_std_p_json_p_Value
}

func (_ Array) is_goml_m_std_p_json_p_Value() {}

type String struct {
    _0 string
}

func (_ String) is_goml_m_std_p_json_p_Value() {}

type Number struct {
    _0 string
}

func (_ Number) is_goml_m_std_p_json_p_Value() {}

type Bool struct {
    _0 bool
}

func (_ Bool) is_goml_m_std_p_json_p_Value() {}

type Null struct {}

func (_ Null) is_goml_m_std_p_json_p_Value() {}

type Option__uint8 interface {
    isOption__uint8()
}

type Option__uint8_None struct {}

func (_ Option__uint8_None) isOption__uint8() {}

type Option__uint8_Some struct {
    _0 uint8
}

func (_ Option__uint8_Some) isOption__uint8() {}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

type Result__unit__string interface {
    isResult__unit__string()
}

type Result__unit__string_Ok struct {
    _0 struct{}
}

func (_ Result__unit__string_Ok) isResult__unit__string() {}

type Result__unit__string_Err struct {
    _0 string
}

func (_ Result__unit__string_Err) isResult__unit__string() {}

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

type _goml_m_Option_____o_string_c_string_q_ interface {
    is_goml_m_Option_____o_string_c_string_q_()
}

type _goml_m_Option_____o_string_c_string_q__None struct {}

func (_ _goml_m_Option_____o_string_c_string_q__None) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option_____o_string_c_string_q__Some struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Option_____o_string_c_string_q__Some) is_goml_m_Option_____o_string_c_string_q_() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

type Option__uint32 interface {
    isOption__uint32()
}

type Option__uint32_None struct {}

func (_ Option__uint32_None) isOption__uint32() {}

type Option__uint32_Some struct {
    _0 uint32
}

func (_ Option__uint32_Some) isOption__uint32() {}

type Result__uint32__string interface {
    isResult__uint32__string()
}

type Result__uint32__string_Ok struct {
    _0 uint32
}

func (_ Result__uint32__string_Ok) isResult__uint32__string() {}

type Result__uint32__string_Err struct {
    _0 string
}

func (_ Result__uint32__string_Err) isResult__uint32__string() {}

type Option__char interface {
    isOption__char()
}

type Option__char_None struct {}

func (_ Option__char_None) isOption__char() {}

type Option__char_Some struct {
    _0 rune
}

func (_ Option__char_Some) isOption__char() {}

type _goml_m_Result____std_p_json_p_Value____string interface {
    is_goml_m_Result____std_p_json_p_Value____string()
}

type _goml_m_Result____std_p_json_p_Value____string_Ok struct {
    _0 _goml_m_std_p_json_p_Value
}

func (_ _goml_m_Result____std_p_json_p_Value____string_Ok) is_goml_m_Result____std_p_json_p_Value____string() {}

type _goml_m_Result____std_p_json_p_Value____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____std_p_json_p_Value____string_Err) is_goml_m_Result____std_p_json_p_Value____string() {}

type _goml_m_Option____std_p_json_p_Value interface {
    is_goml_m_Option____std_p_json_p_Value()
}

type _goml_m_Option____std_p_json_p_Value_None struct {}

func (_ _goml_m_Option____std_p_json_p_Value_None) is_goml_m_Option____std_p_json_p_Value() {}

type _goml_m_Option____std_p_json_p_Value_Some struct {
    _0 _goml_m_std_p_json_p_Value
}

func (_ _goml_m_Option____std_p_json_p_Value_Some) is_goml_m_Option____std_p_json_p_Value() {}

type Option__bool interface {
    isOption__bool()
}

type Option__bool_None struct {}

func (_ Option__bool_None) isOption__bool() {}

type Option__bool_Some struct {
    _0 bool
}

func (_ Option__bool_Some) isOption__bool() {}

type _goml_m_Option____Vec_l_std_p_json_p_Value_r_ interface {
    is_goml_m_Option____Vec_l_std_p_json_p_Value_r_()
}

type _goml_m_Option____Vec_l_std_p_json_p_Value_r__None struct {}

func (_ _goml_m_Option____Vec_l_std_p_json_p_Value_r__None) is_goml_m_Option____Vec_l_std_p_json_p_Value_r_() {}

type _goml_m_Option____Vec_l_std_p_json_p_Value_r__Some struct {
    _0 *_goml_vec__goml_m_std_p_json_p_Value
}

func (_ _goml_m_Option____Vec_l_std_p_json_p_Value_r__Some) is_goml_m_Option____Vec_l_std_p_json_p_Value_r_() {}

func _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new() _goml_m_std_p_text_p_StringBuilder {
    var retv192 _goml_m_std_p_text_p_StringBuilder
    var t193 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var t194 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: t193,
    }
    retv192 = t194
    return retv192
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var t209 *_goml_vec_uint8 = self__3.values
    var t210 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__uint8(t209, t210)
    var for_index1 int = 0
    var for_limit2 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    Loop_loop212:
    for {
        var t213 bool = for_index1 < for_limit2
        if t213 {
            var for_item3 int = for_index1
            var t214 int = for_index1 + 1
            for_index1 = t214
            var index__5 int = for_item3
            var t215 *_goml_vec_uint8 = self__3.values
            var t216 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__4, index__5)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(t215, t216)
            continue
        } else {
            break Loop_loop212
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__6 _goml_m_std_p_text_p_StringBuilder, value__7 rune) struct{} {
    var t219 string = _goml_m_inherent_i_char_i_char_i_to__string(value__7)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__6, t219)
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(self__15 _goml_m_std_p_text_p_StringBuilder) string {
    var retv233 string
    var t234 *_goml_vec_uint8 = self__15.values
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t234)
    var x12 string = mtmp10._1
    var value__16 string = x12
    retv233 = value__16
    return retv233
}

func _goml_m_std_p_json_p_json__parser(input__0 string) _goml_m_std_p_json_p_JsonParser {
    var retv504 _goml_m_std_p_json_p_JsonParser
    var t505 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t506 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__0,
        index: t505,
    }
    retv504 = t506
    return retv504
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var retv508 string
    var t509 string = message__2 + " at byte "
    var t510 *ref_int_x = value__1.index
    var t511 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t510)
    var t512 string = _goml_m_inherent_i_int_i_int_i_to__string(t511)
    var t513 string = t509 + t512
    retv508 = t513
    return retv508
}

func _goml_m_std_p_json_p_json__whitespace(value__3 uint8) bool {
    var retv515 bool
    var t524 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 9)
    var jp522 bool
    if t524 {
        jp522 = true
    } else {
        var t525 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 10)
        jp522 = t525
    }
    var jp519 bool
    if jp522 {
        jp519 = true
    } else {
        var t523 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 13)
        jp519 = t523
    }
    var jp517 bool
    if jp519 {
        jp517 = true
    } else {
        var t520 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 32)
        jp517 = t520
    }
    retv515 = jp517
    return retv515
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop528:
    for {
        var t536 *ref_int_x = value__4.index
        var t537 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t536)
        var t538 string = value__4.input
        var t539 int = _goml_m_inherent_i_string_i_string_i_byte__len(t538)
        var t540 bool = t537 < t539
        var jp530 bool
        if t540 {
            var t541 string = value__4.input
            var t542 *ref_int_x = value__4.index
            var t543 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t542)
            var t544 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t541, t543)
            var t545 bool = _goml_m_std_p_json_p_json__whitespace(t544)
            jp530 = t545
        } else {
            jp530 = false
        }
        if jp530 {
            var t531 *ref_int_x = value__4.index
            var t532 *ref_int_x = value__4.index
            var t533 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t532)
            var t534 int = t533 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t531, t534)
            continue
        } else {
            break Loop_loop528
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var retv547 Option__uint32
    var t573 bool = value__5 >= 48
    var jp551 bool
    if t573 {
        var t574 bool = value__5 <= 57
        jp551 = t574
    } else {
        jp551 = false
    }
    var jp549 Option__uint32
    if jp551 {
        var t552 uint8 = value__5 - 48
        var t553 uint32 = uint32(uint8(t552))
        var t554 Option__uint32 = Option__uint32_Some{
            _0: t553,
        }
        jp549 = t554
    } else {
        var t571 bool = value__5 >= 65
        var jp558 bool
        if t571 {
            var t572 bool = value__5 <= 70
            jp558 = t572
        } else {
            jp558 = false
        }
        var jp556 Option__uint32
        if jp558 {
            var t559 uint8 = value__5 - 55
            var t560 uint32 = uint32(uint8(t559))
            var t561 Option__uint32 = Option__uint32_Some{
                _0: t560,
            }
            jp556 = t561
        } else {
            var t569 bool = value__5 >= 97
            var jp565 bool
            if t569 {
                var t570 bool = value__5 <= 102
                jp565 = t570
            } else {
                jp565 = false
            }
            var jp563 Option__uint32
            if jp565 {
                var t566 uint8 = value__5 - 87
                var t567 uint32 = uint32(uint8(t566))
                var t568 Option__uint32 = Option__uint32_Some{
                    _0: t567,
                }
                jp563 = t568
            } else {
                jp563 = Option__uint32_None{}
            }
            jp556 = jp563
        }
        jp549 = jp556
    }
    retv547 = jp549
    return retv547
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var retv576 Result__uint32__string
    var t579 *ref_int_x = value__6.index
    var t580 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t579)
    var t581 int = t580 + 4
    var t582 string = value__6.input
    var t583 int = _goml_m_inherent_i_string_i_string_i_byte__len(t582)
    var t584 bool = t581 > t583
    var jp578 Result__uint32__string
    if t584 {
        var t585 string = _goml_m_std_p_json_p_json__error(value__6, "incomplete unicode escape")
        var t586 Result__uint32__string = Result__uint32__string_Err{
            _0: t585,
        }
        jp578 = t586
        retv576 = jp578
        return retv576
    } else {
        var t587_source int = 0
        var t587 uint32 = uint32(int(t587_source))
        var result__7 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(t587)
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop595:
        for {
            var t596 bool = for_index0 < for_limit1
            if t596 {
                var for_item2 int = for_index0
                var t597 int = for_index0 + 1
                for_index0 = t597
                var offset__8 int = for_item2
                var t598 string = value__6.input
                var t599 *ref_int_x = value__6.index
                var t600 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t599)
                var t601 int = t600 + offset__8
                var t602 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t598, t601)
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t602)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t604 string = _goml_m_std_p_json_p_json__error(value__6, "invalid unicode escape")
                    var t605 Result__uint32__string = Result__uint32__string_Err{
                        _0: t604,
                    }
                    retv576 = t605
                    return retv576
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var digit__9 uint32 = x5
                    var t606 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
                    var t607 uint32 = t606 * 16
                    var t608 uint32 = t607 + digit__9
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(result__7, t608)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop595
            }
        }
        var t589 *ref_int_x = value__6.index
        var t590 *ref_int_x = value__6.index
        var t591 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t590)
        var t592 int = t591 + 4
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t589, t592)
        var t593 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
        var t594 Result__uint32__string = Result__uint32__string_Ok{
            _0: t593,
        }
        jp578 = t594
        retv576 = jp578
        return retv576
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var retv611 Result__unit__string
    var mtmp8 Option__char = char_from_uint32(codepoint__12)
    var jp613 Result__unit__string
    switch mtmp8.(type) {
    case Option__char_None:
        var t614 string = _goml_m_std_p_json_p_json__error(value__10, "invalid unicode codepoint")
        var t615 Result__unit__string = Result__unit__string_Err{
            _0: t614,
        }
        jp613 = t615
    case Option__char_Some:
        var x9 rune = mtmp8.(Option__char_Some)._0
        var character__13 rune = x9
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__11, character__13)
        var t616 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp613 = t616
    default:
        panic("non-exhaustive match")
    }
    retv611 = jp613
    return retv611
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var retv618 Result__unit__string
    var mtmp11 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp620 uint32
    switch mtmp11.(type) {
    case Result__uint32__string_Ok:
        var x12 uint32 = mtmp11.(Result__uint32__string_Ok)._0
        var codepoint__16 uint32 = x12
        jp620 = codepoint__16
        var first__18 uint32 = jp620
        var t682 bool = first__18 >= 55296
        var jp624 bool
        if t682 {
            var t683 bool = first__18 <= 56319
            jp624 = t683
        } else {
            jp624 = false
        }
        var jp622 Result__unit__string
        if jp624 {
            var t661 *ref_int_x = value__14.index
            var t662 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t661)
            var t663 int = t662 + 2
            var t664 string = value__14.input
            var t665 int = _goml_m_inherent_i_string_i_string_i_byte__len(t664)
            var t666 bool = t663 > t665
            var jp653 bool
            if t666 {
                jp653 = true
            } else {
                var t667 string = value__14.input
                var t668 *ref_int_x = value__14.index
                var t669 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t668)
                var t670 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t667, t669)
                var t671 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t670, 92)
                var t672 bool = !t671
                jp653 = t672
            }
            var jp628 bool
            if jp653 {
                jp628 = true
            } else {
                var t654 string = value__14.input
                var t655 *ref_int_x = value__14.index
                var t656 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t655)
                var t657 int = t656 + 1
                var t658 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t654, t657)
                var t659 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t658, 117)
                var t660 bool = !t659
                jp628 = t660
            }
            var jp626 Result__unit__string
            if jp628 {
                var t629 string = _goml_m_std_p_json_p_json__error(value__14, "missing low surrogate")
                var t630 Result__unit__string = Result__unit__string_Err{
                    _0: t629,
                }
                jp626 = t630
                jp622 = jp626
                retv618 = jp622
                return retv618
            } else {
                var t631 *ref_int_x = value__14.index
                var t632 *ref_int_x = value__14.index
                var t633 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t632)
                var t634 int = t633 + 2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t631, t634)
                var mtmp15 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp636 uint32
                switch mtmp15.(type) {
                case Result__uint32__string_Ok:
                    var x16 uint32 = mtmp15.(Result__uint32__string_Ok)._0
                    var codepoint__19 uint32 = x16
                    jp636 = codepoint__19
                    var second__21 uint32 = jp636
                    var t649 bool = second__21 < 56320
                    var jp640 bool
                    if t649 {
                        jp640 = true
                    } else {
                        var t650 bool = second__21 > 57343
                        jp640 = t650
                    }
                    var jp638 Result__unit__string
                    if jp640 {
                        var t641 string = _goml_m_std_p_json_p_json__error(value__14, "invalid low surrogate")
                        var t642 Result__unit__string = Result__unit__string_Err{
                            _0: t641,
                        }
                        jp638 = t642
                    } else {
                        var t643 uint32 = first__18 - 55296
                        var t644 uint32 = t643 * 1024
                        var t645 uint32 = 65536 + t644
                        var t646 uint32 = t645 + second__21
                        var t647 uint32 = t646 - 56320
                        var t648 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, t647)
                        jp638 = t648
                    }
                    jp626 = jp638
                    jp622 = jp626
                    retv618 = jp622
                    return retv618
                case Result__uint32__string_Err:
                    var x17 string = mtmp15.(Result__uint32__string_Err)._0
                    var error__20 string = x17
                    var t651 Result__unit__string = Result__unit__string_Err{
                        _0: error__20,
                    }
                    retv618 = t651
                    return retv618
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t680 bool = first__18 >= 56320
            var jp676 bool
            if t680 {
                var t681 bool = first__18 <= 57343
                jp676 = t681
            } else {
                jp676 = false
            }
            var jp674 Result__unit__string
            if jp676 {
                var t677 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t678 Result__unit__string = Result__unit__string_Err{
                    _0: t677,
                }
                jp674 = t678
            } else {
                var t679 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, first__18)
                jp674 = t679
            }
            jp622 = jp674
            retv618 = jp622
            return retv618
        }
    case Result__uint32__string_Err:
        var x13 string = mtmp11.(Result__uint32__string_Err)._0
        var error__17 string = x13
        var t684 Result__unit__string = Result__unit__string_Err{
            _0: error__17,
        }
        retv618 = t684
        return retv618
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__22 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var retv686 Result__string__string
    var t807 *ref_int_x = value__22.index
    var t808 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t807)
    var t809 string = value__22.input
    var t810 int = _goml_m_inherent_i_string_i_string_i_byte__len(t809)
    var t811 bool = t808 >= t810
    var jp799 bool
    if t811 {
        jp799 = true
    } else {
        var t812 string = value__22.input
        var t813 *ref_int_x = value__22.index
        var t814 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t813)
        var t815 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t812, t814)
        var t816 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t815, 34)
        var t817 bool = !t816
        jp799 = t817
    }
    if jp799 {
        var t800 string = _goml_m_std_p_json_p_json__error(value__22, "expected string")
        var t801 Result__string__string = Result__string__string_Err{
            _0: t800,
        }
        retv686 = t801
        return retv686
    } else {
        var t802 *ref_int_x = value__22.index
        var t803 *ref_int_x = value__22.index
        var t804 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t803)
        var t805 int = t804 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t802, t805)
        var builder__23 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t688 *ref_int_x = value__22.index
        var t689 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t688)
        var segment__24 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t689)
        Loop_loop693:
        for {
            var t694 *ref_int_x = value__22.index
            var t695 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t694)
            var t696 string = value__22.input
            var t697 int = _goml_m_inherent_i_string_i_string_i_byte__len(t696)
            var t698 bool = t695 < t697
            if t698 {
                var t699 string = value__22.input
                var t700 *ref_int_x = value__22.index
                var t701 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t700)
                var byte__25 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t699, t701)
                var t703 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 34)
                if t703 {
                    var t711 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                    var t712 *ref_int_x = value__22.index
                    var t713 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t712)
                    var t714 bool = t711 < t713
                    if t714 {
                        var t715 string = value__22.input
                        var t716 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t717 *ref_int_x = value__22.index
                        var t718 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t717)
                        var t719 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t715, t716, t718)
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t719)
                    } else {}
                    var t705 *ref_int_x = value__22.index
                    var t706 *ref_int_x = value__22.index
                    var t707 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t706)
                    var t708 int = t707 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t705, t708)
                    var t709 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__23)
                    var t710 Result__string__string = Result__string__string_Ok{
                        _0: t709,
                    }
                    retv686 = t710
                    return retv686
                } else {
                    var t722 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 92)
                    if t722 {
                        var t779 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t780 *ref_int_x = value__22.index
                        var t781 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t780)
                        var t782 bool = t779 < t781
                        if t782 {
                            var t783 string = value__22.input
                            var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                            var t785 *ref_int_x = value__22.index
                            var t786 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t785)
                            var t787 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t783, t784, t786)
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t787)
                        } else {}
                        var t724 *ref_int_x = value__22.index
                        var t725 *ref_int_x = value__22.index
                        var t726 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t725)
                        var t727 int = t726 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t724, t727)
                        var t772 *ref_int_x = value__22.index
                        var t773 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t772)
                        var t774 string = value__22.input
                        var t775 int = _goml_m_inherent_i_string_i_string_i_byte__len(t774)
                        var t776 bool = t773 >= t775
                        if t776 {
                            var t777 string = _goml_m_std_p_json_p_json__error(value__22, "incomplete escape")
                            var t778 Result__string__string = Result__string__string_Err{
                                _0: t777,
                            }
                            retv686 = t778
                            return retv686
                        } else {
                            var t729 string = value__22.input
                            var t730 *ref_int_x = value__22.index
                            var t731 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t730)
                            var escape__26 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t729, t731)
                            var t732 *ref_int_x = value__22.index
                            var t733 *ref_int_x = value__22.index
                            var t734 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t733)
                            var t735 int = t734 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t732, t735)
                            var t740 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 34)
                            if t740 {
                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 34)
                                var t737 *ref_int_x = value__22.index
                                var t738 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t737)
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t738)
                                continue
                            } else {
                                var t743 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 92)
                                if t743 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 92)
                                    var t737 *ref_int_x = value__22.index
                                    var t738 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t737)
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t738)
                                    continue
                                } else {
                                    var t746 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 47)
                                    if t746 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 47)
                                        var t737 *ref_int_x = value__22.index
                                        var t738 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t737)
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t738)
                                        continue
                                    } else {
                                        var t749 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 98)
                                        if t749 {
                                            var mtmp25 Option__char = char_from_uint32(8)
                                            switch mtmp25.(type) {
                                            case Option__char_None:
                                            case Option__char_Some:
                                                var x26 rune = mtmp25.(Option__char_Some)._0
                                                var character__27 rune = x26
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, character__27)
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                            var t737 *ref_int_x = value__22.index
                                            var t738 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t737)
                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t738)
                                            continue
                                        } else {
                                            var t753 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 102)
                                            if t753 {
                                                var mtmp27 Option__char = char_from_uint32(12)
                                                switch mtmp27.(type) {
                                                case Option__char_None:
                                                case Option__char_Some:
                                                    var x28 rune = mtmp27.(Option__char_Some)._0
                                                    var character__28 rune = x28
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, character__28)
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                                var t737 *ref_int_x = value__22.index
                                                var t738 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t737)
                                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t738)
                                                continue
                                            } else {
                                                var t757 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 110)
                                                if t757 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 10)
                                                    var t737 *ref_int_x = value__22.index
                                                    var t738 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t737)
                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t738)
                                                    continue
                                                } else {
                                                    var t760 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 114)
                                                    if t760 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 13)
                                                        var t737 *ref_int_x = value__22.index
                                                        var t738 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t737)
                                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t738)
                                                        continue
                                                    } else {
                                                        var t763 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 116)
                                                        if t763 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 9)
                                                            var t737 *ref_int_x = value__22.index
                                                            var t738 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t737)
                                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t738)
                                                            continue
                                                        } else {
                                                            var t766 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 117)
                                                            if t766 {
                                                                var mtmp29 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__22, builder__23)
                                                                switch mtmp29.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t737 *ref_int_x = value__22.index
                                                                    var t738 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t737)
                                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t738)
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x31 string = mtmp29.(Result__unit__string_Err)._0
                                                                    var error__29 string = x31
                                                                    var t769 Result__string__string = Result__string__string_Err{
                                                                        _0: error__29,
                                                                    }
                                                                    retv686 = t769
                                                                    return retv686
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t770 string = _goml_m_std_p_json_p_json__error(value__22, "invalid escape")
                                                                var t771 Result__string__string = Result__string__string_Err{
                                                                    _0: t770,
                                                                }
                                                                retv686 = t771
                                                                return retv686
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        var t790 bool = byte__25 < 32
                        if t790 {
                            var t791 string = _goml_m_std_p_json_p_json__error(value__22, "unescaped control character")
                            var t792 Result__string__string = Result__string__string_Err{
                                _0: t791,
                            }
                            retv686 = t792
                            return retv686
                        } else {
                            var t793 *ref_int_x = value__22.index
                            var t794 *ref_int_x = value__22.index
                            var t795 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t794)
                            var t796 int = t795 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t793, t796)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop693
            }
        }
        var t691 string = _goml_m_std_p_json_p_json__error(value__22, "unterminated string")
        var t692 Result__string__string = Result__string__string_Err{
            _0: t691,
        }
        retv686 = t692
        return retv686
    }
}

func _goml_m_std_p_json_p_json__digit(value__30 uint8) bool {
    var retv819 bool
    var t822 bool = value__30 >= 48
    var jp821 bool
    if t822 {
        var t823 bool = value__30 <= 57
        jp821 = t823
    } else {
        jp821 = false
    }
    retv819 = jp821
    return retv819
}

func _goml_m_std_p_json_p_parse__digits(value__31 _goml_m_std_p_json_p_JsonParser) bool {
    var retv825 bool
    var t826 *ref_int_x = value__31.index
    var start__32 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t826)
    Loop_loop831:
    for {
        var t839 *ref_int_x = value__31.index
        var t840 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t839)
        var t841 string = value__31.input
        var t842 int = _goml_m_inherent_i_string_i_string_i_byte__len(t841)
        var t843 bool = t840 < t842
        var jp833 bool
        if t843 {
            var t844 string = value__31.input
            var t845 *ref_int_x = value__31.index
            var t846 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t845)
            var t847 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t844, t846)
            var t848 bool = _goml_m_std_p_json_p_json__digit(t847)
            jp833 = t848
        } else {
            jp833 = false
        }
        if jp833 {
            var t834 *ref_int_x = value__31.index
            var t835 *ref_int_x = value__31.index
            var t836 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t835)
            var t837 int = t836 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t834, t837)
            continue
        } else {
            break Loop_loop831
        }
    }
    var t828 *ref_int_x = value__31.index
    var t829 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t828)
    var t830 bool = t829 > start__32
    retv825 = t830
    return retv825
}

func _goml_m_std_p_json_p_parse__json__number(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv850 _goml_m_Result____std_p_json_p_Value____string
    var t851 *ref_int_x = value__33.index
    var start__34 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
    var t973 string = value__33.input
    var t974 *ref_int_x = value__33.index
    var t975 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t974)
    var t976 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t973, t975)
    var t977 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t976, 45)
    if t977 {
        var t978 *ref_int_x = value__33.index
        var t979 *ref_int_x = value__33.index
        var t980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t979)
        var t981 int = t980 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t978, t981)
    } else {}
    var t936 *ref_int_x = value__33.index
    var t937 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t936)
    var t938 string = value__33.input
    var t939 int = _goml_m_inherent_i_string_i_string_i_byte__len(t938)
    var t940 bool = t937 >= t939
    if t940 {
        var t941 string = _goml_m_std_p_json_p_json__error(value__33, "incomplete number")
        var t942 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t941,
        }
        retv850 = t942
        return retv850
    } else {
        var t944 string = value__33.input
        var t945 *ref_int_x = value__33.index
        var t946 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t945)
        var t947 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t944, t946)
        var t948 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t947, 48)
        if t948 {
            var t949 *ref_int_x = value__33.index
            var t950 *ref_int_x = value__33.index
            var t951 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t950)
            var t952 int = t951 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t949, t952)
            var t958 *ref_int_x = value__33.index
            var t959 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t958)
            var t960 string = value__33.input
            var t961 int = _goml_m_inherent_i_string_i_string_i_byte__len(t960)
            var t962 bool = t959 < t961
            var jp955 bool
            if t962 {
                var t963 string = value__33.input
                var t964 *ref_int_x = value__33.index
                var t965 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t964)
                var t966 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t963, t965)
                var t967 bool = _goml_m_std_p_json_p_json__digit(t966)
                jp955 = t967
            } else {
                jp955 = false
            }
            if jp955 {
                var t956 string = _goml_m_std_p_json_p_json__error(value__33, "invalid leading zero")
                var t957 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t956,
                }
                retv850 = t957
                return retv850
            } else {
                var t926 *ref_int_x = value__33.index
                var t927 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t926)
                var t928 string = value__33.input
                var t929 int = _goml_m_inherent_i_string_i_string_i_byte__len(t928)
                var t930 bool = t927 < t929
                var jp916 bool
                if t930 {
                    var t931 string = value__33.input
                    var t932 *ref_int_x = value__33.index
                    var t933 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t932)
                    var t934 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t931, t933)
                    var t935 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t934, 46)
                    jp916 = t935
                } else {
                    jp916 = false
                }
                if jp916 {
                    var t917 *ref_int_x = value__33.index
                    var t918 *ref_int_x = value__33.index
                    var t919 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t918)
                    var t920 int = t919 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t917, t920)
                    var t922 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t923 bool = !t922
                    if t923 {
                        var t924 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t925 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t924,
                        }
                        retv850 = t925
                        return retv850
                    } else {
                        var t898 *ref_int_x = value__33.index
                        var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                        var t900 string = value__33.input
                        var t901 int = _goml_m_inherent_i_string_i_string_i_byte__len(t900)
                        var t902 bool = t899 < t901
                        var jp863 bool
                        if t902 {
                            var t905 string = value__33.input
                            var t906 *ref_int_x = value__33.index
                            var t907 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t906)
                            var t908 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t905, t907)
                            var t909 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t908, 101)
                            var jp904 bool
                            if t909 {
                                jp904 = true
                            } else {
                                var t910 string = value__33.input
                                var t911 *ref_int_x = value__33.index
                                var t912 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t911)
                                var t913 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t910, t912)
                                var t914 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t913, 69)
                                jp904 = t914
                            }
                            jp863 = jp904
                        } else {
                            jp863 = false
                        }
                        if jp863 {
                            var t864 *ref_int_x = value__33.index
                            var t865 *ref_int_x = value__33.index
                            var t866 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t865)
                            var t867 int = t866 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t864, t867)
                            var t881 *ref_int_x = value__33.index
                            var t882 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t881)
                            var t883 string = value__33.input
                            var t884 int = _goml_m_inherent_i_string_i_string_i_byte__len(t883)
                            var t885 bool = t882 < t884
                            var jp875 bool
                            if t885 {
                                var t888 string = value__33.input
                                var t889 *ref_int_x = value__33.index
                                var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                                var t891 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t888, t890)
                                var t892 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t891, 43)
                                var jp887 bool
                                if t892 {
                                    jp887 = true
                                } else {
                                    var t893 string = value__33.input
                                    var t894 *ref_int_x = value__33.index
                                    var t895 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t894)
                                    var t896 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t893, t895)
                                    var t897 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t896, 45)
                                    jp887 = t897
                                }
                                jp875 = jp887
                            } else {
                                jp875 = false
                            }
                            if jp875 {
                                var t876 *ref_int_x = value__33.index
                                var t877 *ref_int_x = value__33.index
                                var t878 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t877)
                                var t879 int = t878 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t876, t879)
                            } else {}
                            var t870 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t871 bool = !t870
                            if t871 {
                                var t872 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t873 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t872,
                                }
                                retv850 = t873
                                return retv850
                            } else {
                                var t856 string = value__33.input
                                var t857 *ref_int_x = value__33.index
                                var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                var t859 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t856, start__34, t858)
                                var t860 _goml_m_std_p_json_p_Value = Number{
                                    _0: t859,
                                }
                                var t861 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t860,
                                }
                                retv850 = t861
                                return retv850
                            }
                        } else {
                            var t856 string = value__33.input
                            var t857 *ref_int_x = value__33.index
                            var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                            var t859 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t856, start__34, t858)
                            var t860 _goml_m_std_p_json_p_Value = Number{
                                _0: t859,
                            }
                            var t861 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t860,
                            }
                            retv850 = t861
                            return retv850
                        }
                    }
                } else {
                    var t898 *ref_int_x = value__33.index
                    var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                    var t900 string = value__33.input
                    var t901 int = _goml_m_inherent_i_string_i_string_i_byte__len(t900)
                    var t902 bool = t899 < t901
                    var jp863 bool
                    if t902 {
                        var t905 string = value__33.input
                        var t906 *ref_int_x = value__33.index
                        var t907 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t906)
                        var t908 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t905, t907)
                        var t909 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t908, 101)
                        var jp904 bool
                        if t909 {
                            jp904 = true
                        } else {
                            var t910 string = value__33.input
                            var t911 *ref_int_x = value__33.index
                            var t912 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t911)
                            var t913 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t910, t912)
                            var t914 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t913, 69)
                            jp904 = t914
                        }
                        jp863 = jp904
                    } else {
                        jp863 = false
                    }
                    if jp863 {
                        var t864 *ref_int_x = value__33.index
                        var t865 *ref_int_x = value__33.index
                        var t866 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t865)
                        var t867 int = t866 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t864, t867)
                        var t881 *ref_int_x = value__33.index
                        var t882 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t881)
                        var t883 string = value__33.input
                        var t884 int = _goml_m_inherent_i_string_i_string_i_byte__len(t883)
                        var t885 bool = t882 < t884
                        var jp875 bool
                        if t885 {
                            var t888 string = value__33.input
                            var t889 *ref_int_x = value__33.index
                            var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                            var t891 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t888, t890)
                            var t892 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t891, 43)
                            var jp887 bool
                            if t892 {
                                jp887 = true
                            } else {
                                var t893 string = value__33.input
                                var t894 *ref_int_x = value__33.index
                                var t895 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t894)
                                var t896 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t893, t895)
                                var t897 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t896, 45)
                                jp887 = t897
                            }
                            jp875 = jp887
                        } else {
                            jp875 = false
                        }
                        if jp875 {
                            var t876 *ref_int_x = value__33.index
                            var t877 *ref_int_x = value__33.index
                            var t878 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t877)
                            var t879 int = t878 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t876, t879)
                        } else {}
                        var t870 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t871 bool = !t870
                        if t871 {
                            var t872 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t873 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t872,
                            }
                            retv850 = t873
                            return retv850
                        } else {
                            var t856 string = value__33.input
                            var t857 *ref_int_x = value__33.index
                            var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                            var t859 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t856, start__34, t858)
                            var t860 _goml_m_std_p_json_p_Value = Number{
                                _0: t859,
                            }
                            var t861 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t860,
                            }
                            retv850 = t861
                            return retv850
                        }
                    } else {
                        var t856 string = value__33.input
                        var t857 *ref_int_x = value__33.index
                        var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                        var t859 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t856, start__34, t858)
                        var t860 _goml_m_std_p_json_p_Value = Number{
                            _0: t859,
                        }
                        var t861 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t860,
                        }
                        retv850 = t861
                        return retv850
                    }
                }
            }
        } else {
            var t969 bool = _goml_m_std_p_json_p_parse__digits(value__33)
            var t970 bool = !t969
            if t970 {
                var t971 string = _goml_m_std_p_json_p_json__error(value__33, "expected number")
                var t972 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t971,
                }
                retv850 = t972
                return retv850
            } else {
                var t926 *ref_int_x = value__33.index
                var t927 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t926)
                var t928 string = value__33.input
                var t929 int = _goml_m_inherent_i_string_i_string_i_byte__len(t928)
                var t930 bool = t927 < t929
                var jp916 bool
                if t930 {
                    var t931 string = value__33.input
                    var t932 *ref_int_x = value__33.index
                    var t933 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t932)
                    var t934 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t931, t933)
                    var t935 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t934, 46)
                    jp916 = t935
                } else {
                    jp916 = false
                }
                if jp916 {
                    var t917 *ref_int_x = value__33.index
                    var t918 *ref_int_x = value__33.index
                    var t919 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t918)
                    var t920 int = t919 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t917, t920)
                    var t922 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t923 bool = !t922
                    if t923 {
                        var t924 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t925 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t924,
                        }
                        retv850 = t925
                        return retv850
                    } else {
                        var t898 *ref_int_x = value__33.index
                        var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                        var t900 string = value__33.input
                        var t901 int = _goml_m_inherent_i_string_i_string_i_byte__len(t900)
                        var t902 bool = t899 < t901
                        var jp863 bool
                        if t902 {
                            var t905 string = value__33.input
                            var t906 *ref_int_x = value__33.index
                            var t907 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t906)
                            var t908 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t905, t907)
                            var t909 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t908, 101)
                            var jp904 bool
                            if t909 {
                                jp904 = true
                            } else {
                                var t910 string = value__33.input
                                var t911 *ref_int_x = value__33.index
                                var t912 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t911)
                                var t913 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t910, t912)
                                var t914 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t913, 69)
                                jp904 = t914
                            }
                            jp863 = jp904
                        } else {
                            jp863 = false
                        }
                        if jp863 {
                            var t864 *ref_int_x = value__33.index
                            var t865 *ref_int_x = value__33.index
                            var t866 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t865)
                            var t867 int = t866 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t864, t867)
                            var t881 *ref_int_x = value__33.index
                            var t882 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t881)
                            var t883 string = value__33.input
                            var t884 int = _goml_m_inherent_i_string_i_string_i_byte__len(t883)
                            var t885 bool = t882 < t884
                            var jp875 bool
                            if t885 {
                                var t888 string = value__33.input
                                var t889 *ref_int_x = value__33.index
                                var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                                var t891 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t888, t890)
                                var t892 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t891, 43)
                                var jp887 bool
                                if t892 {
                                    jp887 = true
                                } else {
                                    var t893 string = value__33.input
                                    var t894 *ref_int_x = value__33.index
                                    var t895 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t894)
                                    var t896 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t893, t895)
                                    var t897 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t896, 45)
                                    jp887 = t897
                                }
                                jp875 = jp887
                            } else {
                                jp875 = false
                            }
                            if jp875 {
                                var t876 *ref_int_x = value__33.index
                                var t877 *ref_int_x = value__33.index
                                var t878 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t877)
                                var t879 int = t878 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t876, t879)
                            } else {}
                            var t870 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t871 bool = !t870
                            if t871 {
                                var t872 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t873 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t872,
                                }
                                retv850 = t873
                                return retv850
                            } else {
                                var t856 string = value__33.input
                                var t857 *ref_int_x = value__33.index
                                var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                var t859 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t856, start__34, t858)
                                var t860 _goml_m_std_p_json_p_Value = Number{
                                    _0: t859,
                                }
                                var t861 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t860,
                                }
                                retv850 = t861
                                return retv850
                            }
                        } else {
                            var t856 string = value__33.input
                            var t857 *ref_int_x = value__33.index
                            var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                            var t859 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t856, start__34, t858)
                            var t860 _goml_m_std_p_json_p_Value = Number{
                                _0: t859,
                            }
                            var t861 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t860,
                            }
                            retv850 = t861
                            return retv850
                        }
                    }
                } else {
                    var t898 *ref_int_x = value__33.index
                    var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                    var t900 string = value__33.input
                    var t901 int = _goml_m_inherent_i_string_i_string_i_byte__len(t900)
                    var t902 bool = t899 < t901
                    var jp863 bool
                    if t902 {
                        var t905 string = value__33.input
                        var t906 *ref_int_x = value__33.index
                        var t907 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t906)
                        var t908 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t905, t907)
                        var t909 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t908, 101)
                        var jp904 bool
                        if t909 {
                            jp904 = true
                        } else {
                            var t910 string = value__33.input
                            var t911 *ref_int_x = value__33.index
                            var t912 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t911)
                            var t913 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t910, t912)
                            var t914 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t913, 69)
                            jp904 = t914
                        }
                        jp863 = jp904
                    } else {
                        jp863 = false
                    }
                    if jp863 {
                        var t864 *ref_int_x = value__33.index
                        var t865 *ref_int_x = value__33.index
                        var t866 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t865)
                        var t867 int = t866 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t864, t867)
                        var t881 *ref_int_x = value__33.index
                        var t882 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t881)
                        var t883 string = value__33.input
                        var t884 int = _goml_m_inherent_i_string_i_string_i_byte__len(t883)
                        var t885 bool = t882 < t884
                        var jp875 bool
                        if t885 {
                            var t888 string = value__33.input
                            var t889 *ref_int_x = value__33.index
                            var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                            var t891 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t888, t890)
                            var t892 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t891, 43)
                            var jp887 bool
                            if t892 {
                                jp887 = true
                            } else {
                                var t893 string = value__33.input
                                var t894 *ref_int_x = value__33.index
                                var t895 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t894)
                                var t896 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t893, t895)
                                var t897 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t896, 45)
                                jp887 = t897
                            }
                            jp875 = jp887
                        } else {
                            jp875 = false
                        }
                        if jp875 {
                            var t876 *ref_int_x = value__33.index
                            var t877 *ref_int_x = value__33.index
                            var t878 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t877)
                            var t879 int = t878 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t876, t879)
                        } else {}
                        var t870 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t871 bool = !t870
                        if t871 {
                            var t872 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t873 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t872,
                            }
                            retv850 = t873
                            return retv850
                        } else {
                            var t856 string = value__33.input
                            var t857 *ref_int_x = value__33.index
                            var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                            var t859 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t856, start__34, t858)
                            var t860 _goml_m_std_p_json_p_Value = Number{
                                _0: t859,
                            }
                            var t861 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t860,
                            }
                            retv850 = t861
                            return retv850
                        }
                    } else {
                        var t856 string = value__33.input
                        var t857 *ref_int_x = value__33.index
                        var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                        var t859 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t856, start__34, t858)
                        var t860 _goml_m_std_p_json_p_Value = Number{
                            _0: t859,
                        }
                        var t861 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t860,
                        }
                        retv850 = t861
                        return retv850
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__35 _goml_m_std_p_json_p_JsonParser, expected__36 string, result__37 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var retv984 _goml_m_Result____std_p_json_p_Value____string
    var t997 *ref_int_x = value__35.index
    var t998 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t997)
    var t999 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
    var t1000 int = t998 + t999
    var t1001 string = value__35.input
    var t1002 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1001)
    var t1003 bool = t1000 <= t1002
    var jp988 bool
    if t1003 {
        var t1004 string = value__35.input
        var t1005 *ref_int_x = value__35.index
        var t1006 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1005)
        var t1007 *ref_int_x = value__35.index
        var t1008 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1007)
        var t1009 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t1010 int = t1008 + t1009
        var t1011 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1004, t1006, t1010)
        var t1012 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1011, expected__36)
        jp988 = t1012
    } else {
        jp988 = false
    }
    var jp986 _goml_m_Result____std_p_json_p_Value____string
    if jp988 {
        var t989 *ref_int_x = value__35.index
        var t990 *ref_int_x = value__35.index
        var t991 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t990)
        var t992 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t993 int = t991 + t992
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t989, t993)
        var t994 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__37,
        }
        jp986 = t994
    } else {
        var t995 string = _goml_m_std_p_json_p_json__error(value__35, "invalid literal")
        var t996 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t995,
        }
        jp986 = t996
    }
    retv984 = jp986
    return retv984
}

func _goml_m_std_p_json_p_parse__json__array(value__38 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv1014 _goml_m_Result____std_p_json_p_Value____string
    var t1015 *ref_int_x = value__38.index
    var t1016 *ref_int_x = value__38.index
    var t1017 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1016)
    var t1018 int = t1017 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1015, t1018)
    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
    var result__39 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t1073 *ref_int_x = value__38.index
    var t1074 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1073)
    var t1075 string = value__38.input
    var t1076 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1075)
    var t1077 bool = t1074 < t1076
    var jp1066 bool
    if t1077 {
        var t1078 string = value__38.input
        var t1079 *ref_int_x = value__38.index
        var t1080 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1079)
        var t1081 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1078, t1080)
        var t1082 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1081, 93)
        jp1066 = t1082
    } else {
        jp1066 = false
    }
    if jp1066 {
        var t1067 *ref_int_x = value__38.index
        var t1068 *ref_int_x = value__38.index
        var t1069 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1068)
        var t1070 int = t1069 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1067, t1070)
        var t1071 _goml_m_std_p_json_p_Value = Array{
            _0: result__39,
        }
        var t1072 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1071,
        }
        retv1014 = t1072
        return retv1014
    } else {
        Loop_loop1023:
        for {
            var t1024 *ref_int_x = value__38.index
            var t1025 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1024)
            var t1026 string = value__38.input
            var t1027 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1026)
            var t1028 bool = t1025 < t1027
            if t1028 {
                var mtmp48 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__38)
                switch mtmp48.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x49 _goml_m_std_p_json_p_Value = mtmp48.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    var item__40 _goml_m_std_p_json_p_Value = x49
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(result__39, item__40)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                    var t1031 *ref_int_x = value__38.index
                    var t1032 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1031)
                    var t1033 string = value__38.input
                    var t1034 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1033)
                    var t1035 bool = t1032 >= t1034
                    if t1035 {
                        var t1036 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
                        var t1037 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1036,
                        }
                        retv1014 = t1037
                        return retv1014
                    } else {
                        var t1039 string = value__38.input
                        var t1040 *ref_int_x = value__38.index
                        var t1041 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1040)
                        var t1042 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1039, t1041)
                        var t1043 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1042, 93)
                        if t1043 {
                            var t1044 *ref_int_x = value__38.index
                            var t1045 *ref_int_x = value__38.index
                            var t1046 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1045)
                            var t1047 int = t1046 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1044, t1047)
                            var t1048 _goml_m_std_p_json_p_Value = Array{
                                _0: result__39,
                            }
                            var t1049 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1048,
                            }
                            retv1014 = t1049
                            return retv1014
                        } else {
                            var t1051 string = value__38.input
                            var t1052 *ref_int_x = value__38.index
                            var t1053 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1052)
                            var t1054 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1051, t1053)
                            var t1055 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1054, 44)
                            if t1055 {
                                var t1056 *ref_int_x = value__38.index
                                var t1057 *ref_int_x = value__38.index
                                var t1058 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1057)
                                var t1059 int = t1058 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1056, t1059)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                                continue
                            } else {
                                var t1061 string = _goml_m_std_p_json_p_json__error(value__38, "expected array separator")
                                var t1062 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1061,
                                }
                                retv1014 = t1062
                                return retv1014
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x50 string = mtmp48.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var error__41 string = x50
                    var t1064 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__41,
                    }
                    retv1014 = t1064
                    return retv1014
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1023
            }
        }
        var t1021 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
        var t1022 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1021,
        }
        retv1014 = t1022
        return retv1014
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__42 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv1084 _goml_m_Result____std_p_json_p_Value____string
    var t1085 *ref_int_x = value__42.index
    var t1086 *ref_int_x = value__42.index
    var t1087 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1086)
    var t1088 int = t1087 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1085, t1088)
    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
    var result__43 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t1168 *ref_int_x = value__42.index
    var t1169 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1168)
    var t1170 string = value__42.input
    var t1171 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1170)
    var t1172 bool = t1169 < t1171
    var jp1161 bool
    if t1172 {
        var t1173 string = value__42.input
        var t1174 *ref_int_x = value__42.index
        var t1175 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1174)
        var t1176 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1173, t1175)
        var t1177 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1176, 125)
        jp1161 = t1177
    } else {
        jp1161 = false
    }
    if jp1161 {
        var t1162 *ref_int_x = value__42.index
        var t1163 *ref_int_x = value__42.index
        var t1164 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1163)
        var t1165 int = t1164 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1162, t1165)
        var t1166 _goml_m_std_p_json_p_Value = Object{
            _0: result__43,
        }
        var t1167 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1166,
        }
        retv1084 = t1167
        return retv1084
    } else {
        Loop_loop1093:
        for {
            var t1094 *ref_int_x = value__42.index
            var t1095 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1094)
            var t1096 string = value__42.input
            var t1097 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1096)
            var t1098 bool = t1095 < t1097
            if t1098 {
                var mtmp60 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__42)
                var jp1100 string
                switch mtmp60.(type) {
                case Result__string__string_Ok:
                    var x61 string = mtmp60.(Result__string__string_Ok)._0
                    var name__44 string = x61
                    jp1100 = name__44
                    var name__46 string = jp1100
                    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                    var t1148 *ref_int_x = value__42.index
                    var t1149 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1148)
                    var t1150 string = value__42.input
                    var t1151 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1150)
                    var t1152 bool = t1149 >= t1151
                    var jp1140 bool
                    if t1152 {
                        jp1140 = true
                    } else {
                        var t1153 string = value__42.input
                        var t1154 *ref_int_x = value__42.index
                        var t1155 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1154)
                        var t1156 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1153, t1155)
                        var t1157 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1156, 58)
                        var t1158 bool = !t1157
                        jp1140 = t1158
                    }
                    if jp1140 {
                        var t1141 string = _goml_m_std_p_json_p_json__error(value__42, "expected object colon")
                        var t1142 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1141,
                        }
                        retv1084 = t1142
                        return retv1084
                    } else {
                        var t1143 *ref_int_x = value__42.index
                        var t1144 *ref_int_x = value__42.index
                        var t1145 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1144)
                        var t1146 int = t1145 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1143, t1146)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                        var mtmp66 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__42)
                        switch mtmp66.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x67 _goml_m_std_p_json_p_Value = mtmp66.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            var item__47 _goml_m_std_p_json_p_Value = x67
                            var t1136 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: name__46,
                                _1: item__47,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__43, t1136)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                            var t1104 *ref_int_x = value__42.index
                            var t1105 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1104)
                            var t1106 string = value__42.input
                            var t1107 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1106)
                            var t1108 bool = t1105 >= t1107
                            if t1108 {
                                var t1109 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
                                var t1110 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1109,
                                }
                                retv1084 = t1110
                                return retv1084
                            } else {
                                var t1112 string = value__42.input
                                var t1113 *ref_int_x = value__42.index
                                var t1114 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1113)
                                var t1115 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1112, t1114)
                                var t1116 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1115, 125)
                                if t1116 {
                                    var t1117 *ref_int_x = value__42.index
                                    var t1118 *ref_int_x = value__42.index
                                    var t1119 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1118)
                                    var t1120 int = t1119 + 1
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1117, t1120)
                                    var t1121 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__43,
                                    }
                                    var t1122 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1121,
                                    }
                                    retv1084 = t1122
                                    return retv1084
                                } else {
                                    var t1124 string = value__42.input
                                    var t1125 *ref_int_x = value__42.index
                                    var t1126 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1125)
                                    var t1127 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1124, t1126)
                                    var t1128 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1127, 44)
                                    if t1128 {
                                        var t1129 *ref_int_x = value__42.index
                                        var t1130 *ref_int_x = value__42.index
                                        var t1131 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1130)
                                        var t1132 int = t1131 + 1
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1129, t1132)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                                        continue
                                    } else {
                                        var t1134 string = _goml_m_std_p_json_p_json__error(value__42, "expected object separator")
                                        var t1135 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1134,
                                        }
                                        retv1084 = t1135
                                        return retv1084
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x68 string = mtmp66.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var error__48 string = x68
                            var t1138 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: error__48,
                            }
                            retv1084 = t1138
                            return retv1084
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x62 string = mtmp60.(Result__string__string_Err)._0
                    var error__45 string = x62
                    var t1159 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__45,
                    }
                    retv1084 = t1159
                    return retv1084
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1093
            }
        }
        var t1091 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
        var t1092 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1091,
        }
        retv1084 = t1092
        return retv1084
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__49 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv1179 _goml_m_Result____std_p_json_p_Value____string
    _goml_m_std_p_json_p_skip__json__whitespace(value__49)
    var t1182 *ref_int_x = value__49.index
    var t1183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1182)
    var t1184 string = value__49.input
    var t1185 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1184)
    var t1186 bool = t1183 >= t1185
    var jp1181 _goml_m_Result____std_p_json_p_Value____string
    if t1186 {
        var t1187 string = _goml_m_std_p_json_p_json__error(value__49, "expected JSON value")
        var t1188 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1187,
        }
        jp1181 = t1188
    } else {
        var t1189 string = value__49.input
        var t1190 *ref_int_x = value__49.index
        var t1191 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1190)
        var mtmp75 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1189, t1191)
        var jp1193 _goml_m_Result____std_p_json_p_Value____string
        switch mtmp75 {
        case 123:
            var t1194 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__49)
            jp1193 = t1194
        case 91:
            var t1195 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__49)
            jp1193 = t1195
        case 34:
            var mtmp76 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__49)
            var jp1197 _goml_m_Result____std_p_json_p_Value____string
            switch mtmp76.(type) {
            case Result__string__string_Ok:
                var x77 string = mtmp76.(Result__string__string_Ok)._0
                var text__50 string = x77
                var t1198 _goml_m_std_p_json_p_Value = String{
                    _0: text__50,
                }
                var t1199 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1198,
                }
                jp1197 = t1199
            case Result__string__string_Err:
                var x78 string = mtmp76.(Result__string__string_Err)._0
                var error__51 string = x78
                var t1200 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: error__51,
                }
                jp1197 = t1200
            default:
                panic("non-exhaustive match")
            }
            jp1193 = jp1197
        case 116:
            var t1201 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t1202 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "true", t1201)
            jp1193 = t1202
        case 102:
            var t1203 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t1204 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "false", t1203)
            jp1193 = t1204
        case 110:
            var t1205 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "null", Null{})
            jp1193 = t1205
        default:
            var byte__52 uint8 = mtmp75
            var t1213 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 45)
            var jp1209 bool
            if t1213 {
                jp1209 = true
            } else {
                var t1214 bool = _goml_m_std_p_json_p_json__digit(byte__52)
                jp1209 = t1214
            }
            var jp1207 _goml_m_Result____std_p_json_p_Value____string
            if jp1209 {
                var t1210 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__49)
                jp1207 = t1210
            } else {
                var t1211 string = _goml_m_std_p_json_p_json__error(value__49, "unexpected JSON token")
                var t1212 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1211,
                }
                jp1207 = t1212
            }
            jp1193 = jp1207
        }
        jp1181 = jp1193
    }
    retv1179 = jp1181
    return retv1179
}

func _goml_m_std_p_json_p_parse(input__53 string) _goml_m_Result____std_p_json_p_Value____string {
    var retv1216 _goml_m_Result____std_p_json_p_Value____string
    var parser__54 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_json__parser(input__53)
    var mtmp79 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__54)
    var jp1218 _goml_m_std_p_json_p_Value
    switch mtmp79.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x80 _goml_m_std_p_json_p_Value = mtmp79.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__55 _goml_m_std_p_json_p_Value = x80
        jp1218 = value__55
        var result__57 _goml_m_std_p_json_p_Value = jp1218
        _goml_m_std_p_json_p_skip__json__whitespace(parser__54)
        var t1221 *ref_int_x = parser__54.index
        var t1222 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1221)
        var t1223 int = _goml_m_inherent_i_string_i_string_i_byte__len(input__53)
        var t1224 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1222, t1223)
        var jp1220 _goml_m_Result____std_p_json_p_Value____string
        if t1224 {
            var t1225 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: result__57,
            }
            jp1220 = t1225
        } else {
            var t1226 string = _goml_m_std_p_json_p_json__error(parser__54, "trailing JSON data")
            var t1227 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1226,
            }
            jp1220 = t1227
        }
        retv1216 = jp1220
        return retv1216
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x81 string = mtmp79.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var error__56 string = x81
        var t1228 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: error__56,
        }
        retv1216 = t1228
        return retv1216
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_json__hex__digit(value__58 uint8) rune {
    var retv1230 rune
    var t1231 int = int(uint8(value__58))
    var t1232 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", t1231)
    retv1230 = t1232
    return retv1230
}

func _goml_m_std_p_json_p_write__json__string(builder__59 _goml_m_std_p_text_p_StringBuilder, value__60 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, 34)
    var start__61 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var for_index84 int = 0
    var for_limit85 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    Loop_loop1244:
    for {
        var t1245 bool = for_index84 < for_limit85
        if t1245 {
            var for_item86 int = for_index84
            var t1246 int = for_index84 + 1
            for_index84 = t1246
            var index__62 int = for_item86
            var byte__63 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__60, index__62)
            var t1302 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
            var jp1300 bool
            if t1302 {
                jp1300 = true
            } else {
                var t1303 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                jp1300 = t1303
            }
            var jp1297 bool
            if jp1300 {
                jp1297 = true
            } else {
                var t1301 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                jp1297 = t1301
            }
            var jp1294 bool
            if jp1297 {
                jp1294 = true
            } else {
                var t1298 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                jp1294 = t1298
            }
            var jp1291 bool
            if jp1294 {
                jp1291 = true
            } else {
                var t1295 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                jp1291 = t1295
            }
            var jp1288 bool
            if jp1291 {
                jp1288 = true
            } else {
                var t1292 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                jp1288 = t1292
            }
            var jp1285 bool
            if jp1288 {
                jp1285 = true
            } else {
                var t1289 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                jp1285 = t1289
            }
            var jp1249 bool
            if jp1285 {
                jp1249 = true
            } else {
                var t1286 bool = byte__63 < 32
                jp1249 = t1286
            }
            if jp1249 {
                var t1279 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                var t1280 bool = t1279 < index__62
                if t1280 {
                    var t1281 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                    var t1282 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t1281, index__62)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t1282)
                } else {}
                var t1254 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
                if t1254 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\"")
                } else {
                    var t1257 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                    if t1257 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\\")
                    } else {
                        var t1260 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                        if t1260 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\b")
                        } else {
                            var t1263 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                            if t1263 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\t")
                            } else {
                                var t1266 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                                if t1266 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\n")
                                } else {
                                    var t1269 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                                    if t1269 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\f")
                                    } else {
                                        var t1272 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                                        if t1272 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\u00")
                                            var t1274 uint8 = byte__63 / 16
                                            var t1275 rune = _goml_m_std_p_json_p_json__hex__digit(t1274)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t1275)
                                            var t1276_rhs uint8 = 16
                                            var t1276 uint8 = byte__63 % t1276_rhs
                                            var t1277 rune = _goml_m_std_p_json_p_json__hex__digit(t1276)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t1277)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1252 int = index__62 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(start__61, t1252)
            } else {}
            continue
        } else {
            break Loop_loop1244
        }
    }
    var t1237 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
    var t1238 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    var t1239 bool = t1237 < t1238
    if t1239 {
        var t1240 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
        var t1241 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
        var t1242 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t1240, t1241)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t1242)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__64 _goml_m_std_p_text_p_StringBuilder, value__65 _goml_m_std_p_json_p_Value) struct{} {
    switch value__65.(type) {
    case Object:
        var x94 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__65.(Object)._0
        var fields__66 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x94
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 123)
        var index__67 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var for_source100 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = fields__66
        var for_limit101 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source100)
        var for_index102 int = 0
        Loop_loop1308:
        for {
            var t1309 bool = for_index102 < for_limit101
            if t1309 {
                var for_item103 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source100, for_index102)
                var t1310 int = for_index102 + 1
                for_index102 = t1310
                var field__68 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item103
                var t1317 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                var t1318 bool = t1317 > 0
                if t1318 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 44)
                } else {}
                var t1312 string = field__68._0
                _goml_m_std_p_json_p_write__json__string(builder__64, t1312)
                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 58)
                var t1313 _goml_m_std_p_json_p_Value = field__68._1
                _goml_m_std_p_json_p_write__json__value(builder__64, t1313)
                var t1314 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                var t1315 int = t1314 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__67, t1315)
                continue
            } else {
                break Loop_loop1308
            }
        }
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 125)
        return struct{}{}
    case Array:
        var x95 *_goml_vec__goml_m_std_p_json_p_Value = value__65.(Array)._0
        var items__69 *_goml_vec__goml_m_std_p_json_p_Value = x95
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 91)
        var index__70 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var for_source111 *_goml_vec__goml_m_std_p_json_p_Value = items__69
        var for_limit112 int = vec_len___goml_m_Vec__16std_p_json_p_Value(for_source111)
        var for_index113 int = 0
        Loop_loop1322:
        for {
            var t1323 bool = for_index113 < for_limit112
            if t1323 {
                var for_item114 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(for_source111, for_index113)
                var t1324 int = for_index113 + 1
                for_index113 = t1324
                var item__71 _goml_m_std_p_json_p_Value = for_item114
                var t1329 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__70)
                var t1330 bool = t1329 > 0
                if t1330 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 44)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__64, item__71)
                var t1326 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__70)
                var t1327 int = t1326 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__70, t1327)
                continue
            } else {
                break Loop_loop1322
            }
        }
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 93)
        return struct{}{}
    case String:
        var x96 string = value__65.(String)._0
        var text__72 string = x96
        _goml_m_std_p_json_p_write__json__string(builder__64, text__72)
        return struct{}{}
    case Number:
        var x97 string = value__65.(Number)._0
        var number__73 string = x97
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, number__73)
        return struct{}{}
    case Bool:
        var x98 bool = value__65.(Bool)._0
        var value__74 bool = x98
        var jp1335 string
        if value__74 {
            jp1335 = "true"
        } else {
            jp1335 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, jp1335)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__75 _goml_m_std_p_json_p_Value) string {
    var retv1339 string
    var builder__76 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
    _goml_m_std_p_json_p_write__json__value(builder__76, value__75)
    var t1340 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__76)
    retv1339 = t1340
    return retv1339
}

func _goml_m_std_p_json_p_field(value__77 _goml_m_std_p_json_p_Value, name__78 string) _goml_m_Option____std_p_json_p_Value {
    var retv1342 _goml_m_Option____std_p_json_p_Value
    var jp1344 _goml_m_Option____std_p_json_p_Value
    switch value__77.(type) {
    case Object:
        var x120 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__77.(Object)._0
        var fields__79 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x120
        var result__80 *ref__goml_m_Option____std_p_json_p_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(_goml_m_Option____std_p_json_p_Value_None{})
        var for_source125 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = fields__79
        var for_limit126 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source125)
        var for_index127 int = 0
        Loop_loop1347:
        for {
            var t1348 bool = for_index127 < for_limit126
            if t1348 {
                var for_item128 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source125, for_index127)
                var t1349 int = for_index127 + 1
                for_index127 = t1349
                var field__81 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item128
                var t1351 string = field__81._0
                var t1352 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1351, name__78)
                if t1352 {
                    var t1353 _goml_m_std_p_json_p_Value = field__81._1
                    var t1354 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1353,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(result__80, t1354)
                } else {}
                continue
            } else {
                break Loop_loop1347
            }
        }
        var t1346 _goml_m_Option____std_p_json_p_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_std_p_json_p_Value_r_(result__80)
        jp1344 = t1346
        retv1342 = jp1344
        return retv1342
    default:
        jp1344 = _goml_m_Option____std_p_json_p_Value_None{}
        retv1342 = jp1344
        return retv1342
    }
}

func _goml_m_std_p_json_p_as__string(value__82 _goml_m_std_p_json_p_Value) Option__string {
    var retv1357 Option__string
    var jp1359 Option__string
    switch value__82.(type) {
    case String:
        var x133 string = value__82.(String)._0
        var text__83 string = x133
        var t1360 Option__string = Option__string_Some{
            _0: text__83,
        }
        jp1359 = t1360
    default:
        jp1359 = Option__string_None{}
    }
    retv1357 = jp1359
    return retv1357
}

func _goml_m_std_p_json_p_parse__json__int__text(value__84 string) Option__int {
    var retv1362 Option__int
    var t1365 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
    var t1366 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1365, 0)
    var jp1364 Option__int
    if t1366 {
        jp1364 = Option__int_None{}
        retv1362 = jp1364
        return retv1362
    } else {
        var t1367 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, 0)
        var negative__85 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1367, 45)
        var jp1369 int
        if negative__85 {
            jp1369 = 1
        } else {
            jp1369 = 0
        }
        var index__86 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(jp1369)
        var result__87 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var t1397 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
        var t1398 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1399 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1397, t1398)
        if t1399 {
            retv1362 = Option__int_None{}
            return retv1362
        } else {
            Loop_loop1378:
            for {
                var t1379 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                var t1380 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                var t1381 bool = t1379 < t1380
                if t1381 {
                    var t1382 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                    var byte__88 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1382)
                    var t1395 bool = byte__88 < 48
                    var jp1388 bool
                    if t1395 {
                        jp1388 = true
                    } else {
                        var t1396 bool = byte__88 > 57
                        jp1388 = t1396
                    }
                    if jp1388 {
                        retv1362 = Option__int_None{}
                        return retv1362
                    } else {
                        var t1389 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                        var t1390 int = t1389 * 10
                        var t1391 uint8 = byte__88 - 48
                        var t1392 int = int(uint8(t1391))
                        var t1393 int = t1390 + t1392
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__87, t1393)
                        var t1384 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                        var t1385 int = t1384 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__86, t1385)
                        continue
                    }
                } else {
                    break Loop_loop1378
                }
            }
            var jp1373 int
            if negative__85 {
                var t1375 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                var t1376 int = 0 - t1375
                jp1373 = t1376
            } else {
                var t1377 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                jp1373 = t1377
            }
            var t1374 Option__int = Option__int_Some{
                _0: jp1373,
            }
            jp1364 = t1374
            retv1362 = jp1364
            return retv1362
        }
    }
}

func _goml_m_std_p_json_p_as__int(value__89 _goml_m_std_p_json_p_Value) Option__int {
    var retv1401 Option__int
    var jp1403 Option__int
    switch value__89.(type) {
    case Number:
        var x142 string = value__89.(Number)._0
        var number__90 string = x142
        var t1404 Option__int = _goml_m_std_p_json_p_parse__json__int__text(number__90)
        jp1403 = t1404
    default:
        jp1403 = Option__int_None{}
    }
    retv1401 = jp1403
    return retv1401
}

func _goml_m_std_p_json_p_as__bool(value__91 _goml_m_std_p_json_p_Value) Option__bool {
    var retv1406 Option__bool
    var jp1408 Option__bool
    switch value__91.(type) {
    case Bool:
        var x148 bool = value__91.(Bool)._0
        var result__92 bool = x148
        var t1409 Option__bool = Option__bool_Some{
            _0: result__92,
        }
        jp1408 = t1409
    default:
        jp1408 = Option__bool_None{}
    }
    retv1406 = jp1408
    return retv1406
}

func main0() struct{} {
    var mtmp108 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1420 _goml_m_std_p_json_p_Value
    switch mtmp108.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x109 _goml_m_std_p_json_p_Value = mtmp108.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__0 _goml_m_std_p_json_p_Value = x109
        jp1420 = value__0
        var value__2 _goml_m_std_p_json_p_Value = jp1420
        var mtmp112 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "name")
        switch mtmp112.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing name")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x113 _goml_m_std_p_json_p_Value = mtmp112.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__3 _goml_m_std_p_json_p_Value = x113
            var mtmp114 Option__string = _goml_m_std_p_json_p_as__string(field__3)
            switch mtmp114.(type) {
            case Option__string_None:
                println__T_string("invalid name")
            case Option__string_Some:
                var x115 string = mtmp114.(Option__string_Some)._0
                var name__4 string = x115
                println__T_string(name__4)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var mtmp117 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "version")
        switch mtmp117.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing version")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x118 _goml_m_std_p_json_p_Value = mtmp117.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__5 _goml_m_std_p_json_p_Value = x118
            var mtmp119 Option__int = _goml_m_std_p_json_p_as__int(field__5)
            switch mtmp119.(type) {
            case Option__int_None:
                println__T_string("invalid version")
            case Option__int_Some:
                var x120 int = mtmp119.(Option__int_Some)._0
                var version__6 int = x120
                println__T_int(version__6)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var mtmp122 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "stable")
        switch mtmp122.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing stable")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x123 _goml_m_std_p_json_p_Value = mtmp122.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__7 _goml_m_std_p_json_p_Value = x123
            var mtmp124 Option__bool = _goml_m_std_p_json_p_as__bool(field__7)
            switch mtmp124.(type) {
            case Option__bool_None:
                println__T_string("invalid stable")
            case Option__bool_Some:
                var x125 bool = mtmp124.(Option__bool_Some)._0
                var stable__8 bool = x125
                println__T_bool(stable__8)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var t1424 string = _goml_m_std_p_json_p_encode(value__2)
        println__T_string(t1424)
        return struct{}{}
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x110 string = mtmp108.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var error__1 string = x110
        println__T_string(error__1)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv1439 *_goml_vec_uint8
    var t1440 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv1439 = t1440
    return retv1439
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__126 *_goml_vec_uint8, elem__127 uint8) struct{} {
    vec_push__Vec_5uint8(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__uint8(self__140 *_goml_vec_uint8, additional__141 int) struct{} {
    vec_reserve__Vec_5uint8(self__140, additional__141)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv1482 bool
    var t1483 bool = self__59 == other__60
    retv1482 = t1483
    return retv1482
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv1485 int
    var t1486 int = _goml_runtime_core_string_len(self__9)
    retv1485 = t1486
    return retv1485
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv1488 uint8
    var t1489 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv1488 = t1489
    return retv1488
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv1491 string
    var t1492 string = _goml_runtime_core_char_to_string(self__7)
    retv1491 = t1492
    return retv1491
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv1494 bool
    var t1495 bool = self__69 == other__70
    retv1494 = t1495
    return retv1494
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv1497 *ref_int_x
    var t1498 *ref_int_x = ref__Ref_3int(value__207)
    retv1497 = t1498
    return retv1497
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv1500 int
    var t1501 int = ref_get__Ref_3int(self__208)
    retv1500 = t1501
    return retv1500
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv1506 string
    var t1507 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv1506 = t1507
    return retv1506
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv1509 bool
    var t1510 bool = self__55 == other__56
    retv1509 = t1510
    return retv1509
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv1546 string
    var t1547 string = _goml_runtime_core_int_to_string(self__5)
    retv1546 = t1547
    return retv1546
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__207 uint32) *ref_uint32_x {
    var retv1549 *ref_uint32_x
    var t1550 *ref_uint32_x = ref__Ref_6uint32(value__207)
    retv1549 = t1550
    return retv1549
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__208 *ref_uint32_x) uint32 {
    var retv1552 uint32
    var t1553 uint32 = ref_get__Ref_6uint32(self__208)
    retv1552 = t1553
    return retv1552
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(self__209 *ref_uint32_x, value__210 uint32) struct{} {
    ref_set__Ref_6uint32(self__209, value__210)
    return struct{}{}
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv1557 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp1559 Option__char
    if valid__3 {
        var t1560 Option__char = Option__char_Some{
            _0: value__4,
        }
        jp1559 = t1560
    } else {
        jp1559 = Option__char_None{}
    }
    retv1557 = jp1559
    return retv1557
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var retv1562 *_goml_vec__goml_m_std_p_json_p_Value
    var t1563 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    retv1562 = t1563
    return retv1562
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__126 *_goml_vec__goml_m_std_p_json_p_Value, elem__127 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var retv1567 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
    var t1568 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    retv1567 = t1568
    return retv1567
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__126 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__127 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv1572 rune
    var t1573 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv1572 = t1573
    return retv1572
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(value__207 _goml_m_Option____std_p_json_p_Value) *ref__goml_m_Option____std_p_json_p_Value_x {
    var retv1575 *ref__goml_m_Option____std_p_json_p_Value_x
    var t1576 *ref__goml_m_Option____std_p_json_p_Value_x = ref___goml_m_Ref__24Option____std_p_json_p_Value(value__207)
    retv1575 = t1576
    return retv1575
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(self__209 *ref__goml_m_Option____std_p_json_p_Value_x, value__210 _goml_m_Option____std_p_json_p_Value) struct{} {
    ref_set___goml_m_Ref__24Option____std_p_json_p_Value(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_std_p_json_p_Value_r_(self__208 *ref__goml_m_Option____std_p_json_p_Value_x) _goml_m_Option____std_p_json_p_Value {
    var retv1580 _goml_m_Option____std_p_json_p_Value
    var t1581 _goml_m_Option____std_p_json_p_Value = ref_get___goml_m_Ref__24Option____std_p_json_p_Value(self__208)
    retv1580 = t1581
    return retv1580
}

func println__T_string(value__1 string) struct{} {
    var t1583 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t1583)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t1586 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t1586)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t1589 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t1589)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv1594 string
    retv1594 = self__38
    return retv1594
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv1596 string
    var t1597 string = _goml_runtime_core_int_to_string(self__40)
    retv1596 = t1597
    return retv1596
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv1599 string
    var t1600 string = _goml_runtime_core_bool_to_string(self__37)
    retv1599 = t1600
    return retv1599
}

func main() {
    main0()
}
