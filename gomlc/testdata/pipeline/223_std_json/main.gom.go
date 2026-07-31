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
    var retv234 _goml_m_std_p_text_p_StringBuilder
    var vec_literal__178 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var t235 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    retv234 = t235
    return retv234
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    var t249 *_goml_vec_uint8 = self__3.values
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__uint8(t249, length__5)
    var for_index1 int = 0
    var for_limit2 int = length__5
    Loop_loop251:
    for {
        var t252 bool = for_index1 < for_limit2
        if t252 {
            var for_item3 int = for_index1
            var t253 int = for_index1 + 1
            for_index1 = t253
            var index__6 int = for_item3
            var t254 *_goml_vec_uint8 = self__3.values
            var t255 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__4, index__6)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(t254, t255)
            continue
        } else {
            break Loop_loop251
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t258 string = _goml_m_inherent_i_char_i_char_i_to__string(value__8)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t258)
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(self__16 _goml_m_std_p_text_p_StringBuilder) string {
    var retv272 string
    var t273 *_goml_vec_uint8 = self__16.values
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t273)
    var x12 string = mtmp10._1
    var value__17 string = x12
    retv272 = value__17
    return retv272
}

func _goml_m_std_p_json_p_json__parser(input__0 string) _goml_m_std_p_json_p_JsonParser {
    var retv503 _goml_m_std_p_json_p_JsonParser
    var t504 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t505 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__0,
        index: t504,
    }
    retv503 = t505
    return retv503
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var retv507 string
    var t508 string = "" + message__2
    var t509 string = t508 + " at byte "
    var t510 *ref_int_x = value__1.index
    var t511 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t510)
    var t512 string = _goml_m_inherent_i_int_i_int_i_to__string(t511)
    var t513 string = t509 + t512
    retv507 = t513
    return retv507
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
    var t575 bool = value__5 >= 48
    var jp551 bool
    if t575 {
        var t576 bool = value__5 <= 57
        jp551 = t576
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
        var t573 bool = value__5 >= 65
        var jp558 bool
        if t573 {
            var t574 bool = value__5 <= 70
            jp558 = t574
        } else {
            jp558 = false
        }
        var jp556 Option__uint32
        if jp558 {
            var t559 uint8 = value__5 - 65
            var t560 uint8 = t559 + 10
            var t561 uint32 = uint32(uint8(t560))
            var t562 Option__uint32 = Option__uint32_Some{
                _0: t561,
            }
            jp556 = t562
        } else {
            var t571 bool = value__5 >= 97
            var jp566 bool
            if t571 {
                var t572 bool = value__5 <= 102
                jp566 = t572
            } else {
                jp566 = false
            }
            var jp564 Option__uint32
            if jp566 {
                var t567 uint8 = value__5 - 97
                var t568 uint8 = t567 + 10
                var t569 uint32 = uint32(uint8(t568))
                var t570 Option__uint32 = Option__uint32_Some{
                    _0: t569,
                }
                jp564 = t570
            } else {
                jp564 = Option__uint32_None{}
            }
            jp556 = jp564
        }
        jp549 = jp556
    }
    retv547 = jp549
    return retv547
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var retv578 Result__uint32__string
    var t581 *ref_int_x = value__6.index
    var t582 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t581)
    var t583 int = t582 + 4
    var t584 string = value__6.input
    var t585 int = _goml_m_inherent_i_string_i_string_i_byte__len(t584)
    var t586 bool = t583 > t585
    var jp580 Result__uint32__string
    if t586 {
        var t587 string = _goml_m_std_p_json_p_json__error(value__6, "incomplete unicode escape")
        var t588 Result__uint32__string = Result__uint32__string_Err{
            _0: t587,
        }
        jp580 = t588
        retv578 = jp580
        return retv578
    } else {
        var result__7_source int = 0
        var result__7 uint32 = uint32(int(result__7_source))
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
                    retv578 = t605
                    return retv578
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var digit__9 uint32 = x5
                    var t606 uint32 = result__7 * 16
                    var t607 uint32 = t606 + digit__9
                    result__7 = t607
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop595
            }
        }
        var t590 *ref_int_x = value__6.index
        var t591 *ref_int_x = value__6.index
        var t592 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t591)
        var t593 int = t592 + 4
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t590, t593)
        var t594 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__7,
        }
        jp580 = t594
        retv578 = jp580
        return retv578
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var retv609 Result__unit__string
    var mtmp9 Option__char = char_from_uint32(codepoint__12)
    var jp611 Result__unit__string
    switch mtmp9.(type) {
    case Option__char_None:
        var t612 string = _goml_m_std_p_json_p_json__error(value__10, "invalid unicode codepoint")
        var t613 Result__unit__string = Result__unit__string_Err{
            _0: t612,
        }
        jp611 = t613
    case Option__char_Some:
        var x10 rune = mtmp9.(Option__char_Some)._0
        var character__13 rune = x10
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__11, character__13)
        var t614 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp611 = t614
    default:
        panic("non-exhaustive match")
    }
    retv609 = jp611
    return retv609
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var retv616 Result__unit__string
    var mtmp12 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp618 uint32
    switch mtmp12.(type) {
    case Result__uint32__string_Ok:
        var x13 uint32 = mtmp12.(Result__uint32__string_Ok)._0
        var try_value__191 uint32 = x13
        jp618 = try_value__191
        var first__16 uint32 = jp618
        var t680 bool = first__16 >= 55296
        var jp622 bool
        if t680 {
            var t681 bool = first__16 <= 56319
            jp622 = t681
        } else {
            jp622 = false
        }
        var jp620 Result__unit__string
        if jp622 {
            var t659 *ref_int_x = value__14.index
            var t660 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t659)
            var t661 int = t660 + 2
            var t662 string = value__14.input
            var t663 int = _goml_m_inherent_i_string_i_string_i_byte__len(t662)
            var t664 bool = t661 > t663
            var jp651 bool
            if t664 {
                jp651 = true
            } else {
                var t665 string = value__14.input
                var t666 *ref_int_x = value__14.index
                var t667 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t666)
                var t668 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t665, t667)
                var t669 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t668, 92)
                var t670 bool = !t669
                jp651 = t670
            }
            var jp626 bool
            if jp651 {
                jp626 = true
            } else {
                var t652 string = value__14.input
                var t653 *ref_int_x = value__14.index
                var t654 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t653)
                var t655 int = t654 + 1
                var t656 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t652, t655)
                var t657 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t656, 117)
                var t658 bool = !t657
                jp626 = t658
            }
            var jp624 Result__unit__string
            if jp626 {
                var t627 string = _goml_m_std_p_json_p_json__error(value__14, "missing low surrogate")
                var t628 Result__unit__string = Result__unit__string_Err{
                    _0: t627,
                }
                jp624 = t628
                jp620 = jp624
                retv616 = jp620
                return retv616
            } else {
                var t629 *ref_int_x = value__14.index
                var t630 *ref_int_x = value__14.index
                var t631 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t630)
                var t632 int = t631 + 2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t629, t632)
                var mtmp16 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp634 uint32
                switch mtmp16.(type) {
                case Result__uint32__string_Ok:
                    var x17 uint32 = mtmp16.(Result__uint32__string_Ok)._0
                    var try_value__253 uint32 = x17
                    jp634 = try_value__253
                    var second__17 uint32 = jp634
                    var t647 bool = second__17 < 56320
                    var jp638 bool
                    if t647 {
                        jp638 = true
                    } else {
                        var t648 bool = second__17 > 57343
                        jp638 = t648
                    }
                    var jp636 Result__unit__string
                    if jp638 {
                        var t639 string = _goml_m_std_p_json_p_json__error(value__14, "invalid low surrogate")
                        var t640 Result__unit__string = Result__unit__string_Err{
                            _0: t639,
                        }
                        jp636 = t640
                    } else {
                        var t641 uint32 = first__16 - 55296
                        var t642 uint32 = t641 * 1024
                        var t643 uint32 = 65536 + t642
                        var t644 uint32 = t643 + second__17
                        var t645 uint32 = t644 - 56320
                        var t646 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, t645)
                        jp636 = t646
                    }
                    jp624 = jp636
                    jp620 = jp624
                    retv616 = jp620
                    return retv616
                case Result__uint32__string_Err:
                    var x18 string = mtmp16.(Result__uint32__string_Err)._0
                    var try_residual__253 string = x18
                    var t649 Result__unit__string = Result__unit__string_Err{
                        _0: try_residual__253,
                    }
                    retv616 = t649
                    return retv616
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t678 bool = first__16 >= 56320
            var jp674 bool
            if t678 {
                var t679 bool = first__16 <= 57343
                jp674 = t679
            } else {
                jp674 = false
            }
            var jp672 Result__unit__string
            if jp674 {
                var t675 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t676 Result__unit__string = Result__unit__string_Err{
                    _0: t675,
                }
                jp672 = t676
            } else {
                var t677 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, first__16)
                jp672 = t677
            }
            jp620 = jp672
            retv616 = jp620
            return retv616
        }
    case Result__uint32__string_Err:
        var x14 string = mtmp12.(Result__uint32__string_Err)._0
        var try_residual__191 string = x14
        var t682 Result__unit__string = Result__unit__string_Err{
            _0: try_residual__191,
        }
        retv616 = t682
        return retv616
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__18 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var retv684 Result__string__string
    var t798 *ref_int_x = value__18.index
    var t799 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t798)
    var t800 string = value__18.input
    var t801 int = _goml_m_inherent_i_string_i_string_i_byte__len(t800)
    var t802 bool = t799 >= t801
    var jp790 bool
    if t802 {
        jp790 = true
    } else {
        var t803 string = value__18.input
        var t804 *ref_int_x = value__18.index
        var t805 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t804)
        var t806 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t803, t805)
        var t807 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t806, 34)
        var t808 bool = !t807
        jp790 = t808
    }
    if jp790 {
        var t791 string = _goml_m_std_p_json_p_json__error(value__18, "expected string")
        var t792 Result__string__string = Result__string__string_Err{
            _0: t791,
        }
        retv684 = t792
        return retv684
    } else {
        var t793 *ref_int_x = value__18.index
        var t794 *ref_int_x = value__18.index
        var t795 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t794)
        var t796 int = t795 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t793, t796)
        var builder__19 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t686 *ref_int_x = value__18.index
        var segment__20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t686)
        Loop_loop690:
        for {
            var t691 *ref_int_x = value__18.index
            var t692 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t691)
            var t693 string = value__18.input
            var t694 int = _goml_m_inherent_i_string_i_string_i_byte__len(t693)
            var t695 bool = t692 < t694
            if t695 {
                var t696 string = value__18.input
                var t697 *ref_int_x = value__18.index
                var t698 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t697)
                var byte__21 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t696, t698)
                var t700 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 34)
                if t700 {
                    var t708 *ref_int_x = value__18.index
                    var t709 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t708)
                    var t710 bool = segment__20 < t709
                    if t710 {
                        var t711 string = value__18.input
                        var t712 *ref_int_x = value__18.index
                        var t713 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t712)
                        var t714 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t711, segment__20, t713)
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t714)
                    } else {}
                    var t702 *ref_int_x = value__18.index
                    var t703 *ref_int_x = value__18.index
                    var t704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t703)
                    var t705 int = t704 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t702, t705)
                    var t706 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__19)
                    var t707 Result__string__string = Result__string__string_Ok{
                        _0: t706,
                    }
                    retv684 = t707
                    return retv684
                } else {
                    var t717 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 92)
                    if t717 {
                        var t772 *ref_int_x = value__18.index
                        var t773 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t772)
                        var t774 bool = segment__20 < t773
                        if t774 {
                            var t775 string = value__18.input
                            var t776 *ref_int_x = value__18.index
                            var t777 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t776)
                            var t778 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t775, segment__20, t777)
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t778)
                        } else {}
                        var t719 *ref_int_x = value__18.index
                        var t720 *ref_int_x = value__18.index
                        var t721 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t720)
                        var t722 int = t721 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t719, t722)
                        var t765 *ref_int_x = value__18.index
                        var t766 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t765)
                        var t767 string = value__18.input
                        var t768 int = _goml_m_inherent_i_string_i_string_i_byte__len(t767)
                        var t769 bool = t766 >= t768
                        if t769 {
                            var t770 string = _goml_m_std_p_json_p_json__error(value__18, "incomplete escape")
                            var t771 Result__string__string = Result__string__string_Err{
                                _0: t770,
                            }
                            retv684 = t771
                            return retv684
                        } else {
                            var t724 string = value__18.input
                            var t725 *ref_int_x = value__18.index
                            var t726 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t725)
                            var escape__22 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t724, t726)
                            var t727 *ref_int_x = value__18.index
                            var t728 *ref_int_x = value__18.index
                            var t729 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t728)
                            var t730 int = t729 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t727, t730)
                            var t734 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 34)
                            if t734 {
                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 34)
                                var t732 *ref_int_x = value__18.index
                                var t733 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t732)
                                segment__20 = t733
                                continue
                            } else {
                                var t737 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 92)
                                if t737 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 92)
                                    var t732 *ref_int_x = value__18.index
                                    var t733 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t732)
                                    segment__20 = t733
                                    continue
                                } else {
                                    var t740 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 47)
                                    if t740 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 47)
                                        var t732 *ref_int_x = value__18.index
                                        var t733 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t732)
                                        segment__20 = t733
                                        continue
                                    } else {
                                        var t743 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 98)
                                        if t743 {
                                            var mtmp26 Option__char = char_from_uint32(8)
                                            switch mtmp26.(type) {
                                            case Option__char_None:
                                            case Option__char_Some:
                                                var x27 rune = mtmp26.(Option__char_Some)._0
                                                var character__23 rune = x27
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, character__23)
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                            var t732 *ref_int_x = value__18.index
                                            var t733 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t732)
                                            segment__20 = t733
                                            continue
                                        } else {
                                            var t747 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 102)
                                            if t747 {
                                                var mtmp28 Option__char = char_from_uint32(12)
                                                switch mtmp28.(type) {
                                                case Option__char_None:
                                                case Option__char_Some:
                                                    var x29 rune = mtmp28.(Option__char_Some)._0
                                                    var character__24 rune = x29
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, character__24)
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                                var t732 *ref_int_x = value__18.index
                                                var t733 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t732)
                                                segment__20 = t733
                                                continue
                                            } else {
                                                var t751 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 110)
                                                if t751 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 10)
                                                    var t732 *ref_int_x = value__18.index
                                                    var t733 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t732)
                                                    segment__20 = t733
                                                    continue
                                                } else {
                                                    var t754 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 114)
                                                    if t754 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 13)
                                                        var t732 *ref_int_x = value__18.index
                                                        var t733 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t732)
                                                        segment__20 = t733
                                                        continue
                                                    } else {
                                                        var t757 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 116)
                                                        if t757 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 9)
                                                            var t732 *ref_int_x = value__18.index
                                                            var t733 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t732)
                                                            segment__20 = t733
                                                            continue
                                                        } else {
                                                            var t760 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 117)
                                                            if t760 {
                                                                var mtmp30 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__18, builder__19)
                                                                switch mtmp30.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t732 *ref_int_x = value__18.index
                                                                    var t733 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t732)
                                                                    segment__20 = t733
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x32 string = mtmp30.(Result__unit__string_Err)._0
                                                                    var try_residual__564 string = x32
                                                                    var t762 Result__string__string = Result__string__string_Err{
                                                                        _0: try_residual__564,
                                                                    }
                                                                    retv684 = t762
                                                                    return retv684
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t763 string = _goml_m_std_p_json_p_json__error(value__18, "invalid escape")
                                                                var t764 Result__string__string = Result__string__string_Err{
                                                                    _0: t763,
                                                                }
                                                                retv684 = t764
                                                                return retv684
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
                        var t781 bool = byte__21 < 32
                        if t781 {
                            var t782 string = _goml_m_std_p_json_p_json__error(value__18, "unescaped control character")
                            var t783 Result__string__string = Result__string__string_Err{
                                _0: t782,
                            }
                            retv684 = t783
                            return retv684
                        } else {
                            var t784 *ref_int_x = value__18.index
                            var t785 *ref_int_x = value__18.index
                            var t786 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t785)
                            var t787 int = t786 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t784, t787)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop690
            }
        }
        var t688 string = _goml_m_std_p_json_p_json__error(value__18, "unterminated string")
        var t689 Result__string__string = Result__string__string_Err{
            _0: t688,
        }
        retv684 = t689
        return retv684
    }
}

func _goml_m_std_p_json_p_json__digit(value__25 uint8) bool {
    var retv810 bool
    var t813 bool = value__25 >= 48
    var jp812 bool
    if t813 {
        var t814 bool = value__25 <= 57
        jp812 = t814
    } else {
        jp812 = false
    }
    retv810 = jp812
    return retv810
}

func _goml_m_std_p_json_p_parse__digits(value__26 _goml_m_std_p_json_p_JsonParser) bool {
    var retv816 bool
    var t817 *ref_int_x = value__26.index
    var start__27 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t817)
    Loop_loop822:
    for {
        var t830 *ref_int_x = value__26.index
        var t831 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t830)
        var t832 string = value__26.input
        var t833 int = _goml_m_inherent_i_string_i_string_i_byte__len(t832)
        var t834 bool = t831 < t833
        var jp824 bool
        if t834 {
            var t835 string = value__26.input
            var t836 *ref_int_x = value__26.index
            var t837 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t836)
            var t838 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t835, t837)
            var t839 bool = _goml_m_std_p_json_p_json__digit(t838)
            jp824 = t839
        } else {
            jp824 = false
        }
        if jp824 {
            var t825 *ref_int_x = value__26.index
            var t826 *ref_int_x = value__26.index
            var t827 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t826)
            var t828 int = t827 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t825, t828)
            continue
        } else {
            break Loop_loop822
        }
    }
    var t819 *ref_int_x = value__26.index
    var t820 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t819)
    var t821 bool = t820 > start__27
    retv816 = t821
    return retv816
}

func _goml_m_std_p_json_p_parse__json__number(value__28 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv841 _goml_m_Result____std_p_json_p_Value____string
    var t842 *ref_int_x = value__28.index
    var start__29 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t842)
    var t964 string = value__28.input
    var t965 *ref_int_x = value__28.index
    var t966 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t965)
    var t967 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t964, t966)
    var t968 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t967, 45)
    if t968 {
        var t969 *ref_int_x = value__28.index
        var t970 *ref_int_x = value__28.index
        var t971 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t970)
        var t972 int = t971 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t969, t972)
    } else {}
    var t927 *ref_int_x = value__28.index
    var t928 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t927)
    var t929 string = value__28.input
    var t930 int = _goml_m_inherent_i_string_i_string_i_byte__len(t929)
    var t931 bool = t928 >= t930
    if t931 {
        var t932 string = _goml_m_std_p_json_p_json__error(value__28, "incomplete number")
        var t933 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t932,
        }
        retv841 = t933
        return retv841
    } else {
        var t935 string = value__28.input
        var t936 *ref_int_x = value__28.index
        var t937 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t936)
        var t938 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t935, t937)
        var t939 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t938, 48)
        if t939 {
            var t940 *ref_int_x = value__28.index
            var t941 *ref_int_x = value__28.index
            var t942 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t941)
            var t943 int = t942 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t940, t943)
            var t949 *ref_int_x = value__28.index
            var t950 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t949)
            var t951 string = value__28.input
            var t952 int = _goml_m_inherent_i_string_i_string_i_byte__len(t951)
            var t953 bool = t950 < t952
            var jp946 bool
            if t953 {
                var t954 string = value__28.input
                var t955 *ref_int_x = value__28.index
                var t956 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t955)
                var t957 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t954, t956)
                var t958 bool = _goml_m_std_p_json_p_json__digit(t957)
                jp946 = t958
            } else {
                jp946 = false
            }
            if jp946 {
                var t947 string = _goml_m_std_p_json_p_json__error(value__28, "invalid leading zero")
                var t948 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t947,
                }
                retv841 = t948
                return retv841
            } else {
                var t917 *ref_int_x = value__28.index
                var t918 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t917)
                var t919 string = value__28.input
                var t920 int = _goml_m_inherent_i_string_i_string_i_byte__len(t919)
                var t921 bool = t918 < t920
                var jp907 bool
                if t921 {
                    var t922 string = value__28.input
                    var t923 *ref_int_x = value__28.index
                    var t924 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t923)
                    var t925 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t922, t924)
                    var t926 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t925, 46)
                    jp907 = t926
                } else {
                    jp907 = false
                }
                if jp907 {
                    var t908 *ref_int_x = value__28.index
                    var t909 *ref_int_x = value__28.index
                    var t910 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t909)
                    var t911 int = t910 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t908, t911)
                    var t913 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t914 bool = !t913
                    if t914 {
                        var t915 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t916 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t915,
                        }
                        retv841 = t916
                        return retv841
                    } else {
                        var t889 *ref_int_x = value__28.index
                        var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                        var t891 string = value__28.input
                        var t892 int = _goml_m_inherent_i_string_i_string_i_byte__len(t891)
                        var t893 bool = t890 < t892
                        var jp854 bool
                        if t893 {
                            var t896 string = value__28.input
                            var t897 *ref_int_x = value__28.index
                            var t898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t897)
                            var t899 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t896, t898)
                            var t900 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t899, 101)
                            var jp895 bool
                            if t900 {
                                jp895 = true
                            } else {
                                var t901 string = value__28.input
                                var t902 *ref_int_x = value__28.index
                                var t903 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t902)
                                var t904 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t901, t903)
                                var t905 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t904, 69)
                                jp895 = t905
                            }
                            jp854 = jp895
                        } else {
                            jp854 = false
                        }
                        if jp854 {
                            var t855 *ref_int_x = value__28.index
                            var t856 *ref_int_x = value__28.index
                            var t857 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t856)
                            var t858 int = t857 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t855, t858)
                            var t872 *ref_int_x = value__28.index
                            var t873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t872)
                            var t874 string = value__28.input
                            var t875 int = _goml_m_inherent_i_string_i_string_i_byte__len(t874)
                            var t876 bool = t873 < t875
                            var jp866 bool
                            if t876 {
                                var t879 string = value__28.input
                                var t880 *ref_int_x = value__28.index
                                var t881 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t880)
                                var t882 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t879, t881)
                                var t883 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t882, 43)
                                var jp878 bool
                                if t883 {
                                    jp878 = true
                                } else {
                                    var t884 string = value__28.input
                                    var t885 *ref_int_x = value__28.index
                                    var t886 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t885)
                                    var t887 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t884, t886)
                                    var t888 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t887, 45)
                                    jp878 = t888
                                }
                                jp866 = jp878
                            } else {
                                jp866 = false
                            }
                            if jp866 {
                                var t867 *ref_int_x = value__28.index
                                var t868 *ref_int_x = value__28.index
                                var t869 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t868)
                                var t870 int = t869 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t867, t870)
                            } else {}
                            var t861 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t862 bool = !t861
                            if t862 {
                                var t863 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t864 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t863,
                                }
                                retv841 = t864
                                return retv841
                            } else {
                                var t847 string = value__28.input
                                var t848 *ref_int_x = value__28.index
                                var t849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t848)
                                var t850 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t847, start__29, t849)
                                var t851 _goml_m_std_p_json_p_Value = Number{
                                    _0: t850,
                                }
                                var t852 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t851,
                                }
                                retv841 = t852
                                return retv841
                            }
                        } else {
                            var t847 string = value__28.input
                            var t848 *ref_int_x = value__28.index
                            var t849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t848)
                            var t850 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t847, start__29, t849)
                            var t851 _goml_m_std_p_json_p_Value = Number{
                                _0: t850,
                            }
                            var t852 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t851,
                            }
                            retv841 = t852
                            return retv841
                        }
                    }
                } else {
                    var t889 *ref_int_x = value__28.index
                    var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                    var t891 string = value__28.input
                    var t892 int = _goml_m_inherent_i_string_i_string_i_byte__len(t891)
                    var t893 bool = t890 < t892
                    var jp854 bool
                    if t893 {
                        var t896 string = value__28.input
                        var t897 *ref_int_x = value__28.index
                        var t898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t897)
                        var t899 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t896, t898)
                        var t900 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t899, 101)
                        var jp895 bool
                        if t900 {
                            jp895 = true
                        } else {
                            var t901 string = value__28.input
                            var t902 *ref_int_x = value__28.index
                            var t903 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t902)
                            var t904 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t901, t903)
                            var t905 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t904, 69)
                            jp895 = t905
                        }
                        jp854 = jp895
                    } else {
                        jp854 = false
                    }
                    if jp854 {
                        var t855 *ref_int_x = value__28.index
                        var t856 *ref_int_x = value__28.index
                        var t857 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t856)
                        var t858 int = t857 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t855, t858)
                        var t872 *ref_int_x = value__28.index
                        var t873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t872)
                        var t874 string = value__28.input
                        var t875 int = _goml_m_inherent_i_string_i_string_i_byte__len(t874)
                        var t876 bool = t873 < t875
                        var jp866 bool
                        if t876 {
                            var t879 string = value__28.input
                            var t880 *ref_int_x = value__28.index
                            var t881 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t880)
                            var t882 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t879, t881)
                            var t883 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t882, 43)
                            var jp878 bool
                            if t883 {
                                jp878 = true
                            } else {
                                var t884 string = value__28.input
                                var t885 *ref_int_x = value__28.index
                                var t886 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t885)
                                var t887 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t884, t886)
                                var t888 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t887, 45)
                                jp878 = t888
                            }
                            jp866 = jp878
                        } else {
                            jp866 = false
                        }
                        if jp866 {
                            var t867 *ref_int_x = value__28.index
                            var t868 *ref_int_x = value__28.index
                            var t869 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t868)
                            var t870 int = t869 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t867, t870)
                        } else {}
                        var t861 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t862 bool = !t861
                        if t862 {
                            var t863 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t864 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t863,
                            }
                            retv841 = t864
                            return retv841
                        } else {
                            var t847 string = value__28.input
                            var t848 *ref_int_x = value__28.index
                            var t849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t848)
                            var t850 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t847, start__29, t849)
                            var t851 _goml_m_std_p_json_p_Value = Number{
                                _0: t850,
                            }
                            var t852 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t851,
                            }
                            retv841 = t852
                            return retv841
                        }
                    } else {
                        var t847 string = value__28.input
                        var t848 *ref_int_x = value__28.index
                        var t849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t848)
                        var t850 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t847, start__29, t849)
                        var t851 _goml_m_std_p_json_p_Value = Number{
                            _0: t850,
                        }
                        var t852 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t851,
                        }
                        retv841 = t852
                        return retv841
                    }
                }
            }
        } else {
            var t960 bool = _goml_m_std_p_json_p_parse__digits(value__28)
            var t961 bool = !t960
            if t961 {
                var t962 string = _goml_m_std_p_json_p_json__error(value__28, "expected number")
                var t963 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t962,
                }
                retv841 = t963
                return retv841
            } else {
                var t917 *ref_int_x = value__28.index
                var t918 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t917)
                var t919 string = value__28.input
                var t920 int = _goml_m_inherent_i_string_i_string_i_byte__len(t919)
                var t921 bool = t918 < t920
                var jp907 bool
                if t921 {
                    var t922 string = value__28.input
                    var t923 *ref_int_x = value__28.index
                    var t924 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t923)
                    var t925 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t922, t924)
                    var t926 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t925, 46)
                    jp907 = t926
                } else {
                    jp907 = false
                }
                if jp907 {
                    var t908 *ref_int_x = value__28.index
                    var t909 *ref_int_x = value__28.index
                    var t910 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t909)
                    var t911 int = t910 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t908, t911)
                    var t913 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t914 bool = !t913
                    if t914 {
                        var t915 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t916 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t915,
                        }
                        retv841 = t916
                        return retv841
                    } else {
                        var t889 *ref_int_x = value__28.index
                        var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                        var t891 string = value__28.input
                        var t892 int = _goml_m_inherent_i_string_i_string_i_byte__len(t891)
                        var t893 bool = t890 < t892
                        var jp854 bool
                        if t893 {
                            var t896 string = value__28.input
                            var t897 *ref_int_x = value__28.index
                            var t898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t897)
                            var t899 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t896, t898)
                            var t900 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t899, 101)
                            var jp895 bool
                            if t900 {
                                jp895 = true
                            } else {
                                var t901 string = value__28.input
                                var t902 *ref_int_x = value__28.index
                                var t903 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t902)
                                var t904 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t901, t903)
                                var t905 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t904, 69)
                                jp895 = t905
                            }
                            jp854 = jp895
                        } else {
                            jp854 = false
                        }
                        if jp854 {
                            var t855 *ref_int_x = value__28.index
                            var t856 *ref_int_x = value__28.index
                            var t857 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t856)
                            var t858 int = t857 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t855, t858)
                            var t872 *ref_int_x = value__28.index
                            var t873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t872)
                            var t874 string = value__28.input
                            var t875 int = _goml_m_inherent_i_string_i_string_i_byte__len(t874)
                            var t876 bool = t873 < t875
                            var jp866 bool
                            if t876 {
                                var t879 string = value__28.input
                                var t880 *ref_int_x = value__28.index
                                var t881 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t880)
                                var t882 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t879, t881)
                                var t883 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t882, 43)
                                var jp878 bool
                                if t883 {
                                    jp878 = true
                                } else {
                                    var t884 string = value__28.input
                                    var t885 *ref_int_x = value__28.index
                                    var t886 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t885)
                                    var t887 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t884, t886)
                                    var t888 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t887, 45)
                                    jp878 = t888
                                }
                                jp866 = jp878
                            } else {
                                jp866 = false
                            }
                            if jp866 {
                                var t867 *ref_int_x = value__28.index
                                var t868 *ref_int_x = value__28.index
                                var t869 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t868)
                                var t870 int = t869 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t867, t870)
                            } else {}
                            var t861 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t862 bool = !t861
                            if t862 {
                                var t863 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t864 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t863,
                                }
                                retv841 = t864
                                return retv841
                            } else {
                                var t847 string = value__28.input
                                var t848 *ref_int_x = value__28.index
                                var t849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t848)
                                var t850 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t847, start__29, t849)
                                var t851 _goml_m_std_p_json_p_Value = Number{
                                    _0: t850,
                                }
                                var t852 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t851,
                                }
                                retv841 = t852
                                return retv841
                            }
                        } else {
                            var t847 string = value__28.input
                            var t848 *ref_int_x = value__28.index
                            var t849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t848)
                            var t850 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t847, start__29, t849)
                            var t851 _goml_m_std_p_json_p_Value = Number{
                                _0: t850,
                            }
                            var t852 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t851,
                            }
                            retv841 = t852
                            return retv841
                        }
                    }
                } else {
                    var t889 *ref_int_x = value__28.index
                    var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                    var t891 string = value__28.input
                    var t892 int = _goml_m_inherent_i_string_i_string_i_byte__len(t891)
                    var t893 bool = t890 < t892
                    var jp854 bool
                    if t893 {
                        var t896 string = value__28.input
                        var t897 *ref_int_x = value__28.index
                        var t898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t897)
                        var t899 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t896, t898)
                        var t900 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t899, 101)
                        var jp895 bool
                        if t900 {
                            jp895 = true
                        } else {
                            var t901 string = value__28.input
                            var t902 *ref_int_x = value__28.index
                            var t903 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t902)
                            var t904 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t901, t903)
                            var t905 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t904, 69)
                            jp895 = t905
                        }
                        jp854 = jp895
                    } else {
                        jp854 = false
                    }
                    if jp854 {
                        var t855 *ref_int_x = value__28.index
                        var t856 *ref_int_x = value__28.index
                        var t857 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t856)
                        var t858 int = t857 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t855, t858)
                        var t872 *ref_int_x = value__28.index
                        var t873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t872)
                        var t874 string = value__28.input
                        var t875 int = _goml_m_inherent_i_string_i_string_i_byte__len(t874)
                        var t876 bool = t873 < t875
                        var jp866 bool
                        if t876 {
                            var t879 string = value__28.input
                            var t880 *ref_int_x = value__28.index
                            var t881 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t880)
                            var t882 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t879, t881)
                            var t883 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t882, 43)
                            var jp878 bool
                            if t883 {
                                jp878 = true
                            } else {
                                var t884 string = value__28.input
                                var t885 *ref_int_x = value__28.index
                                var t886 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t885)
                                var t887 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t884, t886)
                                var t888 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t887, 45)
                                jp878 = t888
                            }
                            jp866 = jp878
                        } else {
                            jp866 = false
                        }
                        if jp866 {
                            var t867 *ref_int_x = value__28.index
                            var t868 *ref_int_x = value__28.index
                            var t869 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t868)
                            var t870 int = t869 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t867, t870)
                        } else {}
                        var t861 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t862 bool = !t861
                        if t862 {
                            var t863 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t864 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t863,
                            }
                            retv841 = t864
                            return retv841
                        } else {
                            var t847 string = value__28.input
                            var t848 *ref_int_x = value__28.index
                            var t849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t848)
                            var t850 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t847, start__29, t849)
                            var t851 _goml_m_std_p_json_p_Value = Number{
                                _0: t850,
                            }
                            var t852 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t851,
                            }
                            retv841 = t852
                            return retv841
                        }
                    } else {
                        var t847 string = value__28.input
                        var t848 *ref_int_x = value__28.index
                        var t849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t848)
                        var t850 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t847, start__29, t849)
                        var t851 _goml_m_std_p_json_p_Value = Number{
                            _0: t850,
                        }
                        var t852 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t851,
                        }
                        retv841 = t852
                        return retv841
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__30 _goml_m_std_p_json_p_JsonParser, expected__31 string, result__32 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var retv975 _goml_m_Result____std_p_json_p_Value____string
    var t988 *ref_int_x = value__30.index
    var t989 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t988)
    var t990 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__31)
    var t991 int = t989 + t990
    var t992 string = value__30.input
    var t993 int = _goml_m_inherent_i_string_i_string_i_byte__len(t992)
    var t994 bool = t991 <= t993
    var jp979 bool
    if t994 {
        var t995 string = value__30.input
        var t996 *ref_int_x = value__30.index
        var t997 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t996)
        var t998 *ref_int_x = value__30.index
        var t999 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t998)
        var t1000 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__31)
        var t1001 int = t999 + t1000
        var t1002 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t995, t997, t1001)
        var t1003 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1002, expected__31)
        jp979 = t1003
    } else {
        jp979 = false
    }
    var jp977 _goml_m_Result____std_p_json_p_Value____string
    if jp979 {
        var t980 *ref_int_x = value__30.index
        var t981 *ref_int_x = value__30.index
        var t982 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t981)
        var t983 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__31)
        var t984 int = t982 + t983
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t980, t984)
        var t985 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__32,
        }
        jp977 = t985
    } else {
        var t986 string = _goml_m_std_p_json_p_json__error(value__30, "invalid literal")
        var t987 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t986,
        }
        jp977 = t987
    }
    retv975 = jp977
    return retv975
}

func _goml_m_std_p_json_p_parse__json__array(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv1005 _goml_m_Result____std_p_json_p_Value____string
    var t1006 *ref_int_x = value__33.index
    var t1007 *ref_int_x = value__33.index
    var t1008 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1007)
    var t1009 int = t1008 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1006, t1009)
    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
    var vec_literal__8961 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var result__34 *_goml_vec__goml_m_std_p_json_p_Value = vec_literal__8961
    var t1064 *ref_int_x = value__33.index
    var t1065 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1064)
    var t1066 string = value__33.input
    var t1067 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1066)
    var t1068 bool = t1065 < t1067
    var jp1057 bool
    if t1068 {
        var t1069 string = value__33.input
        var t1070 *ref_int_x = value__33.index
        var t1071 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1070)
        var t1072 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1069, t1071)
        var t1073 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1072, 93)
        jp1057 = t1073
    } else {
        jp1057 = false
    }
    if jp1057 {
        var t1058 *ref_int_x = value__33.index
        var t1059 *ref_int_x = value__33.index
        var t1060 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1059)
        var t1061 int = t1060 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1058, t1061)
        var t1062 _goml_m_std_p_json_p_Value = Array{
            _0: result__34,
        }
        var t1063 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1062,
        }
        retv1005 = t1063
        return retv1005
    } else {
        Loop_loop1014:
        for {
            var t1015 *ref_int_x = value__33.index
            var t1016 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1015)
            var t1017 string = value__33.input
            var t1018 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1017)
            var t1019 bool = t1016 < t1018
            if t1019 {
                var mtmp50 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__33)
                var jp1021 _goml_m_std_p_json_p_Value
                switch mtmp50.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x51 _goml_m_std_p_json_p_Value = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    var try_value__1060 _goml_m_std_p_json_p_Value = x51
                    jp1021 = try_value__1060
                    var item__35 _goml_m_std_p_json_p_Value = jp1021
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(result__34, item__35)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                    var t1023 *ref_int_x = value__33.index
                    var t1024 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1023)
                    var t1025 string = value__33.input
                    var t1026 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1025)
                    var t1027 bool = t1024 >= t1026
                    if t1027 {
                        var t1028 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
                        var t1029 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1028,
                        }
                        retv1005 = t1029
                        return retv1005
                    } else {
                        var t1031 string = value__33.input
                        var t1032 *ref_int_x = value__33.index
                        var t1033 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1032)
                        var t1034 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1031, t1033)
                        var t1035 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1034, 93)
                        if t1035 {
                            var t1036 *ref_int_x = value__33.index
                            var t1037 *ref_int_x = value__33.index
                            var t1038 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1037)
                            var t1039 int = t1038 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1036, t1039)
                            var t1040 _goml_m_std_p_json_p_Value = Array{
                                _0: result__34,
                            }
                            var t1041 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1040,
                            }
                            retv1005 = t1041
                            return retv1005
                        } else {
                            var t1043 string = value__33.input
                            var t1044 *ref_int_x = value__33.index
                            var t1045 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1044)
                            var t1046 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1043, t1045)
                            var t1047 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1046, 44)
                            if t1047 {
                                var t1048 *ref_int_x = value__33.index
                                var t1049 *ref_int_x = value__33.index
                                var t1050 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1049)
                                var t1051 int = t1050 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1048, t1051)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                                continue
                            } else {
                                var t1053 string = _goml_m_std_p_json_p_json__error(value__33, "expected array separator")
                                var t1054 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1053,
                                }
                                retv1005 = t1054
                                return retv1005
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x52 string = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var try_residual__1060 string = x52
                    var t1055 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: try_residual__1060,
                    }
                    retv1005 = t1055
                    return retv1005
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1014
            }
        }
        var t1012 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
        var t1013 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1012,
        }
        retv1005 = t1013
        return retv1005
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__36 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv1075 _goml_m_Result____std_p_json_p_Value____string
    var t1076 *ref_int_x = value__36.index
    var t1077 *ref_int_x = value__36.index
    var t1078 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1077)
    var t1079 int = t1078 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1076, t1079)
    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
    var vec_literal__10180 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var result__37 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_literal__10180
    var t1159 *ref_int_x = value__36.index
    var t1160 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1159)
    var t1161 string = value__36.input
    var t1162 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1161)
    var t1163 bool = t1160 < t1162
    var jp1152 bool
    if t1163 {
        var t1164 string = value__36.input
        var t1165 *ref_int_x = value__36.index
        var t1166 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1165)
        var t1167 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1164, t1166)
        var t1168 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1167, 125)
        jp1152 = t1168
    } else {
        jp1152 = false
    }
    if jp1152 {
        var t1153 *ref_int_x = value__36.index
        var t1154 *ref_int_x = value__36.index
        var t1155 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1154)
        var t1156 int = t1155 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1153, t1156)
        var t1157 _goml_m_std_p_json_p_Value = Object{
            _0: result__37,
        }
        var t1158 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1157,
        }
        retv1075 = t1158
        return retv1075
    } else {
        Loop_loop1084:
        for {
            var t1085 *ref_int_x = value__36.index
            var t1086 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1085)
            var t1087 string = value__36.input
            var t1088 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1087)
            var t1089 bool = t1086 < t1088
            if t1089 {
                var mtmp62 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__36)
                var jp1091 string
                switch mtmp62.(type) {
                case Result__string__string_Ok:
                    var x63 string = mtmp62.(Result__string__string_Ok)._0
                    var try_value__1214 string = x63
                    jp1091 = try_value__1214
                    var name__38 string = jp1091
                    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                    var t1139 *ref_int_x = value__36.index
                    var t1140 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1139)
                    var t1141 string = value__36.input
                    var t1142 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1141)
                    var t1143 bool = t1140 >= t1142
                    var jp1131 bool
                    if t1143 {
                        jp1131 = true
                    } else {
                        var t1144 string = value__36.input
                        var t1145 *ref_int_x = value__36.index
                        var t1146 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1145)
                        var t1147 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1144, t1146)
                        var t1148 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1147, 58)
                        var t1149 bool = !t1148
                        jp1131 = t1149
                    }
                    if jp1131 {
                        var t1132 string = _goml_m_std_p_json_p_json__error(value__36, "expected object colon")
                        var t1133 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1132,
                        }
                        retv1075 = t1133
                        return retv1075
                    } else {
                        var t1134 *ref_int_x = value__36.index
                        var t1135 *ref_int_x = value__36.index
                        var t1136 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1135)
                        var t1137 int = t1136 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1134, t1137)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                        var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__36)
                        var jp1094 _goml_m_std_p_json_p_Value
                        switch mtmp68.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            var try_value__1263 _goml_m_std_p_json_p_Value = x69
                            jp1094 = try_value__1263
                            var item__39 _goml_m_std_p_json_p_Value = jp1094
                            var t1095 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: name__38,
                                _1: item__39,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__37, t1095)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                            var t1097 *ref_int_x = value__36.index
                            var t1098 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1097)
                            var t1099 string = value__36.input
                            var t1100 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1099)
                            var t1101 bool = t1098 >= t1100
                            if t1101 {
                                var t1102 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
                                var t1103 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1102,
                                }
                                retv1075 = t1103
                                return retv1075
                            } else {
                                var t1105 string = value__36.input
                                var t1106 *ref_int_x = value__36.index
                                var t1107 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1106)
                                var t1108 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1105, t1107)
                                var t1109 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1108, 125)
                                if t1109 {
                                    var t1110 *ref_int_x = value__36.index
                                    var t1111 *ref_int_x = value__36.index
                                    var t1112 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1111)
                                    var t1113 int = t1112 + 1
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1110, t1113)
                                    var t1114 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__37,
                                    }
                                    var t1115 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1114,
                                    }
                                    retv1075 = t1115
                                    return retv1075
                                } else {
                                    var t1117 string = value__36.input
                                    var t1118 *ref_int_x = value__36.index
                                    var t1119 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1118)
                                    var t1120 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1117, t1119)
                                    var t1121 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1120, 44)
                                    if t1121 {
                                        var t1122 *ref_int_x = value__36.index
                                        var t1123 *ref_int_x = value__36.index
                                        var t1124 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1123)
                                        var t1125 int = t1124 + 1
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1122, t1125)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                                        continue
                                    } else {
                                        var t1127 string = _goml_m_std_p_json_p_json__error(value__36, "expected object separator")
                                        var t1128 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1127,
                                        }
                                        retv1075 = t1128
                                        return retv1075
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var try_residual__1263 string = x70
                            var t1129 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: try_residual__1263,
                            }
                            retv1075 = t1129
                            return retv1075
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x64 string = mtmp62.(Result__string__string_Err)._0
                    var try_residual__1214 string = x64
                    var t1150 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: try_residual__1214,
                    }
                    retv1075 = t1150
                    return retv1075
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1084
            }
        }
        var t1082 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
        var t1083 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1082,
        }
        retv1075 = t1083
        return retv1075
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__40 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv1170 _goml_m_Result____std_p_json_p_Value____string
    _goml_m_std_p_json_p_skip__json__whitespace(value__40)
    var t1173 *ref_int_x = value__40.index
    var t1174 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1173)
    var t1175 string = value__40.input
    var t1176 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1175)
    var t1177 bool = t1174 >= t1176
    var jp1172 _goml_m_Result____std_p_json_p_Value____string
    if t1177 {
        var t1178 string = _goml_m_std_p_json_p_json__error(value__40, "expected JSON value")
        var t1179 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1178,
        }
        jp1172 = t1179
    } else {
        var t1180 string = value__40.input
        var t1181 *ref_int_x = value__40.index
        var t1182 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1181)
        var mtmp77 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1180, t1182)
        var jp1184 _goml_m_Result____std_p_json_p_Value____string
        switch mtmp77 {
        case 123:
            var t1185 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__40)
            jp1184 = t1185
        case 91:
            var t1186 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__40)
            jp1184 = t1186
        case 34:
            var mtmp78 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__40)
            var jp1188 _goml_m_Result____std_p_json_p_Value____string
            switch mtmp78.(type) {
            case Result__string__string_Ok:
                var x79 string = mtmp78.(Result__string__string_Ok)._0
                var text__41 string = x79
                var t1189 _goml_m_std_p_json_p_Value = String{
                    _0: text__41,
                }
                var t1190 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1189,
                }
                jp1188 = t1190
            case Result__string__string_Err:
                var x80 string = mtmp78.(Result__string__string_Err)._0
                var error__42 string = x80
                var t1191 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: error__42,
                }
                jp1188 = t1191
            default:
                panic("non-exhaustive match")
            }
            jp1184 = jp1188
        case 116:
            var t1192 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t1193 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "true", t1192)
            jp1184 = t1193
        case 102:
            var t1194 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t1195 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "false", t1194)
            jp1184 = t1195
        case 110:
            var t1196 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "null", Null{})
            jp1184 = t1196
        default:
            var byte__43 uint8 = mtmp77
            var t1204 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__43, 45)
            var jp1200 bool
            if t1204 {
                jp1200 = true
            } else {
                var t1205 bool = _goml_m_std_p_json_p_json__digit(byte__43)
                jp1200 = t1205
            }
            var jp1198 _goml_m_Result____std_p_json_p_Value____string
            if jp1200 {
                var t1201 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__40)
                jp1198 = t1201
            } else {
                var t1202 string = _goml_m_std_p_json_p_json__error(value__40, "unexpected JSON token")
                var t1203 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1202,
                }
                jp1198 = t1203
            }
            jp1184 = jp1198
        }
        jp1172 = jp1184
    }
    retv1170 = jp1172
    return retv1170
}

func _goml_m_std_p_json_p_parse(input__44 string) _goml_m_Result____std_p_json_p_Value____string {
    var retv1207 _goml_m_Result____std_p_json_p_Value____string
    var parser__45 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_json__parser(input__44)
    var mtmp81 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__45)
    var jp1209 _goml_m_std_p_json_p_Value
    switch mtmp81.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x82 _goml_m_std_p_json_p_Value = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var try_value__1440 _goml_m_std_p_json_p_Value = x82
        jp1209 = try_value__1440
        var result__46 _goml_m_std_p_json_p_Value = jp1209
        _goml_m_std_p_json_p_skip__json__whitespace(parser__45)
        var t1212 *ref_int_x = parser__45.index
        var t1213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1212)
        var t1214 int = _goml_m_inherent_i_string_i_string_i_byte__len(input__44)
        var t1215 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1213, t1214)
        var jp1211 _goml_m_Result____std_p_json_p_Value____string
        if t1215 {
            var t1216 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: result__46,
            }
            jp1211 = t1216
        } else {
            var t1217 string = _goml_m_std_p_json_p_json__error(parser__45, "trailing JSON data")
            var t1218 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1217,
            }
            jp1211 = t1218
        }
        retv1207 = jp1211
        return retv1207
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x83 string = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var try_residual__1440 string = x83
        var t1219 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: try_residual__1440,
        }
        retv1207 = t1219
        return retv1207
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_json__hex__digit(value__47 uint8) rune {
    var retv1221 rune
    var t1222 int = int(uint8(value__47))
    var t1223 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", t1222)
    retv1221 = t1223
    return retv1221
}

func _goml_m_std_p_json_p_write__json__string(builder__48 _goml_m_std_p_text_p_StringBuilder, value__49 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    var start__50 int = 0
    var for_index86 int = 0
    var for_limit87 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    Loop_loop1233:
    for {
        var t1234 bool = for_index86 < for_limit87
        if t1234 {
            var for_item88 int = for_index86
            var t1235 int = for_index86 + 1
            for_index86 = t1235
            var index__51 int = for_item88
            var byte__52 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__49, index__51)
            var t1288 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 34)
            var jp1286 bool
            if t1288 {
                jp1286 = true
            } else {
                var t1289 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 92)
                jp1286 = t1289
            }
            var jp1283 bool
            if jp1286 {
                jp1283 = true
            } else {
                var t1287 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 8)
                jp1283 = t1287
            }
            var jp1280 bool
            if jp1283 {
                jp1280 = true
            } else {
                var t1284 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 9)
                jp1280 = t1284
            }
            var jp1277 bool
            if jp1280 {
                jp1277 = true
            } else {
                var t1281 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 10)
                jp1277 = t1281
            }
            var jp1274 bool
            if jp1277 {
                jp1274 = true
            } else {
                var t1278 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 12)
                jp1274 = t1278
            }
            var jp1271 bool
            if jp1274 {
                jp1271 = true
            } else {
                var t1275 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 13)
                jp1271 = t1275
            }
            var jp1238 bool
            if jp1271 {
                jp1238 = true
            } else {
                var t1272 bool = byte__52 < 32
                jp1238 = t1272
            }
            if jp1238 {
                var t1267 bool = start__50 < index__51
                if t1267 {
                    var t1268 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__49, start__50, index__51)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1268)
                } else {}
                var t1242 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 34)
                if t1242 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\"")
                } else {
                    var t1245 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 92)
                    if t1245 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\\")
                    } else {
                        var t1248 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 8)
                        if t1248 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\b")
                        } else {
                            var t1251 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 9)
                            if t1251 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\t")
                            } else {
                                var t1254 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 10)
                                if t1254 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\n")
                                } else {
                                    var t1257 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 12)
                                    if t1257 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\f")
                                    } else {
                                        var t1260 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 13)
                                        if t1260 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\u00")
                                            var t1262 uint8 = byte__52 / 16
                                            var t1263 rune = _goml_m_std_p_json_p_json__hex__digit(t1262)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, t1263)
                                            var t1264_rhs uint8 = 16
                                            var t1264 uint8 = byte__52 % t1264_rhs
                                            var t1265 rune = _goml_m_std_p_json_p_json__hex__digit(t1264)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, t1265)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1241 int = index__51 + 1
                start__50 = t1241
            } else {}
            continue
        } else {
            break Loop_loop1233
        }
    }
    var t1228 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    var t1229 bool = start__50 < t1228
    if t1229 {
        var t1230 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
        var t1231 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__49, start__50, t1230)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1231)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__53 _goml_m_std_p_text_p_StringBuilder, value__54 _goml_m_std_p_json_p_Value) struct{} {
    switch value__54.(type) {
    case Object:
        var x97 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__54.(Object)._0
        var fields__55 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x97
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 123)
        var index__56 int = 0
        var for_source103 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = fields__55
        var for_limit104 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source103)
        var for_index105 int = 0
        Loop_loop1294:
        for {
            var t1295 bool = for_index105 < for_limit104
            if t1295 {
                var for_item106 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source103, for_index105)
                var t1296 int = for_index105 + 1
                for_index105 = t1296
                var field__57 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item106
                var t1302 bool = index__56 > 0
                if t1302 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 44)
                } else {}
                var t1298 string = field__57._0
                _goml_m_std_p_json_p_write__json__string(builder__53, t1298)
                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 58)
                var t1299 _goml_m_std_p_json_p_Value = field__57._1
                _goml_m_std_p_json_p_write__json__value(builder__53, t1299)
                var compound_old112 int = index__56
                var compound_value113 int = 1
                var t1300 int = compound_old112 + compound_value113
                index__56 = t1300
                continue
            } else {
                break Loop_loop1294
            }
        }
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 125)
        return struct{}{}
    case Array:
        var x98 *_goml_vec__goml_m_std_p_json_p_Value = value__54.(Array)._0
        var items__58 *_goml_vec__goml_m_std_p_json_p_Value = x98
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 91)
        var index__59 int = 0
        var for_source117 *_goml_vec__goml_m_std_p_json_p_Value = items__58
        var for_limit118 int = vec_len___goml_m_Vec__16std_p_json_p_Value(for_source117)
        var for_index119 int = 0
        Loop_loop1306:
        for {
            var t1307 bool = for_index119 < for_limit118
            if t1307 {
                var for_item120 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(for_source117, for_index119)
                var t1308 int = for_index119 + 1
                for_index119 = t1308
                var item__60 _goml_m_std_p_json_p_Value = for_item120
                var t1312 bool = index__59 > 0
                if t1312 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 44)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__53, item__60)
                var compound_old124 int = index__59
                var compound_value125 int = 1
                var t1310 int = compound_old124 + compound_value125
                index__59 = t1310
                continue
            } else {
                break Loop_loop1306
            }
        }
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 93)
        return struct{}{}
    case String:
        var x99 string = value__54.(String)._0
        var text__61 string = x99
        _goml_m_std_p_json_p_write__json__string(builder__53, text__61)
        return struct{}{}
    case Number:
        var x100 string = value__54.(Number)._0
        var number__62 string = x100
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, number__62)
        return struct{}{}
    case Bool:
        var x101 bool = value__54.(Bool)._0
        var value__63 bool = x101
        var jp1317 string
        if value__63 {
            jp1317 = "true"
        } else {
            jp1317 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, jp1317)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__64 _goml_m_std_p_json_p_Value) string {
    var retv1321 string
    var builder__65 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
    _goml_m_std_p_json_p_write__json__value(builder__65, value__64)
    var t1322 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__65)
    retv1321 = t1322
    return retv1321
}

func _goml_m_std_p_json_p_field(value__66 _goml_m_std_p_json_p_Value, name__67 string) _goml_m_Option____std_p_json_p_Value {
    var retv1324 _goml_m_Option____std_p_json_p_Value
    var jp1326 _goml_m_Option____std_p_json_p_Value
    switch value__66.(type) {
    case Object:
        var x129 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__66.(Object)._0
        var fields__68 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x129
        var for_source134 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = fields__68
        var for_limit135 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source134)
        var for_index136 int = 0
        Loop_loop1328:
        for {
            var t1329 bool = for_index136 < for_limit135
            if t1329 {
                var for_item137 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source134, for_index136)
                var t1330 int = for_index136 + 1
                for_index136 = t1330
                var field__69 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item137
                var t1332 string = field__69._0
                var t1333 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1332, name__67)
                if t1333 {
                    var t1334 _goml_m_std_p_json_p_Value = field__69._1
                    var t1335 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1334,
                    }
                    retv1324 = t1335
                    return retv1324
                } else {
                    continue
                }
            } else {
                break Loop_loop1328
            }
        }
        jp1326 = _goml_m_Option____std_p_json_p_Value_None{}
        retv1324 = jp1326
        return retv1324
    default:
        jp1326 = _goml_m_Option____std_p_json_p_Value_None{}
        retv1324 = jp1326
        return retv1324
    }
}

func _goml_m_std_p_json_p_as__string(value__70 _goml_m_std_p_json_p_Value) Option__string {
    var retv1337 Option__string
    var jp1339 Option__string
    switch value__70.(type) {
    case String:
        var x142 string = value__70.(String)._0
        var text__71 string = x142
        var t1340 Option__string = Option__string_Some{
            _0: text__71,
        }
        jp1339 = t1340
    default:
        jp1339 = Option__string_None{}
    }
    retv1337 = jp1339
    return retv1337
}

func _goml_m_std_p_json_p_parse__json__int__text(value__72 string) Option__int {
    var retv1342 Option__int
    var t1345 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__72)
    var t1346 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1345, 0)
    var jp1344 Option__int
    if t1346 {
        jp1344 = Option__int_None{}
        retv1342 = jp1344
        return retv1342
    } else {
        var t1347 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__72, 0)
        var negative__73 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1347, 45)
        var jp1349 int
        if negative__73 {
            jp1349 = 1
        } else {
            jp1349 = 0
        }
        var index__74 int = jp1349
        var result__75 int = 0
        var t1370 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__72)
        var t1371 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(index__74, t1370)
        if t1371 {
            retv1342 = Option__int_None{}
            return retv1342
        } else {
            Loop_loop1356:
            for {
                var t1357 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__72)
                var t1358 bool = index__74 < t1357
                if t1358 {
                    var byte__76 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__72, index__74)
                    var t1368 bool = byte__76 < 48
                    var jp1363 bool
                    if t1368 {
                        jp1363 = true
                    } else {
                        var t1369 bool = byte__76 > 57
                        jp1363 = t1369
                    }
                    if jp1363 {
                        retv1342 = Option__int_None{}
                        return retv1342
                    } else {
                        var t1364 int = result__75 * 10
                        var t1365 uint8 = byte__76 - 48
                        var t1366 int = int(uint8(t1365))
                        var t1367 int = t1364 + t1366
                        result__75 = t1367
                        var compound_old148 int = index__74
                        var compound_value149 int = 1
                        var t1360 int = compound_old148 + compound_value149
                        index__74 = t1360
                        continue
                    }
                } else {
                    break Loop_loop1356
                }
            }
            var jp1353 int
            if negative__73 {
                var t1355 int = 0 - result__75
                jp1353 = t1355
            } else {
                jp1353 = result__75
            }
            var t1354 Option__int = Option__int_Some{
                _0: jp1353,
            }
            jp1344 = t1354
            retv1342 = jp1344
            return retv1342
        }
    }
}

func _goml_m_std_p_json_p_as__int(value__77 _goml_m_std_p_json_p_Value) Option__int {
    var retv1373 Option__int
    var jp1375 Option__int
    switch value__77.(type) {
    case Number:
        var x155 string = value__77.(Number)._0
        var number__78 string = x155
        var t1376 Option__int = _goml_m_std_p_json_p_parse__json__int__text(number__78)
        jp1375 = t1376
    default:
        jp1375 = Option__int_None{}
    }
    retv1373 = jp1375
    return retv1373
}

func _goml_m_std_p_json_p_as__bool(value__79 _goml_m_std_p_json_p_Value) Option__bool {
    var retv1378 Option__bool
    var jp1380 Option__bool
    switch value__79.(type) {
    case Bool:
        var x161 bool = value__79.(Bool)._0
        var result__80 bool = x161
        var t1381 Option__bool = Option__bool_Some{
            _0: result__80,
        }
        jp1380 = t1381
    default:
        jp1380 = Option__bool_None{}
    }
    retv1378 = jp1380
    return retv1378
}

func main0() struct{} {
    var mtmp152 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1392 _goml_m_std_p_json_p_Value
    switch mtmp152.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x153 _goml_m_std_p_json_p_Value = mtmp152.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__0 _goml_m_std_p_json_p_Value = x153
        jp1392 = value__0
        var value__2 _goml_m_std_p_json_p_Value = jp1392
        var mtmp156 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "name")
        switch mtmp156.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing name")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x157 _goml_m_std_p_json_p_Value = mtmp156.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__3 _goml_m_std_p_json_p_Value = x157
            var mtmp158 Option__string = _goml_m_std_p_json_p_as__string(field__3)
            switch mtmp158.(type) {
            case Option__string_None:
                println__T_string("invalid name")
            case Option__string_Some:
                var x159 string = mtmp158.(Option__string_Some)._0
                var name__4 string = x159
                println__T_string(name__4)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var mtmp161 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "version")
        switch mtmp161.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing version")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x162 _goml_m_std_p_json_p_Value = mtmp161.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__5 _goml_m_std_p_json_p_Value = x162
            var mtmp163 Option__int = _goml_m_std_p_json_p_as__int(field__5)
            switch mtmp163.(type) {
            case Option__int_None:
                println__T_string("invalid version")
            case Option__int_Some:
                var x164 int = mtmp163.(Option__int_Some)._0
                var version__6 int = x164
                println__T_int(version__6)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var mtmp166 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "stable")
        switch mtmp166.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing stable")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x167 _goml_m_std_p_json_p_Value = mtmp166.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__7 _goml_m_std_p_json_p_Value = x167
            var mtmp168 Option__bool = _goml_m_std_p_json_p_as__bool(field__7)
            switch mtmp168.(type) {
            case Option__bool_None:
                println__T_string("invalid stable")
            case Option__bool_Some:
                var x169 bool = mtmp168.(Option__bool_Some)._0
                var stable__8 bool = x169
                println__T_bool(stable__8)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var t1396 string = _goml_m_std_p_json_p_encode(value__2)
        println__T_string(t1396)
        return struct{}{}
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x154 string = mtmp152.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var error__1 string = x154
        println__T_string(error__1)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv1411 *_goml_vec_uint8
    var t1412 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv1411 = t1412
    return retv1411
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__126 *_goml_vec_uint8, elem__127 uint8) struct{} {
    vec_push__Vec_5uint8(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__uint8(self__140 *_goml_vec_uint8, additional__141 int) struct{} {
    vec_reserve__Vec_5uint8(self__140, additional__141)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv1449 int
    var t1450 int = _goml_runtime_core_string_len(self__9)
    retv1449 = t1450
    return retv1449
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv1452 uint8
    var t1453 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv1452 = t1453
    return retv1452
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv1455 string
    var t1456 string = _goml_runtime_core_char_to_string(self__7)
    retv1455 = t1456
    return retv1455
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv1458 bool
    var t1459 bool = self__69 == other__70
    retv1458 = t1459
    return retv1458
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv1461 bool
    var t1462 bool = self__59 == other__60
    retv1461 = t1462
    return retv1461
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv1467 string
    var t1468 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv1467 = t1468
    return retv1467
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv1470 bool
    var t1471 bool = self__55 == other__56
    retv1470 = t1471
    return retv1470
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv1493 *ref_int_x
    var t1494 *ref_int_x = ref__Ref_3int(value__207)
    retv1493 = t1494
    return retv1493
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv1496 int
    var t1497 int = ref_get__Ref_3int(self__208)
    retv1496 = t1497
    return retv1496
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv1499 string
    var t1500 string = _goml_runtime_core_int_to_string(self__5)
    retv1499 = t1500
    return retv1499
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv1504 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp1506 Option__char
    if valid__3 {
        var t1507 Option__char = Option__char_Some{
            _0: value__4,
        }
        jp1506 = t1507
    } else {
        jp1506 = Option__char_None{}
    }
    retv1504 = jp1506
    return retv1504
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var retv1509 *_goml_vec__goml_m_std_p_json_p_Value
    var t1510 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    retv1509 = t1510
    return retv1509
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__126 *_goml_vec__goml_m_std_p_json_p_Value, elem__127 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var retv1514 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
    var t1515 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    retv1514 = t1515
    return retv1514
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__126 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__127 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv1519 rune
    var t1520 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv1519 = t1520
    return retv1519
}

func println__T_string(value__1 string) struct{} {
    var t1522 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t1522)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t1525 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t1525)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t1528 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t1528)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv1533 string
    retv1533 = self__38
    return retv1533
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv1535 string
    var t1536 string = _goml_runtime_core_int_to_string(self__40)
    retv1535 = t1536
    return retv1535
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv1538 string
    var t1539 string = _goml_runtime_core_bool_to_string(self__37)
    retv1538 = t1539
    return retv1538
}

func main() {
    main0()
}
