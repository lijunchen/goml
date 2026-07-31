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
    var retv235 _goml_m_std_p_text_p_StringBuilder
    var t236 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var t237 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: t236,
    }
    retv235 = t237
    return retv235
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var t252 *_goml_vec_uint8 = self__3.values
    var t253 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__uint8(t252, t253)
    var for_index1 int = 0
    var for_limit2 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    Loop_loop255:
    for {
        var t256 bool = for_index1 < for_limit2
        if t256 {
            var for_item3 int = for_index1
            var t257 int = for_index1 + 1
            for_index1 = t257
            var index__5 int = for_item3
            var t258 *_goml_vec_uint8 = self__3.values
            var t259 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__4, index__5)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(t258, t259)
            continue
        } else {
            break Loop_loop255
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__6 _goml_m_std_p_text_p_StringBuilder, value__7 rune) struct{} {
    var t262 string = _goml_m_inherent_i_char_i_char_i_to__string(value__7)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__6, t262)
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(self__15 _goml_m_std_p_text_p_StringBuilder) string {
    var retv276 string
    var t277 *_goml_vec_uint8 = self__15.values
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t277)
    var x12 string = mtmp10._1
    var value__16 string = x12
    retv276 = value__16
    return retv276
}

func _goml_m_std_p_json_p_json__parser(input__0 string) _goml_m_std_p_json_p_JsonParser {
    var retv507 _goml_m_std_p_json_p_JsonParser
    var t508 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t509 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__0,
        index: t508,
    }
    retv507 = t509
    return retv507
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var retv511 string
    var t512 string = "" + message__2
    var t513 string = t512 + " at byte "
    var t514 *ref_int_x = value__1.index
    var t515 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t514)
    var t516 string = _goml_m_inherent_i_int_i_int_i_to__string(t515)
    var t517 string = t513 + t516
    retv511 = t517
    return retv511
}

func _goml_m_std_p_json_p_json__whitespace(value__3 uint8) bool {
    var retv519 bool
    var t528 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 9)
    var jp526 bool
    if t528 {
        jp526 = true
    } else {
        var t529 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 10)
        jp526 = t529
    }
    var jp523 bool
    if jp526 {
        jp523 = true
    } else {
        var t527 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 13)
        jp523 = t527
    }
    var jp521 bool
    if jp523 {
        jp521 = true
    } else {
        var t524 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 32)
        jp521 = t524
    }
    retv519 = jp521
    return retv519
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop532:
    for {
        var t540 *ref_int_x = value__4.index
        var t541 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t540)
        var t542 string = value__4.input
        var t543 int = _goml_m_inherent_i_string_i_string_i_byte__len(t542)
        var t544 bool = t541 < t543
        var jp534 bool
        if t544 {
            var t545 string = value__4.input
            var t546 *ref_int_x = value__4.index
            var t547 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t546)
            var t548 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t545, t547)
            var t549 bool = _goml_m_std_p_json_p_json__whitespace(t548)
            jp534 = t549
        } else {
            jp534 = false
        }
        if jp534 {
            var t535 *ref_int_x = value__4.index
            var t536 *ref_int_x = value__4.index
            var t537 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t536)
            var t538 int = t537 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t535, t538)
            continue
        } else {
            break Loop_loop532
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var retv551 Option__uint32
    var t579 bool = value__5 >= 48
    var jp555 bool
    if t579 {
        var t580 bool = value__5 <= 57
        jp555 = t580
    } else {
        jp555 = false
    }
    var jp553 Option__uint32
    if jp555 {
        var t556 uint8 = value__5 - 48
        var t557 uint32 = uint32(uint8(t556))
        var t558 Option__uint32 = Option__uint32_Some{
            _0: t557,
        }
        jp553 = t558
    } else {
        var t577 bool = value__5 >= 65
        var jp562 bool
        if t577 {
            var t578 bool = value__5 <= 70
            jp562 = t578
        } else {
            jp562 = false
        }
        var jp560 Option__uint32
        if jp562 {
            var t563 uint8 = value__5 - 65
            var t564 uint8 = t563 + 10
            var t565 uint32 = uint32(uint8(t564))
            var t566 Option__uint32 = Option__uint32_Some{
                _0: t565,
            }
            jp560 = t566
        } else {
            var t575 bool = value__5 >= 97
            var jp570 bool
            if t575 {
                var t576 bool = value__5 <= 102
                jp570 = t576
            } else {
                jp570 = false
            }
            var jp568 Option__uint32
            if jp570 {
                var t571 uint8 = value__5 - 97
                var t572 uint8 = t571 + 10
                var t573 uint32 = uint32(uint8(t572))
                var t574 Option__uint32 = Option__uint32_Some{
                    _0: t573,
                }
                jp568 = t574
            } else {
                jp568 = Option__uint32_None{}
            }
            jp560 = jp568
        }
        jp553 = jp560
    }
    retv551 = jp553
    return retv551
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var retv582 Result__uint32__string
    var t585 *ref_int_x = value__6.index
    var t586 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t585)
    var t587 int = t586 + 4
    var t588 string = value__6.input
    var t589 int = _goml_m_inherent_i_string_i_string_i_byte__len(t588)
    var t590 bool = t587 > t589
    var jp584 Result__uint32__string
    if t590 {
        var t591 string = _goml_m_std_p_json_p_json__error(value__6, "incomplete unicode escape")
        var t592 Result__uint32__string = Result__uint32__string_Err{
            _0: t591,
        }
        jp584 = t592
        retv582 = jp584
        return retv582
    } else {
        var result__7_source int = 0
        var result__7 uint32 = uint32(int(result__7_source))
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop599:
        for {
            var t600 bool = for_index0 < for_limit1
            if t600 {
                var for_item2 int = for_index0
                var t601 int = for_index0 + 1
                for_index0 = t601
                var offset__8 int = for_item2
                var t602 string = value__6.input
                var t603 *ref_int_x = value__6.index
                var t604 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t603)
                var t605 int = t604 + offset__8
                var t606 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t602, t605)
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t606)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t608 string = _goml_m_std_p_json_p_json__error(value__6, "invalid unicode escape")
                    var t609 Result__uint32__string = Result__uint32__string_Err{
                        _0: t608,
                    }
                    retv582 = t609
                    return retv582
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var digit__9 uint32 = x5
                    var t610 uint32 = result__7 * 16
                    var t611 uint32 = t610 + digit__9
                    result__7 = t611
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop599
            }
        }
        var t594 *ref_int_x = value__6.index
        var t595 *ref_int_x = value__6.index
        var t596 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t595)
        var t597 int = t596 + 4
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t594, t597)
        var t598 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__7,
        }
        jp584 = t598
        retv582 = jp584
        return retv582
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var retv613 Result__unit__string
    var mtmp9 Option__char = char_from_uint32(codepoint__12)
    var jp615 Result__unit__string
    switch mtmp9.(type) {
    case Option__char_None:
        var t616 string = _goml_m_std_p_json_p_json__error(value__10, "invalid unicode codepoint")
        var t617 Result__unit__string = Result__unit__string_Err{
            _0: t616,
        }
        jp615 = t617
    case Option__char_Some:
        var x10 rune = mtmp9.(Option__char_Some)._0
        var character__13 rune = x10
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__11, character__13)
        var t618 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp615 = t618
    default:
        panic("non-exhaustive match")
    }
    retv613 = jp615
    return retv613
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var retv620 Result__unit__string
    var mtmp12 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp622 uint32
    switch mtmp12.(type) {
    case Result__uint32__string_Ok:
        var x13 uint32 = mtmp12.(Result__uint32__string_Ok)._0
        var try_value__191 uint32 = x13
        jp622 = try_value__191
        var first__16 uint32 = jp622
        var t684 bool = first__16 >= 55296
        var jp626 bool
        if t684 {
            var t685 bool = first__16 <= 56319
            jp626 = t685
        } else {
            jp626 = false
        }
        var jp624 Result__unit__string
        if jp626 {
            var t663 *ref_int_x = value__14.index
            var t664 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t663)
            var t665 int = t664 + 2
            var t666 string = value__14.input
            var t667 int = _goml_m_inherent_i_string_i_string_i_byte__len(t666)
            var t668 bool = t665 > t667
            var jp655 bool
            if t668 {
                jp655 = true
            } else {
                var t669 string = value__14.input
                var t670 *ref_int_x = value__14.index
                var t671 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t670)
                var t672 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t669, t671)
                var t673 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t672, 92)
                var t674 bool = !t673
                jp655 = t674
            }
            var jp630 bool
            if jp655 {
                jp630 = true
            } else {
                var t656 string = value__14.input
                var t657 *ref_int_x = value__14.index
                var t658 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t657)
                var t659 int = t658 + 1
                var t660 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t656, t659)
                var t661 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t660, 117)
                var t662 bool = !t661
                jp630 = t662
            }
            var jp628 Result__unit__string
            if jp630 {
                var t631 string = _goml_m_std_p_json_p_json__error(value__14, "missing low surrogate")
                var t632 Result__unit__string = Result__unit__string_Err{
                    _0: t631,
                }
                jp628 = t632
                jp624 = jp628
                retv620 = jp624
                return retv620
            } else {
                var t633 *ref_int_x = value__14.index
                var t634 *ref_int_x = value__14.index
                var t635 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t634)
                var t636 int = t635 + 2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t633, t636)
                var mtmp16 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp638 uint32
                switch mtmp16.(type) {
                case Result__uint32__string_Ok:
                    var x17 uint32 = mtmp16.(Result__uint32__string_Ok)._0
                    var try_value__253 uint32 = x17
                    jp638 = try_value__253
                    var second__17 uint32 = jp638
                    var t651 bool = second__17 < 56320
                    var jp642 bool
                    if t651 {
                        jp642 = true
                    } else {
                        var t652 bool = second__17 > 57343
                        jp642 = t652
                    }
                    var jp640 Result__unit__string
                    if jp642 {
                        var t643 string = _goml_m_std_p_json_p_json__error(value__14, "invalid low surrogate")
                        var t644 Result__unit__string = Result__unit__string_Err{
                            _0: t643,
                        }
                        jp640 = t644
                    } else {
                        var t645 uint32 = first__16 - 55296
                        var t646 uint32 = t645 * 1024
                        var t647 uint32 = 65536 + t646
                        var t648 uint32 = t647 + second__17
                        var t649 uint32 = t648 - 56320
                        var t650 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, t649)
                        jp640 = t650
                    }
                    jp628 = jp640
                    jp624 = jp628
                    retv620 = jp624
                    return retv620
                case Result__uint32__string_Err:
                    var x18 string = mtmp16.(Result__uint32__string_Err)._0
                    var try_residual__253 string = x18
                    var t653 Result__unit__string = Result__unit__string_Err{
                        _0: try_residual__253,
                    }
                    retv620 = t653
                    return retv620
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t682 bool = first__16 >= 56320
            var jp678 bool
            if t682 {
                var t683 bool = first__16 <= 57343
                jp678 = t683
            } else {
                jp678 = false
            }
            var jp676 Result__unit__string
            if jp678 {
                var t679 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t680 Result__unit__string = Result__unit__string_Err{
                    _0: t679,
                }
                jp676 = t680
            } else {
                var t681 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, first__16)
                jp676 = t681
            }
            jp624 = jp676
            retv620 = jp624
            return retv620
        }
    case Result__uint32__string_Err:
        var x14 string = mtmp12.(Result__uint32__string_Err)._0
        var try_residual__191 string = x14
        var t686 Result__unit__string = Result__unit__string_Err{
            _0: try_residual__191,
        }
        retv620 = t686
        return retv620
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__18 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var retv688 Result__string__string
    var t802 *ref_int_x = value__18.index
    var t803 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t802)
    var t804 string = value__18.input
    var t805 int = _goml_m_inherent_i_string_i_string_i_byte__len(t804)
    var t806 bool = t803 >= t805
    var jp794 bool
    if t806 {
        jp794 = true
    } else {
        var t807 string = value__18.input
        var t808 *ref_int_x = value__18.index
        var t809 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t808)
        var t810 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t807, t809)
        var t811 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t810, 34)
        var t812 bool = !t811
        jp794 = t812
    }
    if jp794 {
        var t795 string = _goml_m_std_p_json_p_json__error(value__18, "expected string")
        var t796 Result__string__string = Result__string__string_Err{
            _0: t795,
        }
        retv688 = t796
        return retv688
    } else {
        var t797 *ref_int_x = value__18.index
        var t798 *ref_int_x = value__18.index
        var t799 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t798)
        var t800 int = t799 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t797, t800)
        var builder__19 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t690 *ref_int_x = value__18.index
        var segment__20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t690)
        Loop_loop694:
        for {
            var t695 *ref_int_x = value__18.index
            var t696 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t695)
            var t697 string = value__18.input
            var t698 int = _goml_m_inherent_i_string_i_string_i_byte__len(t697)
            var t699 bool = t696 < t698
            if t699 {
                var t700 string = value__18.input
                var t701 *ref_int_x = value__18.index
                var t702 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t701)
                var byte__21 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t700, t702)
                var t704 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 34)
                if t704 {
                    var t712 *ref_int_x = value__18.index
                    var t713 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t712)
                    var t714 bool = segment__20 < t713
                    if t714 {
                        var t715 string = value__18.input
                        var t716 *ref_int_x = value__18.index
                        var t717 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t716)
                        var t718 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t715, segment__20, t717)
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t718)
                    } else {}
                    var t706 *ref_int_x = value__18.index
                    var t707 *ref_int_x = value__18.index
                    var t708 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t707)
                    var t709 int = t708 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t706, t709)
                    var t710 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__19)
                    var t711 Result__string__string = Result__string__string_Ok{
                        _0: t710,
                    }
                    retv688 = t711
                    return retv688
                } else {
                    var t721 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 92)
                    if t721 {
                        var t776 *ref_int_x = value__18.index
                        var t777 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t776)
                        var t778 bool = segment__20 < t777
                        if t778 {
                            var t779 string = value__18.input
                            var t780 *ref_int_x = value__18.index
                            var t781 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t780)
                            var t782 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t779, segment__20, t781)
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t782)
                        } else {}
                        var t723 *ref_int_x = value__18.index
                        var t724 *ref_int_x = value__18.index
                        var t725 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t724)
                        var t726 int = t725 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t723, t726)
                        var t769 *ref_int_x = value__18.index
                        var t770 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t769)
                        var t771 string = value__18.input
                        var t772 int = _goml_m_inherent_i_string_i_string_i_byte__len(t771)
                        var t773 bool = t770 >= t772
                        if t773 {
                            var t774 string = _goml_m_std_p_json_p_json__error(value__18, "incomplete escape")
                            var t775 Result__string__string = Result__string__string_Err{
                                _0: t774,
                            }
                            retv688 = t775
                            return retv688
                        } else {
                            var t728 string = value__18.input
                            var t729 *ref_int_x = value__18.index
                            var t730 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t729)
                            var escape__22 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t728, t730)
                            var t731 *ref_int_x = value__18.index
                            var t732 *ref_int_x = value__18.index
                            var t733 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t732)
                            var t734 int = t733 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t731, t734)
                            var t738 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 34)
                            if t738 {
                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 34)
                                var t736 *ref_int_x = value__18.index
                                var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t736)
                                segment__20 = t737
                                continue
                            } else {
                                var t741 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 92)
                                if t741 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 92)
                                    var t736 *ref_int_x = value__18.index
                                    var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t736)
                                    segment__20 = t737
                                    continue
                                } else {
                                    var t744 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 47)
                                    if t744 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 47)
                                        var t736 *ref_int_x = value__18.index
                                        var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t736)
                                        segment__20 = t737
                                        continue
                                    } else {
                                        var t747 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 98)
                                        if t747 {
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
                                            var t736 *ref_int_x = value__18.index
                                            var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t736)
                                            segment__20 = t737
                                            continue
                                        } else {
                                            var t751 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 102)
                                            if t751 {
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
                                                var t736 *ref_int_x = value__18.index
                                                var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t736)
                                                segment__20 = t737
                                                continue
                                            } else {
                                                var t755 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 110)
                                                if t755 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 10)
                                                    var t736 *ref_int_x = value__18.index
                                                    var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t736)
                                                    segment__20 = t737
                                                    continue
                                                } else {
                                                    var t758 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 114)
                                                    if t758 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 13)
                                                        var t736 *ref_int_x = value__18.index
                                                        var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t736)
                                                        segment__20 = t737
                                                        continue
                                                    } else {
                                                        var t761 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 116)
                                                        if t761 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 9)
                                                            var t736 *ref_int_x = value__18.index
                                                            var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t736)
                                                            segment__20 = t737
                                                            continue
                                                        } else {
                                                            var t764 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 117)
                                                            if t764 {
                                                                var mtmp30 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__18, builder__19)
                                                                switch mtmp30.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t736 *ref_int_x = value__18.index
                                                                    var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t736)
                                                                    segment__20 = t737
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x32 string = mtmp30.(Result__unit__string_Err)._0
                                                                    var try_residual__564 string = x32
                                                                    var t766 Result__string__string = Result__string__string_Err{
                                                                        _0: try_residual__564,
                                                                    }
                                                                    retv688 = t766
                                                                    return retv688
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t767 string = _goml_m_std_p_json_p_json__error(value__18, "invalid escape")
                                                                var t768 Result__string__string = Result__string__string_Err{
                                                                    _0: t767,
                                                                }
                                                                retv688 = t768
                                                                return retv688
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
                        var t785 bool = byte__21 < 32
                        if t785 {
                            var t786 string = _goml_m_std_p_json_p_json__error(value__18, "unescaped control character")
                            var t787 Result__string__string = Result__string__string_Err{
                                _0: t786,
                            }
                            retv688 = t787
                            return retv688
                        } else {
                            var t788 *ref_int_x = value__18.index
                            var t789 *ref_int_x = value__18.index
                            var t790 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t789)
                            var t791 int = t790 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t788, t791)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop694
            }
        }
        var t692 string = _goml_m_std_p_json_p_json__error(value__18, "unterminated string")
        var t693 Result__string__string = Result__string__string_Err{
            _0: t692,
        }
        retv688 = t693
        return retv688
    }
}

func _goml_m_std_p_json_p_json__digit(value__25 uint8) bool {
    var retv814 bool
    var t817 bool = value__25 >= 48
    var jp816 bool
    if t817 {
        var t818 bool = value__25 <= 57
        jp816 = t818
    } else {
        jp816 = false
    }
    retv814 = jp816
    return retv814
}

func _goml_m_std_p_json_p_parse__digits(value__26 _goml_m_std_p_json_p_JsonParser) bool {
    var retv820 bool
    var t821 *ref_int_x = value__26.index
    var start__27 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t821)
    Loop_loop826:
    for {
        var t834 *ref_int_x = value__26.index
        var t835 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t834)
        var t836 string = value__26.input
        var t837 int = _goml_m_inherent_i_string_i_string_i_byte__len(t836)
        var t838 bool = t835 < t837
        var jp828 bool
        if t838 {
            var t839 string = value__26.input
            var t840 *ref_int_x = value__26.index
            var t841 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t840)
            var t842 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t839, t841)
            var t843 bool = _goml_m_std_p_json_p_json__digit(t842)
            jp828 = t843
        } else {
            jp828 = false
        }
        if jp828 {
            var t829 *ref_int_x = value__26.index
            var t830 *ref_int_x = value__26.index
            var t831 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t830)
            var t832 int = t831 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t829, t832)
            continue
        } else {
            break Loop_loop826
        }
    }
    var t823 *ref_int_x = value__26.index
    var t824 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t823)
    var t825 bool = t824 > start__27
    retv820 = t825
    return retv820
}

func _goml_m_std_p_json_p_parse__json__number(value__28 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv845 _goml_m_Result____std_p_json_p_Value____string
    var t846 *ref_int_x = value__28.index
    var start__29 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t846)
    var t968 string = value__28.input
    var t969 *ref_int_x = value__28.index
    var t970 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t969)
    var t971 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t968, t970)
    var t972 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t971, 45)
    if t972 {
        var t973 *ref_int_x = value__28.index
        var t974 *ref_int_x = value__28.index
        var t975 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t974)
        var t976 int = t975 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t973, t976)
    } else {}
    var t931 *ref_int_x = value__28.index
    var t932 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t931)
    var t933 string = value__28.input
    var t934 int = _goml_m_inherent_i_string_i_string_i_byte__len(t933)
    var t935 bool = t932 >= t934
    if t935 {
        var t936 string = _goml_m_std_p_json_p_json__error(value__28, "incomplete number")
        var t937 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t936,
        }
        retv845 = t937
        return retv845
    } else {
        var t939 string = value__28.input
        var t940 *ref_int_x = value__28.index
        var t941 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t940)
        var t942 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t939, t941)
        var t943 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t942, 48)
        if t943 {
            var t944 *ref_int_x = value__28.index
            var t945 *ref_int_x = value__28.index
            var t946 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t945)
            var t947 int = t946 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t944, t947)
            var t953 *ref_int_x = value__28.index
            var t954 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t953)
            var t955 string = value__28.input
            var t956 int = _goml_m_inherent_i_string_i_string_i_byte__len(t955)
            var t957 bool = t954 < t956
            var jp950 bool
            if t957 {
                var t958 string = value__28.input
                var t959 *ref_int_x = value__28.index
                var t960 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t959)
                var t961 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t958, t960)
                var t962 bool = _goml_m_std_p_json_p_json__digit(t961)
                jp950 = t962
            } else {
                jp950 = false
            }
            if jp950 {
                var t951 string = _goml_m_std_p_json_p_json__error(value__28, "invalid leading zero")
                var t952 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t951,
                }
                retv845 = t952
                return retv845
            } else {
                var t921 *ref_int_x = value__28.index
                var t922 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t921)
                var t923 string = value__28.input
                var t924 int = _goml_m_inherent_i_string_i_string_i_byte__len(t923)
                var t925 bool = t922 < t924
                var jp911 bool
                if t925 {
                    var t926 string = value__28.input
                    var t927 *ref_int_x = value__28.index
                    var t928 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t927)
                    var t929 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t926, t928)
                    var t930 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t929, 46)
                    jp911 = t930
                } else {
                    jp911 = false
                }
                if jp911 {
                    var t912 *ref_int_x = value__28.index
                    var t913 *ref_int_x = value__28.index
                    var t914 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t913)
                    var t915 int = t914 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t912, t915)
                    var t917 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t918 bool = !t917
                    if t918 {
                        var t919 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t920 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t919,
                        }
                        retv845 = t920
                        return retv845
                    } else {
                        var t893 *ref_int_x = value__28.index
                        var t894 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t893)
                        var t895 string = value__28.input
                        var t896 int = _goml_m_inherent_i_string_i_string_i_byte__len(t895)
                        var t897 bool = t894 < t896
                        var jp858 bool
                        if t897 {
                            var t900 string = value__28.input
                            var t901 *ref_int_x = value__28.index
                            var t902 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t901)
                            var t903 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t900, t902)
                            var t904 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t903, 101)
                            var jp899 bool
                            if t904 {
                                jp899 = true
                            } else {
                                var t905 string = value__28.input
                                var t906 *ref_int_x = value__28.index
                                var t907 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t906)
                                var t908 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t905, t907)
                                var t909 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t908, 69)
                                jp899 = t909
                            }
                            jp858 = jp899
                        } else {
                            jp858 = false
                        }
                        if jp858 {
                            var t859 *ref_int_x = value__28.index
                            var t860 *ref_int_x = value__28.index
                            var t861 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t860)
                            var t862 int = t861 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t859, t862)
                            var t876 *ref_int_x = value__28.index
                            var t877 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t876)
                            var t878 string = value__28.input
                            var t879 int = _goml_m_inherent_i_string_i_string_i_byte__len(t878)
                            var t880 bool = t877 < t879
                            var jp870 bool
                            if t880 {
                                var t883 string = value__28.input
                                var t884 *ref_int_x = value__28.index
                                var t885 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t884)
                                var t886 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t883, t885)
                                var t887 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t886, 43)
                                var jp882 bool
                                if t887 {
                                    jp882 = true
                                } else {
                                    var t888 string = value__28.input
                                    var t889 *ref_int_x = value__28.index
                                    var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                                    var t891 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t888, t890)
                                    var t892 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t891, 45)
                                    jp882 = t892
                                }
                                jp870 = jp882
                            } else {
                                jp870 = false
                            }
                            if jp870 {
                                var t871 *ref_int_x = value__28.index
                                var t872 *ref_int_x = value__28.index
                                var t873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t872)
                                var t874 int = t873 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t871, t874)
                            } else {}
                            var t865 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t866 bool = !t865
                            if t866 {
                                var t867 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t868 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t867,
                                }
                                retv845 = t868
                                return retv845
                            } else {
                                var t851 string = value__28.input
                                var t852 *ref_int_x = value__28.index
                                var t853 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t852)
                                var t854 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t851, start__29, t853)
                                var t855 _goml_m_std_p_json_p_Value = Number{
                                    _0: t854,
                                }
                                var t856 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t855,
                                }
                                retv845 = t856
                                return retv845
                            }
                        } else {
                            var t851 string = value__28.input
                            var t852 *ref_int_x = value__28.index
                            var t853 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t852)
                            var t854 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t851, start__29, t853)
                            var t855 _goml_m_std_p_json_p_Value = Number{
                                _0: t854,
                            }
                            var t856 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t855,
                            }
                            retv845 = t856
                            return retv845
                        }
                    }
                } else {
                    var t893 *ref_int_x = value__28.index
                    var t894 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t893)
                    var t895 string = value__28.input
                    var t896 int = _goml_m_inherent_i_string_i_string_i_byte__len(t895)
                    var t897 bool = t894 < t896
                    var jp858 bool
                    if t897 {
                        var t900 string = value__28.input
                        var t901 *ref_int_x = value__28.index
                        var t902 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t901)
                        var t903 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t900, t902)
                        var t904 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t903, 101)
                        var jp899 bool
                        if t904 {
                            jp899 = true
                        } else {
                            var t905 string = value__28.input
                            var t906 *ref_int_x = value__28.index
                            var t907 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t906)
                            var t908 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t905, t907)
                            var t909 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t908, 69)
                            jp899 = t909
                        }
                        jp858 = jp899
                    } else {
                        jp858 = false
                    }
                    if jp858 {
                        var t859 *ref_int_x = value__28.index
                        var t860 *ref_int_x = value__28.index
                        var t861 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t860)
                        var t862 int = t861 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t859, t862)
                        var t876 *ref_int_x = value__28.index
                        var t877 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t876)
                        var t878 string = value__28.input
                        var t879 int = _goml_m_inherent_i_string_i_string_i_byte__len(t878)
                        var t880 bool = t877 < t879
                        var jp870 bool
                        if t880 {
                            var t883 string = value__28.input
                            var t884 *ref_int_x = value__28.index
                            var t885 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t884)
                            var t886 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t883, t885)
                            var t887 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t886, 43)
                            var jp882 bool
                            if t887 {
                                jp882 = true
                            } else {
                                var t888 string = value__28.input
                                var t889 *ref_int_x = value__28.index
                                var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                                var t891 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t888, t890)
                                var t892 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t891, 45)
                                jp882 = t892
                            }
                            jp870 = jp882
                        } else {
                            jp870 = false
                        }
                        if jp870 {
                            var t871 *ref_int_x = value__28.index
                            var t872 *ref_int_x = value__28.index
                            var t873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t872)
                            var t874 int = t873 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t871, t874)
                        } else {}
                        var t865 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t866 bool = !t865
                        if t866 {
                            var t867 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t868 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t867,
                            }
                            retv845 = t868
                            return retv845
                        } else {
                            var t851 string = value__28.input
                            var t852 *ref_int_x = value__28.index
                            var t853 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t852)
                            var t854 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t851, start__29, t853)
                            var t855 _goml_m_std_p_json_p_Value = Number{
                                _0: t854,
                            }
                            var t856 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t855,
                            }
                            retv845 = t856
                            return retv845
                        }
                    } else {
                        var t851 string = value__28.input
                        var t852 *ref_int_x = value__28.index
                        var t853 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t852)
                        var t854 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t851, start__29, t853)
                        var t855 _goml_m_std_p_json_p_Value = Number{
                            _0: t854,
                        }
                        var t856 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t855,
                        }
                        retv845 = t856
                        return retv845
                    }
                }
            }
        } else {
            var t964 bool = _goml_m_std_p_json_p_parse__digits(value__28)
            var t965 bool = !t964
            if t965 {
                var t966 string = _goml_m_std_p_json_p_json__error(value__28, "expected number")
                var t967 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t966,
                }
                retv845 = t967
                return retv845
            } else {
                var t921 *ref_int_x = value__28.index
                var t922 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t921)
                var t923 string = value__28.input
                var t924 int = _goml_m_inherent_i_string_i_string_i_byte__len(t923)
                var t925 bool = t922 < t924
                var jp911 bool
                if t925 {
                    var t926 string = value__28.input
                    var t927 *ref_int_x = value__28.index
                    var t928 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t927)
                    var t929 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t926, t928)
                    var t930 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t929, 46)
                    jp911 = t930
                } else {
                    jp911 = false
                }
                if jp911 {
                    var t912 *ref_int_x = value__28.index
                    var t913 *ref_int_x = value__28.index
                    var t914 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t913)
                    var t915 int = t914 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t912, t915)
                    var t917 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t918 bool = !t917
                    if t918 {
                        var t919 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t920 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t919,
                        }
                        retv845 = t920
                        return retv845
                    } else {
                        var t893 *ref_int_x = value__28.index
                        var t894 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t893)
                        var t895 string = value__28.input
                        var t896 int = _goml_m_inherent_i_string_i_string_i_byte__len(t895)
                        var t897 bool = t894 < t896
                        var jp858 bool
                        if t897 {
                            var t900 string = value__28.input
                            var t901 *ref_int_x = value__28.index
                            var t902 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t901)
                            var t903 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t900, t902)
                            var t904 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t903, 101)
                            var jp899 bool
                            if t904 {
                                jp899 = true
                            } else {
                                var t905 string = value__28.input
                                var t906 *ref_int_x = value__28.index
                                var t907 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t906)
                                var t908 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t905, t907)
                                var t909 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t908, 69)
                                jp899 = t909
                            }
                            jp858 = jp899
                        } else {
                            jp858 = false
                        }
                        if jp858 {
                            var t859 *ref_int_x = value__28.index
                            var t860 *ref_int_x = value__28.index
                            var t861 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t860)
                            var t862 int = t861 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t859, t862)
                            var t876 *ref_int_x = value__28.index
                            var t877 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t876)
                            var t878 string = value__28.input
                            var t879 int = _goml_m_inherent_i_string_i_string_i_byte__len(t878)
                            var t880 bool = t877 < t879
                            var jp870 bool
                            if t880 {
                                var t883 string = value__28.input
                                var t884 *ref_int_x = value__28.index
                                var t885 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t884)
                                var t886 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t883, t885)
                                var t887 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t886, 43)
                                var jp882 bool
                                if t887 {
                                    jp882 = true
                                } else {
                                    var t888 string = value__28.input
                                    var t889 *ref_int_x = value__28.index
                                    var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                                    var t891 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t888, t890)
                                    var t892 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t891, 45)
                                    jp882 = t892
                                }
                                jp870 = jp882
                            } else {
                                jp870 = false
                            }
                            if jp870 {
                                var t871 *ref_int_x = value__28.index
                                var t872 *ref_int_x = value__28.index
                                var t873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t872)
                                var t874 int = t873 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t871, t874)
                            } else {}
                            var t865 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t866 bool = !t865
                            if t866 {
                                var t867 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t868 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t867,
                                }
                                retv845 = t868
                                return retv845
                            } else {
                                var t851 string = value__28.input
                                var t852 *ref_int_x = value__28.index
                                var t853 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t852)
                                var t854 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t851, start__29, t853)
                                var t855 _goml_m_std_p_json_p_Value = Number{
                                    _0: t854,
                                }
                                var t856 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t855,
                                }
                                retv845 = t856
                                return retv845
                            }
                        } else {
                            var t851 string = value__28.input
                            var t852 *ref_int_x = value__28.index
                            var t853 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t852)
                            var t854 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t851, start__29, t853)
                            var t855 _goml_m_std_p_json_p_Value = Number{
                                _0: t854,
                            }
                            var t856 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t855,
                            }
                            retv845 = t856
                            return retv845
                        }
                    }
                } else {
                    var t893 *ref_int_x = value__28.index
                    var t894 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t893)
                    var t895 string = value__28.input
                    var t896 int = _goml_m_inherent_i_string_i_string_i_byte__len(t895)
                    var t897 bool = t894 < t896
                    var jp858 bool
                    if t897 {
                        var t900 string = value__28.input
                        var t901 *ref_int_x = value__28.index
                        var t902 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t901)
                        var t903 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t900, t902)
                        var t904 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t903, 101)
                        var jp899 bool
                        if t904 {
                            jp899 = true
                        } else {
                            var t905 string = value__28.input
                            var t906 *ref_int_x = value__28.index
                            var t907 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t906)
                            var t908 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t905, t907)
                            var t909 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t908, 69)
                            jp899 = t909
                        }
                        jp858 = jp899
                    } else {
                        jp858 = false
                    }
                    if jp858 {
                        var t859 *ref_int_x = value__28.index
                        var t860 *ref_int_x = value__28.index
                        var t861 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t860)
                        var t862 int = t861 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t859, t862)
                        var t876 *ref_int_x = value__28.index
                        var t877 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t876)
                        var t878 string = value__28.input
                        var t879 int = _goml_m_inherent_i_string_i_string_i_byte__len(t878)
                        var t880 bool = t877 < t879
                        var jp870 bool
                        if t880 {
                            var t883 string = value__28.input
                            var t884 *ref_int_x = value__28.index
                            var t885 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t884)
                            var t886 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t883, t885)
                            var t887 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t886, 43)
                            var jp882 bool
                            if t887 {
                                jp882 = true
                            } else {
                                var t888 string = value__28.input
                                var t889 *ref_int_x = value__28.index
                                var t890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t889)
                                var t891 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t888, t890)
                                var t892 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t891, 45)
                                jp882 = t892
                            }
                            jp870 = jp882
                        } else {
                            jp870 = false
                        }
                        if jp870 {
                            var t871 *ref_int_x = value__28.index
                            var t872 *ref_int_x = value__28.index
                            var t873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t872)
                            var t874 int = t873 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t871, t874)
                        } else {}
                        var t865 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t866 bool = !t865
                        if t866 {
                            var t867 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t868 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t867,
                            }
                            retv845 = t868
                            return retv845
                        } else {
                            var t851 string = value__28.input
                            var t852 *ref_int_x = value__28.index
                            var t853 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t852)
                            var t854 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t851, start__29, t853)
                            var t855 _goml_m_std_p_json_p_Value = Number{
                                _0: t854,
                            }
                            var t856 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t855,
                            }
                            retv845 = t856
                            return retv845
                        }
                    } else {
                        var t851 string = value__28.input
                        var t852 *ref_int_x = value__28.index
                        var t853 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t852)
                        var t854 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t851, start__29, t853)
                        var t855 _goml_m_std_p_json_p_Value = Number{
                            _0: t854,
                        }
                        var t856 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t855,
                        }
                        retv845 = t856
                        return retv845
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__30 _goml_m_std_p_json_p_JsonParser, expected__31 string, result__32 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var retv979 _goml_m_Result____std_p_json_p_Value____string
    var t992 *ref_int_x = value__30.index
    var t993 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t992)
    var t994 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__31)
    var t995 int = t993 + t994
    var t996 string = value__30.input
    var t997 int = _goml_m_inherent_i_string_i_string_i_byte__len(t996)
    var t998 bool = t995 <= t997
    var jp983 bool
    if t998 {
        var t999 string = value__30.input
        var t1000 *ref_int_x = value__30.index
        var t1001 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1000)
        var t1002 *ref_int_x = value__30.index
        var t1003 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1002)
        var t1004 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__31)
        var t1005 int = t1003 + t1004
        var t1006 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t999, t1001, t1005)
        var t1007 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1006, expected__31)
        jp983 = t1007
    } else {
        jp983 = false
    }
    var jp981 _goml_m_Result____std_p_json_p_Value____string
    if jp983 {
        var t984 *ref_int_x = value__30.index
        var t985 *ref_int_x = value__30.index
        var t986 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t985)
        var t987 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__31)
        var t988 int = t986 + t987
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t984, t988)
        var t989 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__32,
        }
        jp981 = t989
    } else {
        var t990 string = _goml_m_std_p_json_p_json__error(value__30, "invalid literal")
        var t991 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t990,
        }
        jp981 = t991
    }
    retv979 = jp981
    return retv979
}

func _goml_m_std_p_json_p_parse__json__array(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv1009 _goml_m_Result____std_p_json_p_Value____string
    var t1010 *ref_int_x = value__33.index
    var t1011 *ref_int_x = value__33.index
    var t1012 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1011)
    var t1013 int = t1012 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1010, t1013)
    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
    var result__34 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t1068 *ref_int_x = value__33.index
    var t1069 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1068)
    var t1070 string = value__33.input
    var t1071 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1070)
    var t1072 bool = t1069 < t1071
    var jp1061 bool
    if t1072 {
        var t1073 string = value__33.input
        var t1074 *ref_int_x = value__33.index
        var t1075 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1074)
        var t1076 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1073, t1075)
        var t1077 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1076, 93)
        jp1061 = t1077
    } else {
        jp1061 = false
    }
    if jp1061 {
        var t1062 *ref_int_x = value__33.index
        var t1063 *ref_int_x = value__33.index
        var t1064 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1063)
        var t1065 int = t1064 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1062, t1065)
        var t1066 _goml_m_std_p_json_p_Value = Array{
            _0: result__34,
        }
        var t1067 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1066,
        }
        retv1009 = t1067
        return retv1009
    } else {
        Loop_loop1018:
        for {
            var t1019 *ref_int_x = value__33.index
            var t1020 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1019)
            var t1021 string = value__33.input
            var t1022 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1021)
            var t1023 bool = t1020 < t1022
            if t1023 {
                var mtmp50 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__33)
                var jp1025 _goml_m_std_p_json_p_Value
                switch mtmp50.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x51 _goml_m_std_p_json_p_Value = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    var try_value__1061 _goml_m_std_p_json_p_Value = x51
                    jp1025 = try_value__1061
                    var item__35 _goml_m_std_p_json_p_Value = jp1025
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(result__34, item__35)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                    var t1027 *ref_int_x = value__33.index
                    var t1028 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1027)
                    var t1029 string = value__33.input
                    var t1030 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1029)
                    var t1031 bool = t1028 >= t1030
                    if t1031 {
                        var t1032 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
                        var t1033 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1032,
                        }
                        retv1009 = t1033
                        return retv1009
                    } else {
                        var t1035 string = value__33.input
                        var t1036 *ref_int_x = value__33.index
                        var t1037 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1036)
                        var t1038 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1035, t1037)
                        var t1039 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1038, 93)
                        if t1039 {
                            var t1040 *ref_int_x = value__33.index
                            var t1041 *ref_int_x = value__33.index
                            var t1042 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1041)
                            var t1043 int = t1042 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1040, t1043)
                            var t1044 _goml_m_std_p_json_p_Value = Array{
                                _0: result__34,
                            }
                            var t1045 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1044,
                            }
                            retv1009 = t1045
                            return retv1009
                        } else {
                            var t1047 string = value__33.input
                            var t1048 *ref_int_x = value__33.index
                            var t1049 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1048)
                            var t1050 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1047, t1049)
                            var t1051 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1050, 44)
                            if t1051 {
                                var t1052 *ref_int_x = value__33.index
                                var t1053 *ref_int_x = value__33.index
                                var t1054 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1053)
                                var t1055 int = t1054 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1052, t1055)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                                continue
                            } else {
                                var t1057 string = _goml_m_std_p_json_p_json__error(value__33, "expected array separator")
                                var t1058 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1057,
                                }
                                retv1009 = t1058
                                return retv1009
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x52 string = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var try_residual__1061 string = x52
                    var t1059 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: try_residual__1061,
                    }
                    retv1009 = t1059
                    return retv1009
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1018
            }
        }
        var t1016 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
        var t1017 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1016,
        }
        retv1009 = t1017
        return retv1009
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__36 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv1079 _goml_m_Result____std_p_json_p_Value____string
    var t1080 *ref_int_x = value__36.index
    var t1081 *ref_int_x = value__36.index
    var t1082 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1081)
    var t1083 int = t1082 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1080, t1083)
    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
    var result__37 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t1163 *ref_int_x = value__36.index
    var t1164 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1163)
    var t1165 string = value__36.input
    var t1166 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1165)
    var t1167 bool = t1164 < t1166
    var jp1156 bool
    if t1167 {
        var t1168 string = value__36.input
        var t1169 *ref_int_x = value__36.index
        var t1170 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1169)
        var t1171 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1168, t1170)
        var t1172 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1171, 125)
        jp1156 = t1172
    } else {
        jp1156 = false
    }
    if jp1156 {
        var t1157 *ref_int_x = value__36.index
        var t1158 *ref_int_x = value__36.index
        var t1159 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1158)
        var t1160 int = t1159 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1157, t1160)
        var t1161 _goml_m_std_p_json_p_Value = Object{
            _0: result__37,
        }
        var t1162 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1161,
        }
        retv1079 = t1162
        return retv1079
    } else {
        Loop_loop1088:
        for {
            var t1089 *ref_int_x = value__36.index
            var t1090 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1089)
            var t1091 string = value__36.input
            var t1092 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1091)
            var t1093 bool = t1090 < t1092
            if t1093 {
                var mtmp62 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__36)
                var jp1095 string
                switch mtmp62.(type) {
                case Result__string__string_Ok:
                    var x63 string = mtmp62.(Result__string__string_Ok)._0
                    var try_value__1216 string = x63
                    jp1095 = try_value__1216
                    var name__38 string = jp1095
                    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                    var t1143 *ref_int_x = value__36.index
                    var t1144 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1143)
                    var t1145 string = value__36.input
                    var t1146 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1145)
                    var t1147 bool = t1144 >= t1146
                    var jp1135 bool
                    if t1147 {
                        jp1135 = true
                    } else {
                        var t1148 string = value__36.input
                        var t1149 *ref_int_x = value__36.index
                        var t1150 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1149)
                        var t1151 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1148, t1150)
                        var t1152 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1151, 58)
                        var t1153 bool = !t1152
                        jp1135 = t1153
                    }
                    if jp1135 {
                        var t1136 string = _goml_m_std_p_json_p_json__error(value__36, "expected object colon")
                        var t1137 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1136,
                        }
                        retv1079 = t1137
                        return retv1079
                    } else {
                        var t1138 *ref_int_x = value__36.index
                        var t1139 *ref_int_x = value__36.index
                        var t1140 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1139)
                        var t1141 int = t1140 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1138, t1141)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                        var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__36)
                        var jp1098 _goml_m_std_p_json_p_Value
                        switch mtmp68.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            var try_value__1265 _goml_m_std_p_json_p_Value = x69
                            jp1098 = try_value__1265
                            var item__39 _goml_m_std_p_json_p_Value = jp1098
                            var t1099 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: name__38,
                                _1: item__39,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__37, t1099)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                            var t1101 *ref_int_x = value__36.index
                            var t1102 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1101)
                            var t1103 string = value__36.input
                            var t1104 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1103)
                            var t1105 bool = t1102 >= t1104
                            if t1105 {
                                var t1106 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
                                var t1107 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1106,
                                }
                                retv1079 = t1107
                                return retv1079
                            } else {
                                var t1109 string = value__36.input
                                var t1110 *ref_int_x = value__36.index
                                var t1111 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1110)
                                var t1112 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1109, t1111)
                                var t1113 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1112, 125)
                                if t1113 {
                                    var t1114 *ref_int_x = value__36.index
                                    var t1115 *ref_int_x = value__36.index
                                    var t1116 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1115)
                                    var t1117 int = t1116 + 1
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1114, t1117)
                                    var t1118 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__37,
                                    }
                                    var t1119 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1118,
                                    }
                                    retv1079 = t1119
                                    return retv1079
                                } else {
                                    var t1121 string = value__36.input
                                    var t1122 *ref_int_x = value__36.index
                                    var t1123 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1122)
                                    var t1124 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1121, t1123)
                                    var t1125 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1124, 44)
                                    if t1125 {
                                        var t1126 *ref_int_x = value__36.index
                                        var t1127 *ref_int_x = value__36.index
                                        var t1128 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1127)
                                        var t1129 int = t1128 + 1
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1126, t1129)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                                        continue
                                    } else {
                                        var t1131 string = _goml_m_std_p_json_p_json__error(value__36, "expected object separator")
                                        var t1132 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1131,
                                        }
                                        retv1079 = t1132
                                        return retv1079
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var try_residual__1265 string = x70
                            var t1133 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: try_residual__1265,
                            }
                            retv1079 = t1133
                            return retv1079
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x64 string = mtmp62.(Result__string__string_Err)._0
                    var try_residual__1216 string = x64
                    var t1154 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: try_residual__1216,
                    }
                    retv1079 = t1154
                    return retv1079
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1088
            }
        }
        var t1086 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
        var t1087 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1086,
        }
        retv1079 = t1087
        return retv1079
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__40 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv1174 _goml_m_Result____std_p_json_p_Value____string
    _goml_m_std_p_json_p_skip__json__whitespace(value__40)
    var t1177 *ref_int_x = value__40.index
    var t1178 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1177)
    var t1179 string = value__40.input
    var t1180 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1179)
    var t1181 bool = t1178 >= t1180
    var jp1176 _goml_m_Result____std_p_json_p_Value____string
    if t1181 {
        var t1182 string = _goml_m_std_p_json_p_json__error(value__40, "expected JSON value")
        var t1183 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1182,
        }
        jp1176 = t1183
    } else {
        var t1184 string = value__40.input
        var t1185 *ref_int_x = value__40.index
        var t1186 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1185)
        var mtmp77 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1184, t1186)
        var jp1188 _goml_m_Result____std_p_json_p_Value____string
        switch mtmp77 {
        case 123:
            var t1189 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__40)
            jp1188 = t1189
        case 91:
            var t1190 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__40)
            jp1188 = t1190
        case 34:
            var mtmp78 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__40)
            var jp1192 _goml_m_Result____std_p_json_p_Value____string
            switch mtmp78.(type) {
            case Result__string__string_Ok:
                var x79 string = mtmp78.(Result__string__string_Ok)._0
                var text__41 string = x79
                var t1193 _goml_m_std_p_json_p_Value = String{
                    _0: text__41,
                }
                var t1194 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1193,
                }
                jp1192 = t1194
            case Result__string__string_Err:
                var x80 string = mtmp78.(Result__string__string_Err)._0
                var error__42 string = x80
                var t1195 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: error__42,
                }
                jp1192 = t1195
            default:
                panic("non-exhaustive match")
            }
            jp1188 = jp1192
        case 116:
            var t1196 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t1197 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "true", t1196)
            jp1188 = t1197
        case 102:
            var t1198 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t1199 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "false", t1198)
            jp1188 = t1199
        case 110:
            var t1200 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "null", Null{})
            jp1188 = t1200
        default:
            var byte__43 uint8 = mtmp77
            var t1208 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__43, 45)
            var jp1204 bool
            if t1208 {
                jp1204 = true
            } else {
                var t1209 bool = _goml_m_std_p_json_p_json__digit(byte__43)
                jp1204 = t1209
            }
            var jp1202 _goml_m_Result____std_p_json_p_Value____string
            if jp1204 {
                var t1205 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__40)
                jp1202 = t1205
            } else {
                var t1206 string = _goml_m_std_p_json_p_json__error(value__40, "unexpected JSON token")
                var t1207 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1206,
                }
                jp1202 = t1207
            }
            jp1188 = jp1202
        }
        jp1176 = jp1188
    }
    retv1174 = jp1176
    return retv1174
}

func _goml_m_std_p_json_p_parse(input__44 string) _goml_m_Result____std_p_json_p_Value____string {
    var retv1211 _goml_m_Result____std_p_json_p_Value____string
    var parser__45 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_json__parser(input__44)
    var mtmp81 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__45)
    var jp1213 _goml_m_std_p_json_p_Value
    switch mtmp81.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x82 _goml_m_std_p_json_p_Value = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var try_value__1442 _goml_m_std_p_json_p_Value = x82
        jp1213 = try_value__1442
        var result__46 _goml_m_std_p_json_p_Value = jp1213
        _goml_m_std_p_json_p_skip__json__whitespace(parser__45)
        var t1216 *ref_int_x = parser__45.index
        var t1217 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1216)
        var t1218 int = _goml_m_inherent_i_string_i_string_i_byte__len(input__44)
        var t1219 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1217, t1218)
        var jp1215 _goml_m_Result____std_p_json_p_Value____string
        if t1219 {
            var t1220 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: result__46,
            }
            jp1215 = t1220
        } else {
            var t1221 string = _goml_m_std_p_json_p_json__error(parser__45, "trailing JSON data")
            var t1222 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1221,
            }
            jp1215 = t1222
        }
        retv1211 = jp1215
        return retv1211
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x83 string = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var try_residual__1442 string = x83
        var t1223 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: try_residual__1442,
        }
        retv1211 = t1223
        return retv1211
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_json__hex__digit(value__47 uint8) rune {
    var retv1225 rune
    var t1226 int = int(uint8(value__47))
    var t1227 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", t1226)
    retv1225 = t1227
    return retv1225
}

func _goml_m_std_p_json_p_write__json__string(builder__48 _goml_m_std_p_text_p_StringBuilder, value__49 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    var start__50 int = 0
    var for_index86 int = 0
    var for_limit87 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    Loop_loop1237:
    for {
        var t1238 bool = for_index86 < for_limit87
        if t1238 {
            var for_item88 int = for_index86
            var t1239 int = for_index86 + 1
            for_index86 = t1239
            var index__51 int = for_item88
            var byte__52 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__49, index__51)
            var t1292 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 34)
            var jp1290 bool
            if t1292 {
                jp1290 = true
            } else {
                var t1293 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 92)
                jp1290 = t1293
            }
            var jp1287 bool
            if jp1290 {
                jp1287 = true
            } else {
                var t1291 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 8)
                jp1287 = t1291
            }
            var jp1284 bool
            if jp1287 {
                jp1284 = true
            } else {
                var t1288 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 9)
                jp1284 = t1288
            }
            var jp1281 bool
            if jp1284 {
                jp1281 = true
            } else {
                var t1285 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 10)
                jp1281 = t1285
            }
            var jp1278 bool
            if jp1281 {
                jp1278 = true
            } else {
                var t1282 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 12)
                jp1278 = t1282
            }
            var jp1275 bool
            if jp1278 {
                jp1275 = true
            } else {
                var t1279 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 13)
                jp1275 = t1279
            }
            var jp1242 bool
            if jp1275 {
                jp1242 = true
            } else {
                var t1276 bool = byte__52 < 32
                jp1242 = t1276
            }
            if jp1242 {
                var t1271 bool = start__50 < index__51
                if t1271 {
                    var t1272 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__49, start__50, index__51)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1272)
                } else {}
                var t1246 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 34)
                if t1246 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\"")
                } else {
                    var t1249 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 92)
                    if t1249 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\\")
                    } else {
                        var t1252 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 8)
                        if t1252 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\b")
                        } else {
                            var t1255 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 9)
                            if t1255 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\t")
                            } else {
                                var t1258 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 10)
                                if t1258 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\n")
                                } else {
                                    var t1261 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 12)
                                    if t1261 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\f")
                                    } else {
                                        var t1264 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 13)
                                        if t1264 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\u00")
                                            var t1266 uint8 = byte__52 / 16
                                            var t1267 rune = _goml_m_std_p_json_p_json__hex__digit(t1266)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, t1267)
                                            var t1268_rhs uint8 = 16
                                            var t1268 uint8 = byte__52 % t1268_rhs
                                            var t1269 rune = _goml_m_std_p_json_p_json__hex__digit(t1268)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, t1269)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1245 int = index__51 + 1
                start__50 = t1245
            } else {}
            continue
        } else {
            break Loop_loop1237
        }
    }
    var t1232 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    var t1233 bool = start__50 < t1232
    if t1233 {
        var t1234 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
        var t1235 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__49, start__50, t1234)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1235)
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
        Loop_loop1298:
        for {
            var t1299 bool = for_index105 < for_limit104
            if t1299 {
                var for_item106 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source103, for_index105)
                var t1300 int = for_index105 + 1
                for_index105 = t1300
                var field__57 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item106
                var t1306 bool = index__56 > 0
                if t1306 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 44)
                } else {}
                var t1302 string = field__57._0
                _goml_m_std_p_json_p_write__json__string(builder__53, t1302)
                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 58)
                var t1303 _goml_m_std_p_json_p_Value = field__57._1
                _goml_m_std_p_json_p_write__json__value(builder__53, t1303)
                var compound_old112 int = index__56
                var compound_value113 int = 1
                var t1304 int = compound_old112 + compound_value113
                index__56 = t1304
                continue
            } else {
                break Loop_loop1298
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
        Loop_loop1310:
        for {
            var t1311 bool = for_index119 < for_limit118
            if t1311 {
                var for_item120 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(for_source117, for_index119)
                var t1312 int = for_index119 + 1
                for_index119 = t1312
                var item__60 _goml_m_std_p_json_p_Value = for_item120
                var t1316 bool = index__59 > 0
                if t1316 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 44)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__53, item__60)
                var compound_old124 int = index__59
                var compound_value125 int = 1
                var t1314 int = compound_old124 + compound_value125
                index__59 = t1314
                continue
            } else {
                break Loop_loop1310
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
        var jp1321 string
        if value__63 {
            jp1321 = "true"
        } else {
            jp1321 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, jp1321)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__64 _goml_m_std_p_json_p_Value) string {
    var retv1325 string
    var builder__65 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
    _goml_m_std_p_json_p_write__json__value(builder__65, value__64)
    var t1326 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__65)
    retv1325 = t1326
    return retv1325
}

func _goml_m_std_p_json_p_field(value__66 _goml_m_std_p_json_p_Value, name__67 string) _goml_m_Option____std_p_json_p_Value {
    var retv1328 _goml_m_Option____std_p_json_p_Value
    var jp1330 _goml_m_Option____std_p_json_p_Value
    switch value__66.(type) {
    case Object:
        var x129 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__66.(Object)._0
        var fields__68 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x129
        var for_source134 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = fields__68
        var for_limit135 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source134)
        var for_index136 int = 0
        Loop_loop1332:
        for {
            var t1333 bool = for_index136 < for_limit135
            if t1333 {
                var for_item137 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source134, for_index136)
                var t1334 int = for_index136 + 1
                for_index136 = t1334
                var field__69 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item137
                var t1336 string = field__69._0
                var t1337 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1336, name__67)
                if t1337 {
                    var t1338 _goml_m_std_p_json_p_Value = field__69._1
                    var t1339 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1338,
                    }
                    retv1328 = t1339
                    return retv1328
                } else {
                    continue
                }
            } else {
                break Loop_loop1332
            }
        }
        jp1330 = _goml_m_Option____std_p_json_p_Value_None{}
        retv1328 = jp1330
        return retv1328
    default:
        jp1330 = _goml_m_Option____std_p_json_p_Value_None{}
        retv1328 = jp1330
        return retv1328
    }
}

func _goml_m_std_p_json_p_as__string(value__70 _goml_m_std_p_json_p_Value) Option__string {
    var retv1341 Option__string
    var jp1343 Option__string
    switch value__70.(type) {
    case String:
        var x142 string = value__70.(String)._0
        var text__71 string = x142
        var t1344 Option__string = Option__string_Some{
            _0: text__71,
        }
        jp1343 = t1344
    default:
        jp1343 = Option__string_None{}
    }
    retv1341 = jp1343
    return retv1341
}

func _goml_m_std_p_json_p_parse__json__int__text(value__72 string) Option__int {
    var retv1346 Option__int
    var t1349 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__72)
    var t1350 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1349, 0)
    var jp1348 Option__int
    if t1350 {
        jp1348 = Option__int_None{}
        retv1346 = jp1348
        return retv1346
    } else {
        var t1351 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__72, 0)
        var negative__73 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1351, 45)
        var jp1353 int
        if negative__73 {
            jp1353 = 1
        } else {
            jp1353 = 0
        }
        var index__74 int = jp1353
        var result__75 int = 0
        var t1374 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__72)
        var t1375 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(index__74, t1374)
        if t1375 {
            retv1346 = Option__int_None{}
            return retv1346
        } else {
            Loop_loop1360:
            for {
                var t1361 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__72)
                var t1362 bool = index__74 < t1361
                if t1362 {
                    var byte__76 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__72, index__74)
                    var t1372 bool = byte__76 < 48
                    var jp1367 bool
                    if t1372 {
                        jp1367 = true
                    } else {
                        var t1373 bool = byte__76 > 57
                        jp1367 = t1373
                    }
                    if jp1367 {
                        retv1346 = Option__int_None{}
                        return retv1346
                    } else {
                        var t1368 int = result__75 * 10
                        var t1369 uint8 = byte__76 - 48
                        var t1370 int = int(uint8(t1369))
                        var t1371 int = t1368 + t1370
                        result__75 = t1371
                        var compound_old148 int = index__74
                        var compound_value149 int = 1
                        var t1364 int = compound_old148 + compound_value149
                        index__74 = t1364
                        continue
                    }
                } else {
                    break Loop_loop1360
                }
            }
            var jp1357 int
            if negative__73 {
                var t1359 int = 0 - result__75
                jp1357 = t1359
            } else {
                jp1357 = result__75
            }
            var t1358 Option__int = Option__int_Some{
                _0: jp1357,
            }
            jp1348 = t1358
            retv1346 = jp1348
            return retv1346
        }
    }
}

func _goml_m_std_p_json_p_as__int(value__77 _goml_m_std_p_json_p_Value) Option__int {
    var retv1377 Option__int
    var jp1379 Option__int
    switch value__77.(type) {
    case Number:
        var x155 string = value__77.(Number)._0
        var number__78 string = x155
        var t1380 Option__int = _goml_m_std_p_json_p_parse__json__int__text(number__78)
        jp1379 = t1380
    default:
        jp1379 = Option__int_None{}
    }
    retv1377 = jp1379
    return retv1377
}

func _goml_m_std_p_json_p_as__bool(value__79 _goml_m_std_p_json_p_Value) Option__bool {
    var retv1382 Option__bool
    var jp1384 Option__bool
    switch value__79.(type) {
    case Bool:
        var x161 bool = value__79.(Bool)._0
        var result__80 bool = x161
        var t1385 Option__bool = Option__bool_Some{
            _0: result__80,
        }
        jp1384 = t1385
    default:
        jp1384 = Option__bool_None{}
    }
    retv1382 = jp1384
    return retv1382
}

func main0() struct{} {
    var mtmp152 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1396 _goml_m_std_p_json_p_Value
    switch mtmp152.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x153 _goml_m_std_p_json_p_Value = mtmp152.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__0 _goml_m_std_p_json_p_Value = x153
        jp1396 = value__0
        var value__2 _goml_m_std_p_json_p_Value = jp1396
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
        var t1400 string = _goml_m_std_p_json_p_encode(value__2)
        println__T_string(t1400)
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
    var retv1415 *_goml_vec_uint8
    var t1416 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv1415 = t1416
    return retv1415
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
    var retv1453 bool
    var t1454 bool = self__59 == other__60
    retv1453 = t1454
    return retv1453
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv1456 int
    var t1457 int = _goml_runtime_core_string_len(self__9)
    retv1456 = t1457
    return retv1456
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv1459 uint8
    var t1460 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv1459 = t1460
    return retv1459
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv1462 string
    var t1463 string = _goml_runtime_core_char_to_string(self__7)
    retv1462 = t1463
    return retv1462
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv1465 bool
    var t1466 bool = self__69 == other__70
    retv1465 = t1466
    return retv1465
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv1471 string
    var t1472 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv1471 = t1472
    return retv1471
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv1474 bool
    var t1475 bool = self__55 == other__56
    retv1474 = t1475
    return retv1474
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
