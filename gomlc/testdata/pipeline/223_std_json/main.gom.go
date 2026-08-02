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
    var vec_literal__178 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var t238 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t238
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    var t252 *_goml_vec_uint8 = self__3.values
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__uint8(t252, length__5)
    var for_index1 int = 0
    Loop_loop254:
    for {
        var t255 bool = for_index1 < length__5
        if t255 {
            var for_item3 int = for_index1
            var t256 int = for_index1 + 1
            for_index1 = t256
            var t257 *_goml_vec_uint8 = self__3.values
            var t258 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__4, for_item3)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(t257, t258)
            continue
        } else {
            break Loop_loop254
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t261 string = _goml_m_inherent_i_char_i_char_i_to__string(value__8)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t261)
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(self__16 _goml_m_std_p_text_p_StringBuilder) string {
    var t276 *_goml_vec_uint8 = self__16.values
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t276)
    var x12 string = mtmp10._1
    return x12
}

func _goml_m_std_p_json_p_json__parser(input__0 string) _goml_m_std_p_json_p_JsonParser {
    var t507 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t508 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__0,
        index: t507,
    }
    return t508
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var t511 string = "" + message__2
    var t512 string = t511 + " at byte "
    var t513 *ref_int_x = value__1.index
    var t514 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t513)
    var t515 string = _goml_m_inherent_i_int_i_int_i_to__string(t514)
    var t516 string = t512 + t515
    return t516
}

func _goml_m_std_p_json_p_json__whitespace(value__3 uint8) bool {
    var t527 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 9)
    var jp525 bool
    if t527 {
        jp525 = true
    } else {
        var t528 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 10)
        jp525 = t528
    }
    var jp522 bool
    if jp525 {
        jp522 = true
    } else {
        var t526 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 13)
        jp522 = t526
    }
    if jp522 {
        return true
    } else {
        var t523 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 32)
        return t523
    }
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop531:
    for {
        var t539 *ref_int_x = value__4.index
        var t540 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t539)
        var t541 string = value__4.input
        var t542 int = _goml_m_inherent_i_string_i_string_i_byte__len(t541)
        var t543 bool = t540 < t542
        var jp533 bool
        if t543 {
            var t544 string = value__4.input
            var t545 *ref_int_x = value__4.index
            var t546 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t545)
            var t547 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t544, t546)
            var t548 bool = _goml_m_std_p_json_p_json__whitespace(t547)
            jp533 = t548
        } else {
            jp533 = false
        }
        if jp533 {
            var t534 *ref_int_x = value__4.index
            var t535 *ref_int_x = value__4.index
            var t536 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t535)
            var t537 int = t536 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t534, t537)
            continue
        } else {
            break Loop_loop531
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var t578 bool = value__5 >= 48
    var jp554 bool
    if t578 {
        var t579 bool = value__5 <= 57
        jp554 = t579
    } else {
        jp554 = false
    }
    if jp554 {
        var t555 uint8 = value__5 - 48
        var t556 uint32 = uint32(uint8(t555))
        var t557 Option__uint32 = Option__uint32_Some{
            _0: t556,
        }
        return t557
    } else {
        var t576 bool = value__5 >= 65
        var jp561 bool
        if t576 {
            var t577 bool = value__5 <= 70
            jp561 = t577
        } else {
            jp561 = false
        }
        if jp561 {
            var t562 uint8 = value__5 - 65
            var t563 uint8 = t562 + 10
            var t564 uint32 = uint32(uint8(t563))
            var t565 Option__uint32 = Option__uint32_Some{
                _0: t564,
            }
            return t565
        } else {
            var t574 bool = value__5 >= 97
            var jp569 bool
            if t574 {
                var t575 bool = value__5 <= 102
                jp569 = t575
            } else {
                jp569 = false
            }
            if jp569 {
                var t570 uint8 = value__5 - 97
                var t571 uint8 = t570 + 10
                var t572 uint32 = uint32(uint8(t571))
                var t573 Option__uint32 = Option__uint32_Some{
                    _0: t572,
                }
                return t573
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t584 *ref_int_x = value__6.index
    var t585 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t584)
    var t586 int = t585 + 4
    var t587 string = value__6.input
    var t588 int = _goml_m_inherent_i_string_i_string_i_byte__len(t587)
    var t589 bool = t586 > t588
    if t589 {
        var t590 string = _goml_m_std_p_json_p_json__error(value__6, "incomplete unicode escape")
        var t591 Result__uint32__string = Result__uint32__string_Err{
            _0: t590,
        }
        return t591
    } else {
        var result__7_source int = 0
        var result__7 uint32 = uint32(int(result__7_source))
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop598:
        for {
            var t599 bool = for_index0 < for_limit1
            if t599 {
                var for_item2 int = for_index0
                var t600 int = for_index0 + 1
                for_index0 = t600
                var t601 string = value__6.input
                var t602 *ref_int_x = value__6.index
                var t603 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t602)
                var t604 int = t603 + for_item2
                var t605 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t601, t604)
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t605)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t607 string = _goml_m_std_p_json_p_json__error(value__6, "invalid unicode escape")
                    var t608 Result__uint32__string = Result__uint32__string_Err{
                        _0: t607,
                    }
                    return t608
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var t609 uint32 = result__7 * 16
                    var t610 uint32 = t609 + x5
                    result__7 = t610
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop598
            }
        }
        var t593 *ref_int_x = value__6.index
        var t594 *ref_int_x = value__6.index
        var t595 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t594)
        var t596 int = t595 + 4
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t593, t596)
        var t597 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__7,
        }
        return t597
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var mtmp9 Option__char = char_from_uint32(codepoint__12)
    switch mtmp9.(type) {
    case Option__char_None:
        var t615 string = _goml_m_std_p_json_p_json__error(value__10, "invalid unicode codepoint")
        var t616 Result__unit__string = Result__unit__string_Err{
            _0: t615,
        }
        return t616
    case Option__char_Some:
        var x10 rune = mtmp9.(Option__char_Some)._0
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__11, x10)
        var t617 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t617
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp12 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp621 uint32
    switch mtmp12.(type) {
    case Result__uint32__string_Ok:
        var x13 uint32 = mtmp12.(Result__uint32__string_Ok)._0
        jp621 = x13
        var t683 bool = jp621 >= 55296
        var jp625 bool
        if t683 {
            var t684 bool = jp621 <= 56319
            jp625 = t684
        } else {
            jp625 = false
        }
        if jp625 {
            var t662 *ref_int_x = value__14.index
            var t663 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t662)
            var t664 int = t663 + 2
            var t665 string = value__14.input
            var t666 int = _goml_m_inherent_i_string_i_string_i_byte__len(t665)
            var t667 bool = t664 > t666
            var jp654 bool
            if t667 {
                jp654 = true
            } else {
                var t668 string = value__14.input
                var t669 *ref_int_x = value__14.index
                var t670 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t669)
                var t671 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t668, t670)
                var t672 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t671, 92)
                var t673 bool = !t672
                jp654 = t673
            }
            var jp629 bool
            if jp654 {
                jp629 = true
            } else {
                var t655 string = value__14.input
                var t656 *ref_int_x = value__14.index
                var t657 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t656)
                var t658 int = t657 + 1
                var t659 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t655, t658)
                var t660 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t659, 117)
                var t661 bool = !t660
                jp629 = t661
            }
            if jp629 {
                var t630 string = _goml_m_std_p_json_p_json__error(value__14, "missing low surrogate")
                var t631 Result__unit__string = Result__unit__string_Err{
                    _0: t630,
                }
                return t631
            } else {
                var t632 *ref_int_x = value__14.index
                var t633 *ref_int_x = value__14.index
                var t634 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t633)
                var t635 int = t634 + 2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t632, t635)
                var mtmp16 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp637 uint32
                switch mtmp16.(type) {
                case Result__uint32__string_Ok:
                    var x17 uint32 = mtmp16.(Result__uint32__string_Ok)._0
                    jp637 = x17
                    var t650 bool = jp637 < 56320
                    var jp641 bool
                    if t650 {
                        jp641 = true
                    } else {
                        var t651 bool = jp637 > 57343
                        jp641 = t651
                    }
                    if jp641 {
                        var t642 string = _goml_m_std_p_json_p_json__error(value__14, "invalid low surrogate")
                        var t643 Result__unit__string = Result__unit__string_Err{
                            _0: t642,
                        }
                        return t643
                    } else {
                        var t644 uint32 = jp621 - 55296
                        var t645 uint32 = t644 * 1024
                        var t646 uint32 = 65536 + t645
                        var t647 uint32 = t646 + jp637
                        var t648 uint32 = t647 - 56320
                        var t649 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, t648)
                        return t649
                    }
                case Result__uint32__string_Err:
                    var x18 string = mtmp16.(Result__uint32__string_Err)._0
                    var t652 Result__unit__string = Result__unit__string_Err{
                        _0: x18,
                    }
                    return t652
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t681 bool = jp621 >= 56320
            var jp677 bool
            if t681 {
                var t682 bool = jp621 <= 57343
                jp677 = t682
            } else {
                jp677 = false
            }
            if jp677 {
                var t678 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t679 Result__unit__string = Result__unit__string_Err{
                    _0: t678,
                }
                return t679
            } else {
                var t680 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, jp621)
                return t680
            }
        }
    case Result__uint32__string_Err:
        var x14 string = mtmp12.(Result__uint32__string_Err)._0
        var t685 Result__unit__string = Result__unit__string_Err{
            _0: x14,
        }
        return t685
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__18 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t801 *ref_int_x = value__18.index
    var t802 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t801)
    var t803 string = value__18.input
    var t804 int = _goml_m_inherent_i_string_i_string_i_byte__len(t803)
    var t805 bool = t802 >= t804
    var jp793 bool
    if t805 {
        jp793 = true
    } else {
        var t806 string = value__18.input
        var t807 *ref_int_x = value__18.index
        var t808 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t807)
        var t809 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t806, t808)
        var t810 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t809, 34)
        var t811 bool = !t810
        jp793 = t811
    }
    if jp793 {
        var t794 string = _goml_m_std_p_json_p_json__error(value__18, "expected string")
        var t795 Result__string__string = Result__string__string_Err{
            _0: t794,
        }
        return t795
    } else {
        var t796 *ref_int_x = value__18.index
        var t797 *ref_int_x = value__18.index
        var t798 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t797)
        var t799 int = t798 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t796, t799)
        var builder__19 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t689 *ref_int_x = value__18.index
        var segment__20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t689)
        Loop_loop693:
        for {
            var t694 *ref_int_x = value__18.index
            var t695 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t694)
            var t696 string = value__18.input
            var t697 int = _goml_m_inherent_i_string_i_string_i_byte__len(t696)
            var t698 bool = t695 < t697
            if t698 {
                var t699 string = value__18.input
                var t700 *ref_int_x = value__18.index
                var t701 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t700)
                var byte__21 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t699, t701)
                var t703 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 34)
                if t703 {
                    var t711 *ref_int_x = value__18.index
                    var t712 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t711)
                    var t713 bool = segment__20 < t712
                    if t713 {
                        var t714 string = value__18.input
                        var t715 *ref_int_x = value__18.index
                        var t716 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t715)
                        var t717 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t714, segment__20, t716)
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t717)
                    } else {}
                    var t705 *ref_int_x = value__18.index
                    var t706 *ref_int_x = value__18.index
                    var t707 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t706)
                    var t708 int = t707 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t705, t708)
                    var t709 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__19)
                    var t710 Result__string__string = Result__string__string_Ok{
                        _0: t709,
                    }
                    return t710
                } else {
                    var t720 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 92)
                    if t720 {
                        var t775 *ref_int_x = value__18.index
                        var t776 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t775)
                        var t777 bool = segment__20 < t776
                        if t777 {
                            var t778 string = value__18.input
                            var t779 *ref_int_x = value__18.index
                            var t780 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t779)
                            var t781 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t778, segment__20, t780)
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t781)
                        } else {}
                        var t722 *ref_int_x = value__18.index
                        var t723 *ref_int_x = value__18.index
                        var t724 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t723)
                        var t725 int = t724 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t722, t725)
                        var t768 *ref_int_x = value__18.index
                        var t769 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t768)
                        var t770 string = value__18.input
                        var t771 int = _goml_m_inherent_i_string_i_string_i_byte__len(t770)
                        var t772 bool = t769 >= t771
                        if t772 {
                            var t773 string = _goml_m_std_p_json_p_json__error(value__18, "incomplete escape")
                            var t774 Result__string__string = Result__string__string_Err{
                                _0: t773,
                            }
                            return t774
                        } else {
                            var t727 string = value__18.input
                            var t728 *ref_int_x = value__18.index
                            var t729 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t728)
                            var escape__22 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t727, t729)
                            var t730 *ref_int_x = value__18.index
                            var t731 *ref_int_x = value__18.index
                            var t732 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t731)
                            var t733 int = t732 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t730, t733)
                            var t737 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 34)
                            if t737 {
                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 34)
                                var t735 *ref_int_x = value__18.index
                                var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                segment__20 = t736
                                continue
                            } else {
                                var t740 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 92)
                                if t740 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 92)
                                    var t735 *ref_int_x = value__18.index
                                    var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                    segment__20 = t736
                                    continue
                                } else {
                                    var t743 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 47)
                                    if t743 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 47)
                                        var t735 *ref_int_x = value__18.index
                                        var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                        segment__20 = t736
                                        continue
                                    } else {
                                        var t746 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 98)
                                        if t746 {
                                            var mtmp26 Option__char = char_from_uint32(8)
                                            switch mtmp26.(type) {
                                            case Option__char_None:
                                                var t735 *ref_int_x = value__18.index
                                                var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                segment__20 = t736
                                                continue
                                            case Option__char_Some:
                                                var x27 rune = mtmp26.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x27)
                                                var t735 *ref_int_x = value__18.index
                                                var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                segment__20 = t736
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t750 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 102)
                                            if t750 {
                                                var mtmp28 Option__char = char_from_uint32(12)
                                                switch mtmp28.(type) {
                                                case Option__char_None:
                                                    var t735 *ref_int_x = value__18.index
                                                    var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                    segment__20 = t736
                                                    continue
                                                case Option__char_Some:
                                                    var x29 rune = mtmp28.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x29)
                                                    var t735 *ref_int_x = value__18.index
                                                    var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                    segment__20 = t736
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t754 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 110)
                                                if t754 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 10)
                                                    var t735 *ref_int_x = value__18.index
                                                    var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                    segment__20 = t736
                                                    continue
                                                } else {
                                                    var t757 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 114)
                                                    if t757 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 13)
                                                        var t735 *ref_int_x = value__18.index
                                                        var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                        segment__20 = t736
                                                        continue
                                                    } else {
                                                        var t760 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 116)
                                                        if t760 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 9)
                                                            var t735 *ref_int_x = value__18.index
                                                            var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                            segment__20 = t736
                                                            continue
                                                        } else {
                                                            var t763 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 117)
                                                            if t763 {
                                                                var mtmp30 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__18, builder__19)
                                                                switch mtmp30.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t735 *ref_int_x = value__18.index
                                                                    var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                                    segment__20 = t736
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x32 string = mtmp30.(Result__unit__string_Err)._0
                                                                    var t765 Result__string__string = Result__string__string_Err{
                                                                        _0: x32,
                                                                    }
                                                                    return t765
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t766 string = _goml_m_std_p_json_p_json__error(value__18, "invalid escape")
                                                                var t767 Result__string__string = Result__string__string_Err{
                                                                    _0: t766,
                                                                }
                                                                return t767
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
                        var t784 bool = byte__21 < 32
                        if t784 {
                            var t785 string = _goml_m_std_p_json_p_json__error(value__18, "unescaped control character")
                            var t786 Result__string__string = Result__string__string_Err{
                                _0: t785,
                            }
                            return t786
                        } else {
                            var t787 *ref_int_x = value__18.index
                            var t788 *ref_int_x = value__18.index
                            var t789 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t788)
                            var t790 int = t789 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t787, t790)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop693
            }
        }
        var t691 string = _goml_m_std_p_json_p_json__error(value__18, "unterminated string")
        var t692 Result__string__string = Result__string__string_Err{
            _0: t691,
        }
        return t692
    }
}

func _goml_m_std_p_json_p_json__digit(value__25 uint8) bool {
    var t816 bool = value__25 >= 48
    if t816 {
        var t817 bool = value__25 <= 57
        return t817
    } else {
        return false
    }
}

func _goml_m_std_p_json_p_parse__digits(value__26 _goml_m_std_p_json_p_JsonParser) bool {
    var t820 *ref_int_x = value__26.index
    var start__27 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t820)
    Loop_loop825:
    for {
        var t833 *ref_int_x = value__26.index
        var t834 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t833)
        var t835 string = value__26.input
        var t836 int = _goml_m_inherent_i_string_i_string_i_byte__len(t835)
        var t837 bool = t834 < t836
        var jp827 bool
        if t837 {
            var t838 string = value__26.input
            var t839 *ref_int_x = value__26.index
            var t840 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t839)
            var t841 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t838, t840)
            var t842 bool = _goml_m_std_p_json_p_json__digit(t841)
            jp827 = t842
        } else {
            jp827 = false
        }
        if jp827 {
            var t828 *ref_int_x = value__26.index
            var t829 *ref_int_x = value__26.index
            var t830 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t829)
            var t831 int = t830 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t828, t831)
            continue
        } else {
            break Loop_loop825
        }
    }
    var t822 *ref_int_x = value__26.index
    var t823 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t822)
    var t824 bool = t823 > start__27
    return t824
}

func _goml_m_std_p_json_p_parse__json__number(value__28 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t845 *ref_int_x = value__28.index
    var start__29 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t845)
    var t967 string = value__28.input
    var t968 *ref_int_x = value__28.index
    var t969 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t968)
    var t970 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t967, t969)
    var t971 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t970, 45)
    if t971 {
        var t972 *ref_int_x = value__28.index
        var t973 *ref_int_x = value__28.index
        var t974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t973)
        var t975 int = t974 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t972, t975)
    } else {}
    var t930 *ref_int_x = value__28.index
    var t931 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t930)
    var t932 string = value__28.input
    var t933 int = _goml_m_inherent_i_string_i_string_i_byte__len(t932)
    var t934 bool = t931 >= t933
    if t934 {
        var t935 string = _goml_m_std_p_json_p_json__error(value__28, "incomplete number")
        var t936 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t935,
        }
        return t936
    } else {
        var t938 string = value__28.input
        var t939 *ref_int_x = value__28.index
        var t940 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t939)
        var t941 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t938, t940)
        var t942 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t941, 48)
        if t942 {
            var t943 *ref_int_x = value__28.index
            var t944 *ref_int_x = value__28.index
            var t945 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t944)
            var t946 int = t945 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t943, t946)
            var t952 *ref_int_x = value__28.index
            var t953 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t952)
            var t954 string = value__28.input
            var t955 int = _goml_m_inherent_i_string_i_string_i_byte__len(t954)
            var t956 bool = t953 < t955
            var jp949 bool
            if t956 {
                var t957 string = value__28.input
                var t958 *ref_int_x = value__28.index
                var t959 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t958)
                var t960 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t957, t959)
                var t961 bool = _goml_m_std_p_json_p_json__digit(t960)
                jp949 = t961
            } else {
                jp949 = false
            }
            if jp949 {
                var t950 string = _goml_m_std_p_json_p_json__error(value__28, "invalid leading zero")
                var t951 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t950,
                }
                return t951
            } else {
                var t920 *ref_int_x = value__28.index
                var t921 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t920)
                var t922 string = value__28.input
                var t923 int = _goml_m_inherent_i_string_i_string_i_byte__len(t922)
                var t924 bool = t921 < t923
                var jp910 bool
                if t924 {
                    var t925 string = value__28.input
                    var t926 *ref_int_x = value__28.index
                    var t927 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t926)
                    var t928 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t925, t927)
                    var t929 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t928, 46)
                    jp910 = t929
                } else {
                    jp910 = false
                }
                if jp910 {
                    var t911 *ref_int_x = value__28.index
                    var t912 *ref_int_x = value__28.index
                    var t913 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t912)
                    var t914 int = t913 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t911, t914)
                    var t916 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t917 bool = !t916
                    if t917 {
                        var t918 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t919 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t918,
                        }
                        return t919
                    } else {
                        var t892 *ref_int_x = value__28.index
                        var t893 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t892)
                        var t894 string = value__28.input
                        var t895 int = _goml_m_inherent_i_string_i_string_i_byte__len(t894)
                        var t896 bool = t893 < t895
                        var jp857 bool
                        if t896 {
                            var t899 string = value__28.input
                            var t900 *ref_int_x = value__28.index
                            var t901 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t900)
                            var t902 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t899, t901)
                            var t903 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t902, 101)
                            if t903 {
                                jp857 = true
                            } else {
                                var t904 string = value__28.input
                                var t905 *ref_int_x = value__28.index
                                var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                                var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                                var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 69)
                                jp857 = t908
                            }
                        } else {
                            jp857 = false
                        }
                        if jp857 {
                            var t858 *ref_int_x = value__28.index
                            var t859 *ref_int_x = value__28.index
                            var t860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t859)
                            var t861 int = t860 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t858, t861)
                            var t875 *ref_int_x = value__28.index
                            var t876 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t875)
                            var t877 string = value__28.input
                            var t878 int = _goml_m_inherent_i_string_i_string_i_byte__len(t877)
                            var t879 bool = t876 < t878
                            var jp869 bool
                            if t879 {
                                var t882 string = value__28.input
                                var t883 *ref_int_x = value__28.index
                                var t884 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t883)
                                var t885 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t882, t884)
                                var t886 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t885, 43)
                                if t886 {
                                    jp869 = true
                                } else {
                                    var t887 string = value__28.input
                                    var t888 *ref_int_x = value__28.index
                                    var t889 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t888)
                                    var t890 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t887, t889)
                                    var t891 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t890, 45)
                                    jp869 = t891
                                }
                            } else {
                                jp869 = false
                            }
                            if jp869 {
                                var t870 *ref_int_x = value__28.index
                                var t871 *ref_int_x = value__28.index
                                var t872 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t871)
                                var t873 int = t872 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t870, t873)
                            } else {}
                            var t864 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t865 bool = !t864
                            if t865 {
                                var t866 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t867 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t866,
                                }
                                return t867
                            } else {
                                var t850 string = value__28.input
                                var t851 *ref_int_x = value__28.index
                                var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                                var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                                var t854 _goml_m_std_p_json_p_Value = Number{
                                    _0: t853,
                                }
                                var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t854,
                                }
                                return t855
                            }
                        } else {
                            var t850 string = value__28.input
                            var t851 *ref_int_x = value__28.index
                            var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                            var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                            var t854 _goml_m_std_p_json_p_Value = Number{
                                _0: t853,
                            }
                            var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t854,
                            }
                            return t855
                        }
                    }
                } else {
                    var t892 *ref_int_x = value__28.index
                    var t893 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t892)
                    var t894 string = value__28.input
                    var t895 int = _goml_m_inherent_i_string_i_string_i_byte__len(t894)
                    var t896 bool = t893 < t895
                    var jp857 bool
                    if t896 {
                        var t899 string = value__28.input
                        var t900 *ref_int_x = value__28.index
                        var t901 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t900)
                        var t902 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t899, t901)
                        var t903 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t902, 101)
                        if t903 {
                            jp857 = true
                        } else {
                            var t904 string = value__28.input
                            var t905 *ref_int_x = value__28.index
                            var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                            var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                            var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 69)
                            jp857 = t908
                        }
                    } else {
                        jp857 = false
                    }
                    if jp857 {
                        var t858 *ref_int_x = value__28.index
                        var t859 *ref_int_x = value__28.index
                        var t860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t859)
                        var t861 int = t860 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t858, t861)
                        var t875 *ref_int_x = value__28.index
                        var t876 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t875)
                        var t877 string = value__28.input
                        var t878 int = _goml_m_inherent_i_string_i_string_i_byte__len(t877)
                        var t879 bool = t876 < t878
                        var jp869 bool
                        if t879 {
                            var t882 string = value__28.input
                            var t883 *ref_int_x = value__28.index
                            var t884 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t883)
                            var t885 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t882, t884)
                            var t886 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t885, 43)
                            if t886 {
                                jp869 = true
                            } else {
                                var t887 string = value__28.input
                                var t888 *ref_int_x = value__28.index
                                var t889 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t888)
                                var t890 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t887, t889)
                                var t891 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t890, 45)
                                jp869 = t891
                            }
                        } else {
                            jp869 = false
                        }
                        if jp869 {
                            var t870 *ref_int_x = value__28.index
                            var t871 *ref_int_x = value__28.index
                            var t872 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t871)
                            var t873 int = t872 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t870, t873)
                        } else {}
                        var t864 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t865 bool = !t864
                        if t865 {
                            var t866 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t867 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t866,
                            }
                            return t867
                        } else {
                            var t850 string = value__28.input
                            var t851 *ref_int_x = value__28.index
                            var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                            var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                            var t854 _goml_m_std_p_json_p_Value = Number{
                                _0: t853,
                            }
                            var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t854,
                            }
                            return t855
                        }
                    } else {
                        var t850 string = value__28.input
                        var t851 *ref_int_x = value__28.index
                        var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                        var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                        var t854 _goml_m_std_p_json_p_Value = Number{
                            _0: t853,
                        }
                        var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t854,
                        }
                        return t855
                    }
                }
            }
        } else {
            var t963 bool = _goml_m_std_p_json_p_parse__digits(value__28)
            var t964 bool = !t963
            if t964 {
                var t965 string = _goml_m_std_p_json_p_json__error(value__28, "expected number")
                var t966 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t965,
                }
                return t966
            } else {
                var t920 *ref_int_x = value__28.index
                var t921 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t920)
                var t922 string = value__28.input
                var t923 int = _goml_m_inherent_i_string_i_string_i_byte__len(t922)
                var t924 bool = t921 < t923
                var jp910 bool
                if t924 {
                    var t925 string = value__28.input
                    var t926 *ref_int_x = value__28.index
                    var t927 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t926)
                    var t928 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t925, t927)
                    var t929 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t928, 46)
                    jp910 = t929
                } else {
                    jp910 = false
                }
                if jp910 {
                    var t911 *ref_int_x = value__28.index
                    var t912 *ref_int_x = value__28.index
                    var t913 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t912)
                    var t914 int = t913 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t911, t914)
                    var t916 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t917 bool = !t916
                    if t917 {
                        var t918 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t919 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t918,
                        }
                        return t919
                    } else {
                        var t892 *ref_int_x = value__28.index
                        var t893 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t892)
                        var t894 string = value__28.input
                        var t895 int = _goml_m_inherent_i_string_i_string_i_byte__len(t894)
                        var t896 bool = t893 < t895
                        var jp857 bool
                        if t896 {
                            var t899 string = value__28.input
                            var t900 *ref_int_x = value__28.index
                            var t901 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t900)
                            var t902 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t899, t901)
                            var t903 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t902, 101)
                            if t903 {
                                jp857 = true
                            } else {
                                var t904 string = value__28.input
                                var t905 *ref_int_x = value__28.index
                                var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                                var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                                var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 69)
                                jp857 = t908
                            }
                        } else {
                            jp857 = false
                        }
                        if jp857 {
                            var t858 *ref_int_x = value__28.index
                            var t859 *ref_int_x = value__28.index
                            var t860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t859)
                            var t861 int = t860 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t858, t861)
                            var t875 *ref_int_x = value__28.index
                            var t876 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t875)
                            var t877 string = value__28.input
                            var t878 int = _goml_m_inherent_i_string_i_string_i_byte__len(t877)
                            var t879 bool = t876 < t878
                            var jp869 bool
                            if t879 {
                                var t882 string = value__28.input
                                var t883 *ref_int_x = value__28.index
                                var t884 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t883)
                                var t885 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t882, t884)
                                var t886 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t885, 43)
                                if t886 {
                                    jp869 = true
                                } else {
                                    var t887 string = value__28.input
                                    var t888 *ref_int_x = value__28.index
                                    var t889 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t888)
                                    var t890 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t887, t889)
                                    var t891 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t890, 45)
                                    jp869 = t891
                                }
                            } else {
                                jp869 = false
                            }
                            if jp869 {
                                var t870 *ref_int_x = value__28.index
                                var t871 *ref_int_x = value__28.index
                                var t872 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t871)
                                var t873 int = t872 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t870, t873)
                            } else {}
                            var t864 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t865 bool = !t864
                            if t865 {
                                var t866 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t867 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t866,
                                }
                                return t867
                            } else {
                                var t850 string = value__28.input
                                var t851 *ref_int_x = value__28.index
                                var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                                var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                                var t854 _goml_m_std_p_json_p_Value = Number{
                                    _0: t853,
                                }
                                var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t854,
                                }
                                return t855
                            }
                        } else {
                            var t850 string = value__28.input
                            var t851 *ref_int_x = value__28.index
                            var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                            var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                            var t854 _goml_m_std_p_json_p_Value = Number{
                                _0: t853,
                            }
                            var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t854,
                            }
                            return t855
                        }
                    }
                } else {
                    var t892 *ref_int_x = value__28.index
                    var t893 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t892)
                    var t894 string = value__28.input
                    var t895 int = _goml_m_inherent_i_string_i_string_i_byte__len(t894)
                    var t896 bool = t893 < t895
                    var jp857 bool
                    if t896 {
                        var t899 string = value__28.input
                        var t900 *ref_int_x = value__28.index
                        var t901 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t900)
                        var t902 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t899, t901)
                        var t903 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t902, 101)
                        if t903 {
                            jp857 = true
                        } else {
                            var t904 string = value__28.input
                            var t905 *ref_int_x = value__28.index
                            var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                            var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                            var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 69)
                            jp857 = t908
                        }
                    } else {
                        jp857 = false
                    }
                    if jp857 {
                        var t858 *ref_int_x = value__28.index
                        var t859 *ref_int_x = value__28.index
                        var t860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t859)
                        var t861 int = t860 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t858, t861)
                        var t875 *ref_int_x = value__28.index
                        var t876 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t875)
                        var t877 string = value__28.input
                        var t878 int = _goml_m_inherent_i_string_i_string_i_byte__len(t877)
                        var t879 bool = t876 < t878
                        var jp869 bool
                        if t879 {
                            var t882 string = value__28.input
                            var t883 *ref_int_x = value__28.index
                            var t884 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t883)
                            var t885 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t882, t884)
                            var t886 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t885, 43)
                            if t886 {
                                jp869 = true
                            } else {
                                var t887 string = value__28.input
                                var t888 *ref_int_x = value__28.index
                                var t889 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t888)
                                var t890 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t887, t889)
                                var t891 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t890, 45)
                                jp869 = t891
                            }
                        } else {
                            jp869 = false
                        }
                        if jp869 {
                            var t870 *ref_int_x = value__28.index
                            var t871 *ref_int_x = value__28.index
                            var t872 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t871)
                            var t873 int = t872 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t870, t873)
                        } else {}
                        var t864 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t865 bool = !t864
                        if t865 {
                            var t866 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t867 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t866,
                            }
                            return t867
                        } else {
                            var t850 string = value__28.input
                            var t851 *ref_int_x = value__28.index
                            var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                            var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                            var t854 _goml_m_std_p_json_p_Value = Number{
                                _0: t853,
                            }
                            var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t854,
                            }
                            return t855
                        }
                    } else {
                        var t850 string = value__28.input
                        var t851 *ref_int_x = value__28.index
                        var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                        var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                        var t854 _goml_m_std_p_json_p_Value = Number{
                            _0: t853,
                        }
                        var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t854,
                        }
                        return t855
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__30 _goml_m_std_p_json_p_JsonParser, expected__31 string, result__32 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t991 *ref_int_x = value__30.index
    var t992 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t991)
    var t993 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__31)
    var t994 int = t992 + t993
    var t995 string = value__30.input
    var t996 int = _goml_m_inherent_i_string_i_string_i_byte__len(t995)
    var t997 bool = t994 <= t996
    var jp982 bool
    if t997 {
        var t998 string = value__30.input
        var t999 *ref_int_x = value__30.index
        var t1000 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t999)
        var t1001 *ref_int_x = value__30.index
        var t1002 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1001)
        var t1003 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__31)
        var t1004 int = t1002 + t1003
        var t1005 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t998, t1000, t1004)
        var t1006 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1005, expected__31)
        jp982 = t1006
    } else {
        jp982 = false
    }
    if jp982 {
        var t983 *ref_int_x = value__30.index
        var t984 *ref_int_x = value__30.index
        var t985 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t984)
        var t986 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__31)
        var t987 int = t985 + t986
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t983, t987)
        var t988 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__32,
        }
        return t988
    } else {
        var t989 string = _goml_m_std_p_json_p_json__error(value__30, "invalid literal")
        var t990 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t989,
        }
        return t990
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1009 *ref_int_x = value__33.index
    var t1010 *ref_int_x = value__33.index
    var t1011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1010)
    var t1012 int = t1011 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1009, t1012)
    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
    var vec_literal__8961 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t1067 *ref_int_x = value__33.index
    var t1068 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1067)
    var t1069 string = value__33.input
    var t1070 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1069)
    var t1071 bool = t1068 < t1070
    var jp1060 bool
    if t1071 {
        var t1072 string = value__33.input
        var t1073 *ref_int_x = value__33.index
        var t1074 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1073)
        var t1075 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1072, t1074)
        var t1076 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1075, 93)
        jp1060 = t1076
    } else {
        jp1060 = false
    }
    if jp1060 {
        var t1061 *ref_int_x = value__33.index
        var t1062 *ref_int_x = value__33.index
        var t1063 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1062)
        var t1064 int = t1063 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1061, t1064)
        var t1065 _goml_m_std_p_json_p_Value = Array{
            _0: vec_literal__8961,
        }
        var t1066 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1065,
        }
        return t1066
    } else {
        Loop_loop1017:
        for {
            var t1018 *ref_int_x = value__33.index
            var t1019 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1018)
            var t1020 string = value__33.input
            var t1021 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1020)
            var t1022 bool = t1019 < t1021
            if t1022 {
                var mtmp50 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__33)
                var jp1024 _goml_m_std_p_json_p_Value
                switch mtmp50.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x51 _goml_m_std_p_json_p_Value = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp1024 = x51
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8961, jp1024)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                    var t1026 *ref_int_x = value__33.index
                    var t1027 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1026)
                    var t1028 string = value__33.input
                    var t1029 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1028)
                    var t1030 bool = t1027 >= t1029
                    if t1030 {
                        var t1031 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
                        var t1032 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1031,
                        }
                        return t1032
                    } else {
                        var t1034 string = value__33.input
                        var t1035 *ref_int_x = value__33.index
                        var t1036 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1035)
                        var t1037 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1034, t1036)
                        var t1038 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1037, 93)
                        if t1038 {
                            var t1039 *ref_int_x = value__33.index
                            var t1040 *ref_int_x = value__33.index
                            var t1041 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1040)
                            var t1042 int = t1041 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1039, t1042)
                            var t1043 _goml_m_std_p_json_p_Value = Array{
                                _0: vec_literal__8961,
                            }
                            var t1044 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1043,
                            }
                            return t1044
                        } else {
                            var t1046 string = value__33.input
                            var t1047 *ref_int_x = value__33.index
                            var t1048 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1047)
                            var t1049 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1046, t1048)
                            var t1050 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1049, 44)
                            if t1050 {
                                var t1051 *ref_int_x = value__33.index
                                var t1052 *ref_int_x = value__33.index
                                var t1053 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1052)
                                var t1054 int = t1053 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1051, t1054)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                                continue
                            } else {
                                var t1056 string = _goml_m_std_p_json_p_json__error(value__33, "expected array separator")
                                var t1057 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1056,
                                }
                                return t1057
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x52 string = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t1058 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x52,
                    }
                    return t1058
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1017
            }
        }
        var t1015 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
        var t1016 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1015,
        }
        return t1016
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__36 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1079 *ref_int_x = value__36.index
    var t1080 *ref_int_x = value__36.index
    var t1081 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1080)
    var t1082 int = t1081 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1079, t1082)
    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
    var vec_literal__10180 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t1162 *ref_int_x = value__36.index
    var t1163 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1162)
    var t1164 string = value__36.input
    var t1165 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1164)
    var t1166 bool = t1163 < t1165
    var jp1155 bool
    if t1166 {
        var t1167 string = value__36.input
        var t1168 *ref_int_x = value__36.index
        var t1169 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1168)
        var t1170 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1167, t1169)
        var t1171 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1170, 125)
        jp1155 = t1171
    } else {
        jp1155 = false
    }
    if jp1155 {
        var t1156 *ref_int_x = value__36.index
        var t1157 *ref_int_x = value__36.index
        var t1158 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1157)
        var t1159 int = t1158 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1156, t1159)
        var t1160 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10180,
        }
        var t1161 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1160,
        }
        return t1161
    } else {
        Loop_loop1087:
        for {
            var t1088 *ref_int_x = value__36.index
            var t1089 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1088)
            var t1090 string = value__36.input
            var t1091 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1090)
            var t1092 bool = t1089 < t1091
            if t1092 {
                var mtmp62 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__36)
                var jp1094 string
                switch mtmp62.(type) {
                case Result__string__string_Ok:
                    var x63 string = mtmp62.(Result__string__string_Ok)._0
                    jp1094 = x63
                    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                    var t1142 *ref_int_x = value__36.index
                    var t1143 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1142)
                    var t1144 string = value__36.input
                    var t1145 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1144)
                    var t1146 bool = t1143 >= t1145
                    var jp1134 bool
                    if t1146 {
                        jp1134 = true
                    } else {
                        var t1147 string = value__36.input
                        var t1148 *ref_int_x = value__36.index
                        var t1149 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1148)
                        var t1150 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1147, t1149)
                        var t1151 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1150, 58)
                        var t1152 bool = !t1151
                        jp1134 = t1152
                    }
                    if jp1134 {
                        var t1135 string = _goml_m_std_p_json_p_json__error(value__36, "expected object colon")
                        var t1136 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1135,
                        }
                        return t1136
                    } else {
                        var t1137 *ref_int_x = value__36.index
                        var t1138 *ref_int_x = value__36.index
                        var t1139 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1138)
                        var t1140 int = t1139 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1137, t1140)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                        var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__36)
                        var jp1097 _goml_m_std_p_json_p_Value
                        switch mtmp68.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp1097 = x69
                            var t1098 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp1094,
                                _1: jp1097,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10180, t1098)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                            var t1100 *ref_int_x = value__36.index
                            var t1101 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1100)
                            var t1102 string = value__36.input
                            var t1103 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1102)
                            var t1104 bool = t1101 >= t1103
                            if t1104 {
                                var t1105 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
                                var t1106 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1105,
                                }
                                return t1106
                            } else {
                                var t1108 string = value__36.input
                                var t1109 *ref_int_x = value__36.index
                                var t1110 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1109)
                                var t1111 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1108, t1110)
                                var t1112 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1111, 125)
                                if t1112 {
                                    var t1113 *ref_int_x = value__36.index
                                    var t1114 *ref_int_x = value__36.index
                                    var t1115 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1114)
                                    var t1116 int = t1115 + 1
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1113, t1116)
                                    var t1117 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10180,
                                    }
                                    var t1118 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1117,
                                    }
                                    return t1118
                                } else {
                                    var t1120 string = value__36.input
                                    var t1121 *ref_int_x = value__36.index
                                    var t1122 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1121)
                                    var t1123 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1120, t1122)
                                    var t1124 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1123, 44)
                                    if t1124 {
                                        var t1125 *ref_int_x = value__36.index
                                        var t1126 *ref_int_x = value__36.index
                                        var t1127 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1126)
                                        var t1128 int = t1127 + 1
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1125, t1128)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                                        continue
                                    } else {
                                        var t1130 string = _goml_m_std_p_json_p_json__error(value__36, "expected object separator")
                                        var t1131 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1130,
                                        }
                                        return t1131
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t1132 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x70,
                            }
                            return t1132
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x64 string = mtmp62.(Result__string__string_Err)._0
                    var t1153 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x64,
                    }
                    return t1153
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1087
            }
        }
        var t1085 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
        var t1086 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1085,
        }
        return t1086
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__40 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__40)
    var t1176 *ref_int_x = value__40.index
    var t1177 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1176)
    var t1178 string = value__40.input
    var t1179 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1178)
    var t1180 bool = t1177 >= t1179
    if t1180 {
        var t1181 string = _goml_m_std_p_json_p_json__error(value__40, "expected JSON value")
        var t1182 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1181,
        }
        return t1182
    } else {
        var t1183 string = value__40.input
        var t1184 *ref_int_x = value__40.index
        var t1185 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1184)
        var mtmp77 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1183, t1185)
        switch mtmp77 {
        case 123:
            var t1188 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__40)
            return t1188
        case 91:
            var t1189 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__40)
            return t1189
        case 34:
            var mtmp78 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__40)
            switch mtmp78.(type) {
            case Result__string__string_Ok:
                var x79 string = mtmp78.(Result__string__string_Ok)._0
                var t1192 _goml_m_std_p_json_p_Value = String{
                    _0: x79,
                }
                var t1193 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1192,
                }
                return t1193
            case Result__string__string_Err:
                var x80 string = mtmp78.(Result__string__string_Err)._0
                var t1194 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x80,
                }
                return t1194
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t1195 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t1196 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "true", t1195)
            return t1196
        case 102:
            var t1197 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t1198 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "false", t1197)
            return t1198
        case 110:
            var t1199 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "null", Null{})
            return t1199
        default:
            var t1207 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(mtmp77, 45)
            var jp1203 bool
            if t1207 {
                jp1203 = true
            } else {
                var t1208 bool = _goml_m_std_p_json_p_json__digit(mtmp77)
                jp1203 = t1208
            }
            if jp1203 {
                var t1204 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__40)
                return t1204
            } else {
                var t1205 string = _goml_m_std_p_json_p_json__error(value__40, "unexpected JSON token")
                var t1206 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1205,
                }
                return t1206
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__44 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__45 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_json__parser(input__44)
    var mtmp81 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__45)
    var jp1212 _goml_m_std_p_json_p_Value
    switch mtmp81.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x82 _goml_m_std_p_json_p_Value = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1212 = x82
        _goml_m_std_p_json_p_skip__json__whitespace(parser__45)
        var t1215 *ref_int_x = parser__45.index
        var t1216 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1215)
        var t1217 int = _goml_m_inherent_i_string_i_string_i_byte__len(input__44)
        var t1218 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1216, t1217)
        if t1218 {
            var t1219 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp1212,
            }
            return t1219
        } else {
            var t1220 string = _goml_m_std_p_json_p_json__error(parser__45, "trailing JSON data")
            var t1221 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1220,
            }
            return t1221
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x83 string = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t1222 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x83,
        }
        return t1222
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_json__hex__digit(value__47 uint8) rune {
    var t1225 int = int(uint8(value__47))
    var t1226 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", t1225)
    return t1226
}

func _goml_m_std_p_json_p_write__json__string(builder__48 _goml_m_std_p_text_p_StringBuilder, value__49 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    var start__50 int = 0
    var for_index86 int = 0
    var for_limit87 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    Loop_loop1236:
    for {
        var t1237 bool = for_index86 < for_limit87
        if t1237 {
            var for_item88 int = for_index86
            var t1238 int = for_index86 + 1
            for_index86 = t1238
            var byte__52 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__49, for_item88)
            var t1291 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 34)
            var jp1289 bool
            if t1291 {
                jp1289 = true
            } else {
                var t1292 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 92)
                jp1289 = t1292
            }
            var jp1286 bool
            if jp1289 {
                jp1286 = true
            } else {
                var t1290 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 8)
                jp1286 = t1290
            }
            var jp1283 bool
            if jp1286 {
                jp1283 = true
            } else {
                var t1287 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 9)
                jp1283 = t1287
            }
            var jp1280 bool
            if jp1283 {
                jp1280 = true
            } else {
                var t1284 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 10)
                jp1280 = t1284
            }
            var jp1277 bool
            if jp1280 {
                jp1277 = true
            } else {
                var t1281 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 12)
                jp1277 = t1281
            }
            var jp1274 bool
            if jp1277 {
                jp1274 = true
            } else {
                var t1278 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 13)
                jp1274 = t1278
            }
            var jp1241 bool
            if jp1274 {
                jp1241 = true
            } else {
                var t1275 bool = byte__52 < 32
                jp1241 = t1275
            }
            if jp1241 {
                var t1270 bool = start__50 < for_item88
                if t1270 {
                    var t1271 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__49, start__50, for_item88)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1271)
                } else {}
                var t1245 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 34)
                if t1245 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\"")
                } else {
                    var t1248 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 92)
                    if t1248 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\\")
                    } else {
                        var t1251 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 8)
                        if t1251 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\b")
                        } else {
                            var t1254 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 9)
                            if t1254 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\t")
                            } else {
                                var t1257 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 10)
                                if t1257 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\n")
                                } else {
                                    var t1260 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 12)
                                    if t1260 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\f")
                                    } else {
                                        var t1263 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 13)
                                        if t1263 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\u00")
                                            var t1265 uint8 = byte__52 / 16
                                            var t1266 rune = _goml_m_std_p_json_p_json__hex__digit(t1265)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, t1266)
                                            var t1267_rhs uint8 = 16
                                            var t1267 uint8 = byte__52 % t1267_rhs
                                            var t1268 rune = _goml_m_std_p_json_p_json__hex__digit(t1267)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, t1268)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1244 int = for_item88 + 1
                start__50 = t1244
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop1236
        }
    }
    var t1231 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    var t1232 bool = start__50 < t1231
    if t1232 {
        var t1233 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
        var t1234 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__49, start__50, t1233)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1234)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__53 _goml_m_std_p_text_p_StringBuilder, value__54 _goml_m_std_p_json_p_Value) struct{} {
    switch value__54.(type) {
    case Object:
        var x97 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__54.(Object)._0
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 123)
        var index__56 int = 0
        var for_limit104 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97)
        var for_index105 int = 0
        Loop_loop1297:
        for {
            var t1298 bool = for_index105 < for_limit104
            if t1298 {
                var for_item106 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97, for_index105)
                var t1299 int = for_index105 + 1
                for_index105 = t1299
                var t1305 bool = index__56 > 0
                if t1305 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 44)
                } else {}
                var t1301 string = for_item106._0
                _goml_m_std_p_json_p_write__json__string(builder__53, t1301)
                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 58)
                var t1302 _goml_m_std_p_json_p_Value = for_item106._1
                _goml_m_std_p_json_p_write__json__value(builder__53, t1302)
                var compound_old112 int = index__56
                var compound_value113 int = 1
                var t1303 int = compound_old112 + compound_value113
                index__56 = t1303
                continue
            } else {
                break Loop_loop1297
            }
        }
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 125)
        return struct{}{}
    case Array:
        var x98 *_goml_vec__goml_m_std_p_json_p_Value = value__54.(Array)._0
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 91)
        var index__59 int = 0
        var for_limit118 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x98)
        var for_index119 int = 0
        Loop_loop1309:
        for {
            var t1310 bool = for_index119 < for_limit118
            if t1310 {
                var for_item120 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x98, for_index119)
                var t1311 int = for_index119 + 1
                for_index119 = t1311
                var t1315 bool = index__59 > 0
                if t1315 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 44)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__53, for_item120)
                var compound_old124 int = index__59
                var compound_value125 int = 1
                var t1313 int = compound_old124 + compound_value125
                index__59 = t1313
                continue
            } else {
                break Loop_loop1309
            }
        }
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__53, 93)
        return struct{}{}
    case String:
        var x99 string = value__54.(String)._0
        _goml_m_std_p_json_p_write__json__string(builder__53, x99)
        return struct{}{}
    case Number:
        var x100 string = value__54.(Number)._0
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, x100)
        return struct{}{}
    case Bool:
        var x101 bool = value__54.(Bool)._0
        var jp1320 string
        if x101 {
            jp1320 = "true"
        } else {
            jp1320 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, jp1320)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__64 _goml_m_std_p_json_p_Value) string {
    var builder__65 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
    _goml_m_std_p_json_p_write__json__value(builder__65, value__64)
    var t1325 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__65)
    return t1325
}

func _goml_m_std_p_json_p_field(value__66 _goml_m_std_p_json_p_Value, name__67 string) _goml_m_Option____std_p_json_p_Value {
    switch value__66.(type) {
    case Object:
        var x129 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__66.(Object)._0
        var for_limit135 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129)
        var for_index136 int = 0
        Loop_loop1331:
        for {
            var t1332 bool = for_index136 < for_limit135
            if t1332 {
                var for_item137 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129, for_index136)
                var t1333 int = for_index136 + 1
                for_index136 = t1333
                var t1335 string = for_item137._0
                var t1336 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1335, name__67)
                if t1336 {
                    var t1337 _goml_m_std_p_json_p_Value = for_item137._1
                    var t1338 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1337,
                    }
                    return t1338
                } else {
                    continue
                }
            } else {
                break Loop_loop1331
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_as__string(value__70 _goml_m_std_p_json_p_Value) Option__string {
    switch value__70.(type) {
    case String:
        var x142 string = value__70.(String)._0
        var t1343 Option__string = Option__string_Some{
            _0: x142,
        }
        return t1343
    default:
        return Option__string_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__72 string) Option__int {
    var t1348 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__72)
    var t1349 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1348, 0)
    if t1349 {
        return Option__int_None{}
    } else {
        var t1350 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__72, 0)
        var negative__73 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1350, 45)
        var jp1352 int
        if negative__73 {
            jp1352 = 1
        } else {
            jp1352 = 0
        }
        var index__74 int = jp1352
        var result__75 int = 0
        var t1373 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__72)
        var t1374 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(index__74, t1373)
        if t1374 {
            return Option__int_None{}
        } else {
            Loop_loop1359:
            for {
                var t1360 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__72)
                var t1361 bool = index__74 < t1360
                if t1361 {
                    var byte__76 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__72, index__74)
                    var t1371 bool = byte__76 < 48
                    var jp1366 bool
                    if t1371 {
                        jp1366 = true
                    } else {
                        var t1372 bool = byte__76 > 57
                        jp1366 = t1372
                    }
                    if jp1366 {
                        return Option__int_None{}
                    } else {
                        var t1367 int = result__75 * 10
                        var t1368 uint8 = byte__76 - 48
                        var t1369 int = int(uint8(t1368))
                        var t1370 int = t1367 + t1369
                        result__75 = t1370
                        var compound_old148 int = index__74
                        var compound_value149 int = 1
                        var t1363 int = compound_old148 + compound_value149
                        index__74 = t1363
                        continue
                    }
                } else {
                    break Loop_loop1359
                }
            }
            var jp1356 int
            if negative__73 {
                var t1358 int = 0 - result__75
                jp1356 = t1358
            } else {
                jp1356 = result__75
            }
            var t1357 Option__int = Option__int_Some{
                _0: jp1356,
            }
            return t1357
        }
    }
}

func _goml_m_std_p_json_p_as__int(value__77 _goml_m_std_p_json_p_Value) Option__int {
    switch value__77.(type) {
    case Number:
        var x155 string = value__77.(Number)._0
        var t1379 Option__int = _goml_m_std_p_json_p_parse__json__int__text(x155)
        return t1379
    default:
        return Option__int_None{}
    }
}

func _goml_m_std_p_json_p_as__bool(value__79 _goml_m_std_p_json_p_Value) Option__bool {
    switch value__79.(type) {
    case Bool:
        var x161 bool = value__79.(Bool)._0
        var t1384 Option__bool = Option__bool_Some{
            _0: x161,
        }
        return t1384
    default:
        return Option__bool_None{}
    }
}

func main0() struct{} {
    var mtmp155 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1395 _goml_m_std_p_json_p_Value
    switch mtmp155.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x156 _goml_m_std_p_json_p_Value = mtmp155.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1395 = x156
        var mtmp159 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1395, "name")
        switch mtmp159.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing name")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x160 _goml_m_std_p_json_p_Value = mtmp159.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var mtmp161 Option__string = _goml_m_std_p_json_p_as__string(x160)
            switch mtmp161.(type) {
            case Option__string_None:
                println__T_string("invalid name")
            case Option__string_Some:
                var x162 string = mtmp161.(Option__string_Some)._0
                println__T_string(x162)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var mtmp164 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1395, "version")
        switch mtmp164.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing version")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x165 _goml_m_std_p_json_p_Value = mtmp164.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var mtmp166 Option__int = _goml_m_std_p_json_p_as__int(x165)
            switch mtmp166.(type) {
            case Option__int_None:
                println__T_string("invalid version")
            case Option__int_Some:
                var x167 int = mtmp166.(Option__int_Some)._0
                println__T_int(x167)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var mtmp169 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1395, "stable")
        switch mtmp169.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing stable")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x170 _goml_m_std_p_json_p_Value = mtmp169.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var mtmp171 Option__bool = _goml_m_std_p_json_p_as__bool(x170)
            switch mtmp171.(type) {
            case Option__bool_None:
                println__T_string("invalid stable")
            case Option__bool_Some:
                var x172 bool = mtmp171.(Option__bool_Some)._0
                println__T_bool(x172)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var t1399 string = _goml_m_std_p_json_p_encode(jp1395)
        println__T_string(t1399)
        return struct{}{}
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x157 string = mtmp155.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        println__T_string(x157)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t1415 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t1415
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
    var t1453 int = _goml_runtime_core_string_len(self__9)
    return t1453
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var t1456 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    return t1456
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var t1459 string = _goml_runtime_core_char_to_string(self__7)
    return t1459
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var t1462 bool = self__69 == other__70
    return t1462
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var t1465 bool = self__59 == other__60
    return t1465
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var t1471 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    return t1471
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var t1474 bool = self__55 == other__56
    return t1474
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t1497 *ref_int_x = ref__Ref_3int(value__207)
    return t1497
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t1500 int = ref_get__Ref_3int(self__208)
    return t1500
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t1503 string = _goml_runtime_core_int_to_string(self__5)
    return t1503
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func char_from_uint32(value__2 uint32) Option__char {
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    if x1 {
        var t1510 Option__char = Option__char_Some{
            _0: x2,
        }
        return t1510
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t1513 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t1513
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__126 *_goml_vec__goml_m_std_p_json_p_Value, elem__127 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t1518 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t1518
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__126 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__127 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var t1523 rune = _goml_runtime_core_string_get(self__10, index__11)
    return t1523
}

func println__T_string(value__1 string) struct{} {
    var t1525 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t1525)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t1528 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t1528)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t1531 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t1531)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t1539 string = _goml_runtime_core_int_to_string(self__40)
    return t1539
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t1542 string = _goml_runtime_core_bool_to_string(self__37)
    return t1542
}

func main() {
    main0()
}
