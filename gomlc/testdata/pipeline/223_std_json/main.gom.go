package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
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

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
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

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Tuple2_6string_26_goml_m_std_p_json_p_Value struct {
    _0 string
    _1 _goml_m_std_p_json_p_Value
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
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
    var vec_literal__178 *_goml_vec_uint8
    var inline1825 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline1825
    var t260 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t260
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline1840 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline1840
    var t274 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t274, length__5)
    var for_index1 int = 0
    Loop_loop276:
    for {
        var t277 bool = for_index1 < length__5
        if t277 {
            var for_item3 int = for_index1
            var t278 int = for_index1 + 1
            for_index1 = t278
            var t279 *_goml_vec_uint8 = self__3.values
            var t280 uint8
            var inline1836 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t280 = inline1836
            vec_push__Vec_5uint8(t279, t280)
            continue
        } else {
            break Loop_loop276
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t283 string
    var inline1842 string = char_to_string(value__8)
    t283 = inline1842
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t283)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var t559 string = "" + message__2
    var t560 string = t559 + " at byte "
    var t561 *ref_int_x = value__1.index
    var t562 int
    var inline2061 int = ref_get__Ref_3int(t561)
    t562 = inline2061
    var t563 string
    var inline2059 string = _goml_runtime_core_int_to_string(t562)
    t563 = inline2059
    var t564 string = t560 + t563
    return t564
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop579:
    for {
        var t587 *ref_int_x = value__4.index
        var t588 int
        var inline2094 int = ref_get__Ref_3int(t587)
        t588 = inline2094
        var t589 string = value__4.input
        var t590 int
        var inline2092 int = _goml_runtime_core_string_len(t589)
        t590 = inline2092
        var t591 bool = t588 < t590
        var jp581 bool
        if t591 {
            var t592 string = value__4.input
            var t593 *ref_int_x = value__4.index
            var t594 int
            var inline2086 int = ref_get__Ref_3int(t593)
            t594 = inline2086
            var t595 uint8
            var inline2084 uint8 = _goml_runtime_core_string_byte_get(t592, t594)
            t595 = inline2084
            var inline2075 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t595, 9)
            var inline2077 bool
            if inline2075 {
                inline2077 = true
            } else {
                var inline2082 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t595, 10)
                inline2077 = inline2082
            }
            var inline2079 bool
            if inline2077 {
                inline2079 = true
            } else {
                var inline2081 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t595, 13)
                inline2079 = inline2081
            }
            if inline2079 {
                jp581 = true
            } else {
                var inline2080 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t595, 32)
                jp581 = inline2080
            }
        } else {
            jp581 = false
        }
        if jp581 {
            var t582 *ref_int_x = value__4.index
            var t583 *ref_int_x = value__4.index
            var t584 int
            var inline2090 int = ref_get__Ref_3int(t583)
            t584 = inline2090
            var t585 int = t584 + 1
            ref_set__Ref_3int(t582, t585)
            continue
        } else {
            break Loop_loop579
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var t626 bool = value__5 >= 48
    var jp602 bool
    if t626 {
        var t627 bool = value__5 <= 57
        jp602 = t627
    } else {
        jp602 = false
    }
    if jp602 {
        var t603 uint8 = value__5 - 48
        var t604 uint32 = uint32(uint8(t603))
        var t605 Option__uint32 = Option__uint32_Some{
            _0: t604,
        }
        return t605
    } else {
        var t624 bool = value__5 >= 65
        var jp609 bool
        if t624 {
            var t625 bool = value__5 <= 70
            jp609 = t625
        } else {
            jp609 = false
        }
        if jp609 {
            var t610 uint8 = value__5 - 65
            var t611 uint8 = t610 + 10
            var t612 uint32 = uint32(uint8(t611))
            var t613 Option__uint32 = Option__uint32_Some{
                _0: t612,
            }
            return t613
        } else {
            var t622 bool = value__5 >= 97
            var jp617 bool
            if t622 {
                var t623 bool = value__5 <= 102
                jp617 = t623
            } else {
                jp617 = false
            }
            if jp617 {
                var t618 uint8 = value__5 - 97
                var t619 uint8 = t618 + 10
                var t620 uint32 = uint32(uint8(t619))
                var t621 Option__uint32 = Option__uint32_Some{
                    _0: t620,
                }
                return t621
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t632 *ref_int_x = value__6.index
    var t633 int
    var inline2122 int = ref_get__Ref_3int(t632)
    t633 = inline2122
    var t634 int = t633 + 4
    var t635 string = value__6.input
    var t636 int
    var inline2120 int = _goml_runtime_core_string_len(t635)
    t636 = inline2120
    var t637 bool = t634 > t636
    if t637 {
        var t638 string
        var inline2096 string = "incomplete unicode escape"
        var inline2097 string = "" + inline2096
        var inline2098 string = inline2097 + " at byte "
        var inline2099 *ref_int_x = value__6.index
        var inline2100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2099)
        var inline2101 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2100)
        var inline2102 string = inline2098 + inline2101
        t638 = inline2102
        var t639 Result__uint32__string = Result__uint32__string_Err{
            _0: t638,
        }
        return t639
    } else {
        var result__7_source int = 0
        var result__7 uint32 = uint32(int(result__7_source))
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop646:
        for {
            var t647 bool = for_index0 < for_limit1
            if t647 {
                var for_item2 int = for_index0
                var t648 int = for_index0 + 1
                for_index0 = t648
                var t649 string = value__6.input
                var t650 *ref_int_x = value__6.index
                var t651 int
                var inline2114 int = ref_get__Ref_3int(t650)
                t651 = inline2114
                var t652 int = t651 + for_item2
                var t653 uint8
                var inline2112 uint8 = _goml_runtime_core_string_byte_get(t649, t652)
                t653 = inline2112
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t653)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t655 string
                    var inline2104 string = "invalid unicode escape"
                    var inline2105 string = "" + inline2104
                    var inline2106 string = inline2105 + " at byte "
                    var inline2107 *ref_int_x = value__6.index
                    var inline2108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2107)
                    var inline2109 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2108)
                    var inline2110 string = inline2106 + inline2109
                    t655 = inline2110
                    var t656 Result__uint32__string = Result__uint32__string_Err{
                        _0: t655,
                    }
                    return t656
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var t657 uint32 = result__7 * 16
                    var t658 uint32 = t657 + x5
                    result__7 = t658
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop646
            }
        }
        var t641 *ref_int_x = value__6.index
        var t642 *ref_int_x = value__6.index
        var t643 int
        var inline2118 int = ref_get__Ref_3int(t642)
        t643 = inline2118
        var t644 int = t643 + 4
        ref_set__Ref_3int(t641, t644)
        var t645 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__7,
        }
        return t645
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var commute_field2797 rune
    var inline2135 bool = utf8_valid_scalar(codepoint__12)
    if inline2135 {
        var inline2136 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__12)
        var inline2138 rune = inline2136._1
        commute_field2797 = inline2138
        var inline2132 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field2797)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__11, inline2132)
        var t665 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t665
    } else {
        var t663 string
        var inline2124 string = "invalid unicode codepoint"
        var inline2125 string = "" + inline2124
        var inline2126 string = inline2125 + " at byte "
        var inline2127 *ref_int_x = value__10.index
        var inline2128 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2127)
        var inline2129 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2128)
        var inline2130 string = inline2126 + inline2129
        t663 = inline2130
        var t664 Result__unit__string = Result__unit__string_Err{
            _0: t663,
        }
        return t664
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp12 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp669 uint32
    switch mtmp12.(type) {
    case Result__uint32__string_Ok:
        var x13 uint32 = mtmp12.(Result__uint32__string_Ok)._0
        jp669 = x13
        var t731 bool = jp669 >= 55296
        var jp673 bool
        if t731 {
            var t732 bool = jp669 <= 56319
            jp673 = t732
        } else {
            jp673 = false
        }
        if jp673 {
            var t710 *ref_int_x = value__14.index
            var t711 int
            var inline2186 int = ref_get__Ref_3int(t710)
            t711 = inline2186
            var t712 int = t711 + 2
            var t713 string = value__14.input
            var t714 int
            var inline2184 int = _goml_runtime_core_string_len(t713)
            t714 = inline2184
            var t715 bool = t712 > t714
            var jp702 bool
            if t715 {
                jp702 = true
            } else {
                var t716 string = value__14.input
                var t717 *ref_int_x = value__14.index
                var t718 int
                var inline2147 int = ref_get__Ref_3int(t717)
                t718 = inline2147
                var t719 uint8
                var inline2145 uint8 = _goml_runtime_core_string_byte_get(t716, t718)
                t719 = inline2145
                var t720 bool
                var inline2142 uint8 = 92
                var inline2143 bool = t719 == inline2142
                t720 = inline2143
                var t721 bool = !t720
                jp702 = t721
            }
            var jp677 bool
            if jp702 {
                jp677 = true
            } else {
                var t703 string = value__14.input
                var t704 *ref_int_x = value__14.index
                var t705 int
                var inline2154 int = ref_get__Ref_3int(t704)
                t705 = inline2154
                var t706 int = t705 + 1
                var t707 uint8
                var inline2152 uint8 = _goml_runtime_core_string_byte_get(t703, t706)
                t707 = inline2152
                var t708 bool
                var inline2149 uint8 = 117
                var inline2150 bool = t707 == inline2149
                t708 = inline2150
                var t709 bool = !t708
                jp677 = t709
            }
            if jp677 {
                var t678 string
                var inline2156 string = "missing low surrogate"
                var inline2157 string = "" + inline2156
                var inline2158 string = inline2157 + " at byte "
                var inline2159 *ref_int_x = value__14.index
                var inline2160 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2159)
                var inline2161 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2160)
                var inline2162 string = inline2158 + inline2161
                t678 = inline2162
                var t679 Result__unit__string = Result__unit__string_Err{
                    _0: t678,
                }
                return t679
            } else {
                var t680 *ref_int_x = value__14.index
                var t681 *ref_int_x = value__14.index
                var t682 int
                var inline2182 int = ref_get__Ref_3int(t681)
                t682 = inline2182
                var t683 int = t682 + 2
                ref_set__Ref_3int(t680, t683)
                var mtmp16 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp685 uint32
                switch mtmp16.(type) {
                case Result__uint32__string_Ok:
                    var x17 uint32 = mtmp16.(Result__uint32__string_Ok)._0
                    jp685 = x17
                    var t698 bool = jp685 < 56320
                    var jp689 bool
                    if t698 {
                        jp689 = true
                    } else {
                        var t699 bool = jp685 > 57343
                        jp689 = t699
                    }
                    if jp689 {
                        var t690 string
                        var inline2164 string = "invalid low surrogate"
                        var inline2165 string = "" + inline2164
                        var inline2166 string = inline2165 + " at byte "
                        var inline2167 *ref_int_x = value__14.index
                        var inline2168 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2167)
                        var inline2169 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2168)
                        var inline2170 string = inline2166 + inline2169
                        t690 = inline2170
                        var t691 Result__unit__string = Result__unit__string_Err{
                            _0: t690,
                        }
                        return t691
                    } else {
                        var t692 uint32 = jp669 - 55296
                        var t693 uint32 = t692 * 1024
                        var t694 uint32 = 65536 + t693
                        var t695 uint32 = t694 + jp685
                        var t696 uint32 = t695 - 56320
                        var inline2172 Option__char = char_from_uint32(t696)
                        switch inline2172.(type) {
                        case Option__char_None:
                            var inline2173 string = _goml_m_std_p_json_p_json__error(value__14, "invalid unicode codepoint")
                            var inline2174 Result__unit__string = Result__unit__string_Err{
                                _0: inline2173,
                            }
                            return inline2174
                        case Option__char_Some:
                            var inline2175 rune = inline2172.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__15, inline2175)
                            var inline2178 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline2178
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x18 string = mtmp16.(Result__uint32__string_Err)._0
                    var t700 Result__unit__string = Result__unit__string_Err{
                        _0: x18,
                    }
                    return t700
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t729 bool = jp669 >= 56320
            var jp725 bool
            if t729 {
                var t730 bool = jp669 <= 57343
                jp725 = t730
            } else {
                jp725 = false
            }
            if jp725 {
                var t726 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t727 Result__unit__string = Result__unit__string_Err{
                    _0: t726,
                }
                return t727
            } else {
                var t728 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, jp669)
                return t728
            }
        }
    case Result__uint32__string_Err:
        var x14 string = mtmp12.(Result__uint32__string_Err)._0
        var t733 Result__unit__string = Result__unit__string_Err{
            _0: x14,
        }
        return t733
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__18 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t849 *ref_int_x = value__18.index
    var t850 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t849)
    var t851 string = value__18.input
    var t852 int = _goml_m_inherent_i_string_i_string_i_byte__len(t851)
    var t853 bool = t850 >= t852
    var jp841 bool
    if t853 {
        jp841 = true
    } else {
        var t854 string = value__18.input
        var t855 *ref_int_x = value__18.index
        var t856 int
        var inline2193 int = ref_get__Ref_3int(t855)
        t856 = inline2193
        var t857 uint8
        var inline2191 uint8 = _goml_runtime_core_string_byte_get(t854, t856)
        t857 = inline2191
        var t858 bool
        var inline2188 uint8 = 34
        var inline2189 bool = t857 == inline2188
        t858 = inline2189
        var t859 bool = !t858
        jp841 = t859
    }
    if jp841 {
        var t842 string
        var inline2195 string = "expected string"
        var inline2196 string = "" + inline2195
        var inline2197 string = inline2196 + " at byte "
        var inline2198 *ref_int_x = value__18.index
        var inline2199 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2198)
        var inline2200 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2199)
        var inline2201 string = inline2197 + inline2200
        t842 = inline2201
        var t843 Result__string__string = Result__string__string_Err{
            _0: t842,
        }
        return t843
    } else {
        var t844 *ref_int_x = value__18.index
        var t845 *ref_int_x = value__18.index
        var t846 int
        var inline2205 int = ref_get__Ref_3int(t845)
        t846 = inline2205
        var t847 int = t846 + 1
        ref_set__Ref_3int(t844, t847)
        var builder__19 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t737 *ref_int_x = value__18.index
        var segment__20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t737)
        Loop_loop741:
        for {
            var t742 *ref_int_x = value__18.index
            var t743 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t742)
            var t744 string = value__18.input
            var t745 int = _goml_m_inherent_i_string_i_string_i_byte__len(t744)
            var t746 bool = t743 < t745
            if t746 {
                var t747 string = value__18.input
                var t748 *ref_int_x = value__18.index
                var t749 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t748)
                var byte__21 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t747, t749)
                var t751 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 34)
                if t751 {
                    var t759 *ref_int_x = value__18.index
                    var t760 int
                    var inline2221 int = ref_get__Ref_3int(t759)
                    t760 = inline2221
                    var t761 bool = segment__20 < t760
                    if t761 {
                        var t762 string = value__18.input
                        var t763 *ref_int_x = value__18.index
                        var t764 int
                        var inline2209 int = ref_get__Ref_3int(t763)
                        t764 = inline2209
                        var t765 string
                        var inline2207 string = string_byte_slice(t762, segment__20, t764)
                        t765 = inline2207
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t765)
                    } else {}
                    var t753 *ref_int_x = value__18.index
                    var t754 *ref_int_x = value__18.index
                    var t755 int
                    var inline2219 int = ref_get__Ref_3int(t754)
                    t755 = inline2219
                    var t756 int = t755 + 1
                    ref_set__Ref_3int(t753, t756)
                    var t757 string
                    var inline2211 *_goml_vec_uint8 = builder__19.values
                    var inline2212 Tuple2_4bool_6string = string_from_utf8(inline2211)
                    var inline2214 string = inline2212._1
                    t757 = inline2214
                    var t758 Result__string__string = Result__string__string_Ok{
                        _0: t757,
                    }
                    return t758
                } else {
                    var t768 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 92)
                    if t768 {
                        var t823 *ref_int_x = value__18.index
                        var t824 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t823)
                        var t825 bool = segment__20 < t824
                        if t825 {
                            var t826 string = value__18.input
                            var t827 *ref_int_x = value__18.index
                            var t828 int
                            var inline2225 int = ref_get__Ref_3int(t827)
                            t828 = inline2225
                            var t829 string
                            var inline2223 string = string_byte_slice(t826, segment__20, t828)
                            t829 = inline2223
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t829)
                        } else {}
                        var t770 *ref_int_x = value__18.index
                        var t771 *ref_int_x = value__18.index
                        var t772 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t771)
                        var t773 int = t772 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t770, t773)
                        var t816 *ref_int_x = value__18.index
                        var t817 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t816)
                        var t818 string = value__18.input
                        var t819 int = _goml_m_inherent_i_string_i_string_i_byte__len(t818)
                        var t820 bool = t817 >= t819
                        if t820 {
                            var t821 string
                            var inline2227 string = "incomplete escape"
                            var inline2228 string = "" + inline2227
                            var inline2229 string = inline2228 + " at byte "
                            var inline2230 *ref_int_x = value__18.index
                            var inline2231 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2230)
                            var inline2232 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2231)
                            var inline2233 string = inline2229 + inline2232
                            t821 = inline2233
                            var t822 Result__string__string = Result__string__string_Err{
                                _0: t821,
                            }
                            return t822
                        } else {
                            var t775 string = value__18.input
                            var t776 *ref_int_x = value__18.index
                            var t777 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t776)
                            var escape__22 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t775, t777)
                            var t778 *ref_int_x = value__18.index
                            var t779 *ref_int_x = value__18.index
                            var t780 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t779)
                            var t781 int = t780 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t778, t781)
                            var t785 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 34)
                            if t785 {
                                var inline2235 rune = 34
                                var inline2236 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2235)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, inline2236)
                                var t783 *ref_int_x = value__18.index
                                var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
                                segment__20 = t784
                                continue
                            } else {
                                var t788 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 92)
                                if t788 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 92)
                                    var t783 *ref_int_x = value__18.index
                                    var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
                                    segment__20 = t784
                                    continue
                                } else {
                                    var t791 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 47)
                                    if t791 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 47)
                                        var t783 *ref_int_x = value__18.index
                                        var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
                                        segment__20 = t784
                                        continue
                                    } else {
                                        var t794 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 98)
                                        if t794 {
                                            var mtmp26 Option__char = char_from_uint32(8)
                                            switch mtmp26.(type) {
                                            case Option__char_None:
                                                var t783 *ref_int_x = value__18.index
                                                var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
                                                segment__20 = t784
                                                continue
                                            case Option__char_Some:
                                                var x27 rune = mtmp26.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x27)
                                                var t783 *ref_int_x = value__18.index
                                                var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
                                                segment__20 = t784
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t798 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 102)
                                            if t798 {
                                                var mtmp28 Option__char = char_from_uint32(12)
                                                switch mtmp28.(type) {
                                                case Option__char_None:
                                                    var t783 *ref_int_x = value__18.index
                                                    var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
                                                    segment__20 = t784
                                                    continue
                                                case Option__char_Some:
                                                    var x29 rune = mtmp28.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x29)
                                                    var t783 *ref_int_x = value__18.index
                                                    var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
                                                    segment__20 = t784
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t802 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 110)
                                                if t802 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 10)
                                                    var t783 *ref_int_x = value__18.index
                                                    var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
                                                    segment__20 = t784
                                                    continue
                                                } else {
                                                    var t805 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 114)
                                                    if t805 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 13)
                                                        var t783 *ref_int_x = value__18.index
                                                        var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
                                                        segment__20 = t784
                                                        continue
                                                    } else {
                                                        var t808 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 116)
                                                        if t808 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 9)
                                                            var t783 *ref_int_x = value__18.index
                                                            var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
                                                            segment__20 = t784
                                                            continue
                                                        } else {
                                                            var t811 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 117)
                                                            if t811 {
                                                                var mtmp30 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__18, builder__19)
                                                                switch mtmp30.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t783 *ref_int_x = value__18.index
                                                                    var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
                                                                    segment__20 = t784
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x32 string = mtmp30.(Result__unit__string_Err)._0
                                                                    var t813 Result__string__string = Result__string__string_Err{
                                                                        _0: x32,
                                                                    }
                                                                    return t813
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t814 string = _goml_m_std_p_json_p_json__error(value__18, "invalid escape")
                                                                var t815 Result__string__string = Result__string__string_Err{
                                                                    _0: t814,
                                                                }
                                                                return t815
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
                        var t832 bool = byte__21 < 32
                        if t832 {
                            var t833 string = _goml_m_std_p_json_p_json__error(value__18, "unescaped control character")
                            var t834 Result__string__string = Result__string__string_Err{
                                _0: t833,
                            }
                            return t834
                        } else {
                            var t835 *ref_int_x = value__18.index
                            var t836 *ref_int_x = value__18.index
                            var t837 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t836)
                            var t838 int = t837 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t835, t838)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop741
            }
        }
        var t739 string = _goml_m_std_p_json_p_json__error(value__18, "unterminated string")
        var t740 Result__string__string = Result__string__string_Err{
            _0: t739,
        }
        return t740
    }
}

func _goml_m_std_p_json_p_parse__digits(value__26 _goml_m_std_p_json_p_JsonParser) bool {
    var t868 *ref_int_x = value__26.index
    var start__27 int
    var inline2256 int = ref_get__Ref_3int(t868)
    start__27 = inline2256
    Loop_loop873:
    for {
        var t881 *ref_int_x = value__26.index
        var t882 int
        var inline2252 int = ref_get__Ref_3int(t881)
        t882 = inline2252
        var t883 string = value__26.input
        var t884 int
        var inline2250 int = _goml_runtime_core_string_len(t883)
        t884 = inline2250
        var t885 bool = t882 < t884
        var jp875 bool
        if t885 {
            var t886 string = value__26.input
            var t887 *ref_int_x = value__26.index
            var t888 int
            var inline2244 int = ref_get__Ref_3int(t887)
            t888 = inline2244
            var t889 uint8
            var inline2242 uint8 = _goml_runtime_core_string_byte_get(t886, t888)
            t889 = inline2242
            var inline2239 bool = t889 >= 48
            if inline2239 {
                var inline2240 bool = t889 <= 57
                jp875 = inline2240
            } else {
                jp875 = false
            }
        } else {
            jp875 = false
        }
        if jp875 {
            var t876 *ref_int_x = value__26.index
            var t877 *ref_int_x = value__26.index
            var t878 int
            var inline2248 int = ref_get__Ref_3int(t877)
            t878 = inline2248
            var t879 int = t878 + 1
            ref_set__Ref_3int(t876, t879)
            continue
        } else {
            break Loop_loop873
        }
    }
    var t870 *ref_int_x = value__26.index
    var t871 int
    var inline2254 int = ref_get__Ref_3int(t870)
    t871 = inline2254
    var t872 bool = t871 > start__27
    return t872
}

func _goml_m_std_p_json_p_parse__json__number(value__28 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t893 *ref_int_x = value__28.index
    var start__29 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t893)
    var t1015 string = value__28.input
    var t1016 *ref_int_x = value__28.index
    var t1017 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1016)
    var t1018 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1015, t1017)
    var t1019 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1018, 45)
    if t1019 {
        var t1020 *ref_int_x = value__28.index
        var t1021 *ref_int_x = value__28.index
        var t1022 int
        var inline2260 int = ref_get__Ref_3int(t1021)
        t1022 = inline2260
        var t1023 int = t1022 + 1
        ref_set__Ref_3int(t1020, t1023)
    } else {}
    var t978 *ref_int_x = value__28.index
    var t979 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t978)
    var t980 string = value__28.input
    var t981 int = _goml_m_inherent_i_string_i_string_i_byte__len(t980)
    var t982 bool = t979 >= t981
    if t982 {
        var t983 string
        var inline2262 string = "incomplete number"
        var inline2263 string = "" + inline2262
        var inline2264 string = inline2263 + " at byte "
        var inline2265 *ref_int_x = value__28.index
        var inline2266 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2265)
        var inline2267 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2266)
        var inline2268 string = inline2264 + inline2267
        t983 = inline2268
        var t984 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t983,
        }
        return t984
    } else {
        var t986 string = value__28.input
        var t987 *ref_int_x = value__28.index
        var t988 int
        var inline2306 int = ref_get__Ref_3int(t987)
        t988 = inline2306
        var t989 uint8
        var inline2304 uint8 = _goml_runtime_core_string_byte_get(t986, t988)
        t989 = inline2304
        var t990 bool
        var inline2301 uint8 = 48
        var inline2302 bool = t989 == inline2301
        t990 = inline2302
        if t990 {
            var t991 *ref_int_x = value__28.index
            var t992 *ref_int_x = value__28.index
            var t993 int
            var inline2291 int = ref_get__Ref_3int(t992)
            t993 = inline2291
            var t994 int = t993 + 1
            ref_set__Ref_3int(t991, t994)
            var t1000 *ref_int_x = value__28.index
            var t1001 int
            var inline2287 int = ref_get__Ref_3int(t1000)
            t1001 = inline2287
            var t1002 string = value__28.input
            var t1003 int
            var inline2285 int = _goml_runtime_core_string_len(t1002)
            t1003 = inline2285
            var t1004 bool = t1001 < t1003
            var jp997 bool
            if t1004 {
                var t1005 string = value__28.input
                var t1006 *ref_int_x = value__28.index
                var t1007 int
                var inline2275 int = ref_get__Ref_3int(t1006)
                t1007 = inline2275
                var t1008 uint8
                var inline2273 uint8 = _goml_runtime_core_string_byte_get(t1005, t1007)
                t1008 = inline2273
                var inline2270 bool = t1008 >= 48
                if inline2270 {
                    var inline2271 bool = t1008 <= 57
                    jp997 = inline2271
                } else {
                    jp997 = false
                }
            } else {
                jp997 = false
            }
            if jp997 {
                var t998 string
                var inline2277 string = "invalid leading zero"
                var inline2278 string = "" + inline2277
                var inline2279 string = inline2278 + " at byte "
                var inline2280 *ref_int_x = value__28.index
                var inline2281 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2280)
                var inline2282 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2281)
                var inline2283 string = inline2279 + inline2282
                t998 = inline2283
                var t999 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t998,
                }
                return t999
            } else {
                var t968 *ref_int_x = value__28.index
                var t969 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t968)
                var t970 string = value__28.input
                var t971 int = _goml_m_inherent_i_string_i_string_i_byte__len(t970)
                var t972 bool = t969 < t971
                var jp958 bool
                if t972 {
                    var t973 string = value__28.input
                    var t974 *ref_int_x = value__28.index
                    var t975 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t974)
                    var t976 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t973, t975)
                    var t977 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t976, 46)
                    jp958 = t977
                } else {
                    jp958 = false
                }
                if jp958 {
                    var t959 *ref_int_x = value__28.index
                    var t960 *ref_int_x = value__28.index
                    var t961 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t960)
                    var t962 int = t961 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t959, t962)
                    var t964 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t965 bool = !t964
                    if t965 {
                        var t966 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t967 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t966,
                        }
                        return t967
                    } else {
                        var t940 *ref_int_x = value__28.index
                        var t941 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t940)
                        var t942 string = value__28.input
                        var t943 int = _goml_m_inherent_i_string_i_string_i_byte__len(t942)
                        var t944 bool = t941 < t943
                        var jp905 bool
                        if t944 {
                            var t947 string = value__28.input
                            var t948 *ref_int_x = value__28.index
                            var t949 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t948)
                            var t950 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t947, t949)
                            var t951 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t950, 101)
                            if t951 {
                                jp905 = true
                            } else {
                                var t952 string = value__28.input
                                var t953 *ref_int_x = value__28.index
                                var t954 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t953)
                                var t955 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t952, t954)
                                var t956 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t955, 69)
                                jp905 = t956
                            }
                        } else {
                            jp905 = false
                        }
                        if jp905 {
                            var t906 *ref_int_x = value__28.index
                            var t907 *ref_int_x = value__28.index
                            var t908 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t907)
                            var t909 int = t908 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t906, t909)
                            var t923 *ref_int_x = value__28.index
                            var t924 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t923)
                            var t925 string = value__28.input
                            var t926 int = _goml_m_inherent_i_string_i_string_i_byte__len(t925)
                            var t927 bool = t924 < t926
                            var jp917 bool
                            if t927 {
                                var t930 string = value__28.input
                                var t931 *ref_int_x = value__28.index
                                var t932 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t931)
                                var t933 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t930, t932)
                                var t934 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t933, 43)
                                if t934 {
                                    jp917 = true
                                } else {
                                    var t935 string = value__28.input
                                    var t936 *ref_int_x = value__28.index
                                    var t937 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t936)
                                    var t938 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t935, t937)
                                    var t939 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t938, 45)
                                    jp917 = t939
                                }
                            } else {
                                jp917 = false
                            }
                            if jp917 {
                                var t918 *ref_int_x = value__28.index
                                var t919 *ref_int_x = value__28.index
                                var t920 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t919)
                                var t921 int = t920 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t918, t921)
                            } else {}
                            var t912 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t913 bool = !t912
                            if t913 {
                                var t914 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t915 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t914,
                                }
                                return t915
                            } else {
                                var t898 string = value__28.input
                                var t899 *ref_int_x = value__28.index
                                var t900 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t899)
                                var t901 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t898, start__29, t900)
                                var t902 _goml_m_std_p_json_p_Value = Number{
                                    _0: t901,
                                }
                                var t903 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t902,
                                }
                                return t903
                            }
                        } else {
                            var t898 string = value__28.input
                            var t899 *ref_int_x = value__28.index
                            var t900 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t899)
                            var t901 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t898, start__29, t900)
                            var t902 _goml_m_std_p_json_p_Value = Number{
                                _0: t901,
                            }
                            var t903 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t902,
                            }
                            return t903
                        }
                    }
                } else {
                    var t940 *ref_int_x = value__28.index
                    var t941 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t940)
                    var t942 string = value__28.input
                    var t943 int = _goml_m_inherent_i_string_i_string_i_byte__len(t942)
                    var t944 bool = t941 < t943
                    var jp905 bool
                    if t944 {
                        var t947 string = value__28.input
                        var t948 *ref_int_x = value__28.index
                        var t949 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t948)
                        var t950 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t947, t949)
                        var t951 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t950, 101)
                        if t951 {
                            jp905 = true
                        } else {
                            var t952 string = value__28.input
                            var t953 *ref_int_x = value__28.index
                            var t954 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t953)
                            var t955 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t952, t954)
                            var t956 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t955, 69)
                            jp905 = t956
                        }
                    } else {
                        jp905 = false
                    }
                    if jp905 {
                        var t906 *ref_int_x = value__28.index
                        var t907 *ref_int_x = value__28.index
                        var t908 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t907)
                        var t909 int = t908 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t906, t909)
                        var t923 *ref_int_x = value__28.index
                        var t924 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t923)
                        var t925 string = value__28.input
                        var t926 int = _goml_m_inherent_i_string_i_string_i_byte__len(t925)
                        var t927 bool = t924 < t926
                        var jp917 bool
                        if t927 {
                            var t930 string = value__28.input
                            var t931 *ref_int_x = value__28.index
                            var t932 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t931)
                            var t933 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t930, t932)
                            var t934 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t933, 43)
                            if t934 {
                                jp917 = true
                            } else {
                                var t935 string = value__28.input
                                var t936 *ref_int_x = value__28.index
                                var t937 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t936)
                                var t938 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t935, t937)
                                var t939 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t938, 45)
                                jp917 = t939
                            }
                        } else {
                            jp917 = false
                        }
                        if jp917 {
                            var t918 *ref_int_x = value__28.index
                            var t919 *ref_int_x = value__28.index
                            var t920 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t919)
                            var t921 int = t920 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t918, t921)
                        } else {}
                        var t912 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t913 bool = !t912
                        if t913 {
                            var t914 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t915 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t914,
                            }
                            return t915
                        } else {
                            var t898 string = value__28.input
                            var t899 *ref_int_x = value__28.index
                            var t900 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t899)
                            var t901 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t898, start__29, t900)
                            var t902 _goml_m_std_p_json_p_Value = Number{
                                _0: t901,
                            }
                            var t903 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t902,
                            }
                            return t903
                        }
                    } else {
                        var t898 string = value__28.input
                        var t899 *ref_int_x = value__28.index
                        var t900 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t899)
                        var t901 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t898, start__29, t900)
                        var t902 _goml_m_std_p_json_p_Value = Number{
                            _0: t901,
                        }
                        var t903 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t902,
                        }
                        return t903
                    }
                }
            }
        } else {
            var t1011 bool = _goml_m_std_p_json_p_parse__digits(value__28)
            var t1012 bool = !t1011
            if t1012 {
                var t1013 string
                var inline2293 string = "expected number"
                var inline2294 string = "" + inline2293
                var inline2295 string = inline2294 + " at byte "
                var inline2296 *ref_int_x = value__28.index
                var inline2297 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2296)
                var inline2298 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2297)
                var inline2299 string = inline2295 + inline2298
                t1013 = inline2299
                var t1014 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1013,
                }
                return t1014
            } else {
                var t968 *ref_int_x = value__28.index
                var t969 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t968)
                var t970 string = value__28.input
                var t971 int = _goml_m_inherent_i_string_i_string_i_byte__len(t970)
                var t972 bool = t969 < t971
                var jp958 bool
                if t972 {
                    var t973 string = value__28.input
                    var t974 *ref_int_x = value__28.index
                    var t975 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t974)
                    var t976 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t973, t975)
                    var t977 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t976, 46)
                    jp958 = t977
                } else {
                    jp958 = false
                }
                if jp958 {
                    var t959 *ref_int_x = value__28.index
                    var t960 *ref_int_x = value__28.index
                    var t961 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t960)
                    var t962 int = t961 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t959, t962)
                    var t964 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t965 bool = !t964
                    if t965 {
                        var t966 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t967 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t966,
                        }
                        return t967
                    } else {
                        var t940 *ref_int_x = value__28.index
                        var t941 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t940)
                        var t942 string = value__28.input
                        var t943 int = _goml_m_inherent_i_string_i_string_i_byte__len(t942)
                        var t944 bool = t941 < t943
                        var jp905 bool
                        if t944 {
                            var t947 string = value__28.input
                            var t948 *ref_int_x = value__28.index
                            var t949 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t948)
                            var t950 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t947, t949)
                            var t951 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t950, 101)
                            if t951 {
                                jp905 = true
                            } else {
                                var t952 string = value__28.input
                                var t953 *ref_int_x = value__28.index
                                var t954 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t953)
                                var t955 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t952, t954)
                                var t956 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t955, 69)
                                jp905 = t956
                            }
                        } else {
                            jp905 = false
                        }
                        if jp905 {
                            var t906 *ref_int_x = value__28.index
                            var t907 *ref_int_x = value__28.index
                            var t908 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t907)
                            var t909 int = t908 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t906, t909)
                            var t923 *ref_int_x = value__28.index
                            var t924 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t923)
                            var t925 string = value__28.input
                            var t926 int = _goml_m_inherent_i_string_i_string_i_byte__len(t925)
                            var t927 bool = t924 < t926
                            var jp917 bool
                            if t927 {
                                var t930 string = value__28.input
                                var t931 *ref_int_x = value__28.index
                                var t932 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t931)
                                var t933 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t930, t932)
                                var t934 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t933, 43)
                                if t934 {
                                    jp917 = true
                                } else {
                                    var t935 string = value__28.input
                                    var t936 *ref_int_x = value__28.index
                                    var t937 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t936)
                                    var t938 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t935, t937)
                                    var t939 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t938, 45)
                                    jp917 = t939
                                }
                            } else {
                                jp917 = false
                            }
                            if jp917 {
                                var t918 *ref_int_x = value__28.index
                                var t919 *ref_int_x = value__28.index
                                var t920 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t919)
                                var t921 int = t920 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t918, t921)
                            } else {}
                            var t912 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t913 bool = !t912
                            if t913 {
                                var t914 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t915 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t914,
                                }
                                return t915
                            } else {
                                var t898 string = value__28.input
                                var t899 *ref_int_x = value__28.index
                                var t900 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t899)
                                var t901 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t898, start__29, t900)
                                var t902 _goml_m_std_p_json_p_Value = Number{
                                    _0: t901,
                                }
                                var t903 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t902,
                                }
                                return t903
                            }
                        } else {
                            var t898 string = value__28.input
                            var t899 *ref_int_x = value__28.index
                            var t900 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t899)
                            var t901 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t898, start__29, t900)
                            var t902 _goml_m_std_p_json_p_Value = Number{
                                _0: t901,
                            }
                            var t903 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t902,
                            }
                            return t903
                        }
                    }
                } else {
                    var t940 *ref_int_x = value__28.index
                    var t941 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t940)
                    var t942 string = value__28.input
                    var t943 int = _goml_m_inherent_i_string_i_string_i_byte__len(t942)
                    var t944 bool = t941 < t943
                    var jp905 bool
                    if t944 {
                        var t947 string = value__28.input
                        var t948 *ref_int_x = value__28.index
                        var t949 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t948)
                        var t950 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t947, t949)
                        var t951 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t950, 101)
                        if t951 {
                            jp905 = true
                        } else {
                            var t952 string = value__28.input
                            var t953 *ref_int_x = value__28.index
                            var t954 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t953)
                            var t955 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t952, t954)
                            var t956 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t955, 69)
                            jp905 = t956
                        }
                    } else {
                        jp905 = false
                    }
                    if jp905 {
                        var t906 *ref_int_x = value__28.index
                        var t907 *ref_int_x = value__28.index
                        var t908 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t907)
                        var t909 int = t908 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t906, t909)
                        var t923 *ref_int_x = value__28.index
                        var t924 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t923)
                        var t925 string = value__28.input
                        var t926 int = _goml_m_inherent_i_string_i_string_i_byte__len(t925)
                        var t927 bool = t924 < t926
                        var jp917 bool
                        if t927 {
                            var t930 string = value__28.input
                            var t931 *ref_int_x = value__28.index
                            var t932 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t931)
                            var t933 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t930, t932)
                            var t934 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t933, 43)
                            if t934 {
                                jp917 = true
                            } else {
                                var t935 string = value__28.input
                                var t936 *ref_int_x = value__28.index
                                var t937 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t936)
                                var t938 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t935, t937)
                                var t939 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t938, 45)
                                jp917 = t939
                            }
                        } else {
                            jp917 = false
                        }
                        if jp917 {
                            var t918 *ref_int_x = value__28.index
                            var t919 *ref_int_x = value__28.index
                            var t920 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t919)
                            var t921 int = t920 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t918, t921)
                        } else {}
                        var t912 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t913 bool = !t912
                        if t913 {
                            var t914 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t915 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t914,
                            }
                            return t915
                        } else {
                            var t898 string = value__28.input
                            var t899 *ref_int_x = value__28.index
                            var t900 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t899)
                            var t901 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t898, start__29, t900)
                            var t902 _goml_m_std_p_json_p_Value = Number{
                                _0: t901,
                            }
                            var t903 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t902,
                            }
                            return t903
                        }
                    } else {
                        var t898 string = value__28.input
                        var t899 *ref_int_x = value__28.index
                        var t900 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t899)
                        var t901 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t898, start__29, t900)
                        var t902 _goml_m_std_p_json_p_Value = Number{
                            _0: t901,
                        }
                        var t903 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t902,
                        }
                        return t903
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__30 _goml_m_std_p_json_p_JsonParser, expected__31 string, result__32 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t1039 *ref_int_x = value__30.index
    var t1040 int
    var inline2336 int = ref_get__Ref_3int(t1039)
    t1040 = inline2336
    var t1041 int
    var inline2334 int = _goml_runtime_core_string_len(expected__31)
    t1041 = inline2334
    var t1042 int = t1040 + t1041
    var t1043 string = value__30.input
    var t1044 int
    var inline2332 int = _goml_runtime_core_string_len(t1043)
    t1044 = inline2332
    var t1045 bool = t1042 <= t1044
    var jp1030 bool
    if t1045 {
        var t1046 string = value__30.input
        var t1047 *ref_int_x = value__30.index
        var t1048 int
        var inline2316 int = ref_get__Ref_3int(t1047)
        t1048 = inline2316
        var t1049 *ref_int_x = value__30.index
        var t1050 int
        var inline2314 int = ref_get__Ref_3int(t1049)
        t1050 = inline2314
        var t1051 int
        var inline2312 int = _goml_runtime_core_string_len(expected__31)
        t1051 = inline2312
        var t1052 int = t1050 + t1051
        var t1053 string
        var inline2310 string = string_byte_slice(t1046, t1048, t1052)
        t1053 = inline2310
        var inline2308 bool = t1053 == expected__31
        jp1030 = inline2308
    } else {
        jp1030 = false
    }
    if jp1030 {
        var t1031 *ref_int_x = value__30.index
        var t1032 *ref_int_x = value__30.index
        var t1033 int
        var inline2322 int = ref_get__Ref_3int(t1032)
        t1033 = inline2322
        var t1034 int
        var inline2320 int = _goml_runtime_core_string_len(expected__31)
        t1034 = inline2320
        var t1035 int = t1033 + t1034
        ref_set__Ref_3int(t1031, t1035)
        var t1036 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__32,
        }
        return t1036
    } else {
        var t1037 string
        var inline2324 string = "invalid literal"
        var inline2325 string = "" + inline2324
        var inline2326 string = inline2325 + " at byte "
        var inline2327 *ref_int_x = value__30.index
        var inline2328 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2327)
        var inline2329 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2328)
        var inline2330 string = inline2326 + inline2329
        t1037 = inline2330
        var t1038 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1037,
        }
        return t1038
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1057 *ref_int_x = value__33.index
    var t1058 *ref_int_x = value__33.index
    var t1059 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1058)
    var t1060 int = t1059 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1057, t1060)
    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
    var vec_literal__8961 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t1115 *ref_int_x = value__33.index
    var t1116 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1115)
    var t1117 string = value__33.input
    var t1118 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1117)
    var t1119 bool = t1116 < t1118
    var jp1108 bool
    if t1119 {
        var t1120 string = value__33.input
        var t1121 *ref_int_x = value__33.index
        var t1122 int
        var inline2343 int = ref_get__Ref_3int(t1121)
        t1122 = inline2343
        var t1123 uint8
        var inline2341 uint8 = _goml_runtime_core_string_byte_get(t1120, t1122)
        t1123 = inline2341
        var inline2338 uint8 = 93
        var inline2339 bool = t1123 == inline2338
        jp1108 = inline2339
    } else {
        jp1108 = false
    }
    if jp1108 {
        var t1109 *ref_int_x = value__33.index
        var t1110 *ref_int_x = value__33.index
        var t1111 int
        var inline2347 int = ref_get__Ref_3int(t1110)
        t1111 = inline2347
        var t1112 int = t1111 + 1
        ref_set__Ref_3int(t1109, t1112)
        var t1113 _goml_m_std_p_json_p_Value = Array{
            _0: vec_literal__8961,
        }
        var t1114 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1113,
        }
        return t1114
    } else {
        Loop_loop1065:
        for {
            var t1066 *ref_int_x = value__33.index
            var t1067 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1066)
            var t1068 string = value__33.input
            var t1069 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1068)
            var t1070 bool = t1067 < t1069
            if t1070 {
                var mtmp50 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__33)
                var jp1072 _goml_m_std_p_json_p_Value
                switch mtmp50.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x51 _goml_m_std_p_json_p_Value = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp1072 = x51
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8961, jp1072)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                    var t1074 *ref_int_x = value__33.index
                    var t1075 int
                    var inline2389 int = ref_get__Ref_3int(t1074)
                    t1075 = inline2389
                    var t1076 string = value__33.input
                    var t1077 int
                    var inline2387 int = _goml_runtime_core_string_len(t1076)
                    t1077 = inline2387
                    var t1078 bool = t1075 >= t1077
                    if t1078 {
                        var t1079 string
                        var inline2349 string = "unterminated array"
                        var inline2350 string = "" + inline2349
                        var inline2351 string = inline2350 + " at byte "
                        var inline2352 *ref_int_x = value__33.index
                        var inline2353 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2352)
                        var inline2354 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2353)
                        var inline2355 string = inline2351 + inline2354
                        t1079 = inline2355
                        var t1080 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1079,
                        }
                        return t1080
                    } else {
                        var t1082 string = value__33.input
                        var t1083 *ref_int_x = value__33.index
                        var t1084 int
                        var inline2385 int = ref_get__Ref_3int(t1083)
                        t1084 = inline2385
                        var t1085 uint8
                        var inline2383 uint8 = _goml_runtime_core_string_byte_get(t1082, t1084)
                        t1085 = inline2383
                        var t1086 bool
                        var inline2380 uint8 = 93
                        var inline2381 bool = t1085 == inline2380
                        t1086 = inline2381
                        if t1086 {
                            var t1087 *ref_int_x = value__33.index
                            var t1088 *ref_int_x = value__33.index
                            var t1089 int
                            var inline2359 int = ref_get__Ref_3int(t1088)
                            t1089 = inline2359
                            var t1090 int = t1089 + 1
                            ref_set__Ref_3int(t1087, t1090)
                            var t1091 _goml_m_std_p_json_p_Value = Array{
                                _0: vec_literal__8961,
                            }
                            var t1092 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1091,
                            }
                            return t1092
                        } else {
                            var t1094 string = value__33.input
                            var t1095 *ref_int_x = value__33.index
                            var t1096 int
                            var inline2378 int = ref_get__Ref_3int(t1095)
                            t1096 = inline2378
                            var t1097 uint8
                            var inline2376 uint8 = _goml_runtime_core_string_byte_get(t1094, t1096)
                            t1097 = inline2376
                            var t1098 bool
                            var inline2373 uint8 = 44
                            var inline2374 bool = t1097 == inline2373
                            t1098 = inline2374
                            if t1098 {
                                var t1099 *ref_int_x = value__33.index
                                var t1100 *ref_int_x = value__33.index
                                var t1101 int
                                var inline2363 int = ref_get__Ref_3int(t1100)
                                t1101 = inline2363
                                var t1102 int = t1101 + 1
                                ref_set__Ref_3int(t1099, t1102)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                                continue
                            } else {
                                var t1104 string
                                var inline2365 string = "expected array separator"
                                var inline2366 string = "" + inline2365
                                var inline2367 string = inline2366 + " at byte "
                                var inline2368 *ref_int_x = value__33.index
                                var inline2369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2368)
                                var inline2370 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2369)
                                var inline2371 string = inline2367 + inline2370
                                t1104 = inline2371
                                var t1105 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1104,
                                }
                                return t1105
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x52 string = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t1106 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x52,
                    }
                    return t1106
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1065
            }
        }
        var t1063 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
        var t1064 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1063,
        }
        return t1064
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__36 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1127 *ref_int_x = value__36.index
    var t1128 *ref_int_x = value__36.index
    var t1129 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1128)
    var t1130 int = t1129 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1127, t1130)
    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
    var vec_literal__10180 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t1210 *ref_int_x = value__36.index
    var t1211 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1210)
    var t1212 string = value__36.input
    var t1213 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1212)
    var t1214 bool = t1211 < t1213
    var jp1203 bool
    if t1214 {
        var t1215 string = value__36.input
        var t1216 *ref_int_x = value__36.index
        var t1217 int
        var inline2396 int = ref_get__Ref_3int(t1216)
        t1217 = inline2396
        var t1218 uint8
        var inline2394 uint8 = _goml_runtime_core_string_byte_get(t1215, t1217)
        t1218 = inline2394
        var inline2391 uint8 = 125
        var inline2392 bool = t1218 == inline2391
        jp1203 = inline2392
    } else {
        jp1203 = false
    }
    if jp1203 {
        var t1204 *ref_int_x = value__36.index
        var t1205 *ref_int_x = value__36.index
        var t1206 int
        var inline2400 int = ref_get__Ref_3int(t1205)
        t1206 = inline2400
        var t1207 int = t1206 + 1
        ref_set__Ref_3int(t1204, t1207)
        var t1208 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10180,
        }
        var t1209 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1208,
        }
        return t1209
    } else {
        Loop_loop1135:
        for {
            var t1136 *ref_int_x = value__36.index
            var t1137 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1136)
            var t1138 string = value__36.input
            var t1139 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1138)
            var t1140 bool = t1137 < t1139
            if t1140 {
                var mtmp62 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__36)
                var jp1142 string
                switch mtmp62.(type) {
                case Result__string__string_Ok:
                    var x63 string = mtmp62.(Result__string__string_Ok)._0
                    jp1142 = x63
                    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                    var t1190 *ref_int_x = value__36.index
                    var t1191 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1190)
                    var t1192 string = value__36.input
                    var t1193 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1192)
                    var t1194 bool = t1191 >= t1193
                    var jp1182 bool
                    if t1194 {
                        jp1182 = true
                    } else {
                        var t1195 string = value__36.input
                        var t1196 *ref_int_x = value__36.index
                        var t1197 int
                        var inline2407 int = ref_get__Ref_3int(t1196)
                        t1197 = inline2407
                        var t1198 uint8
                        var inline2405 uint8 = _goml_runtime_core_string_byte_get(t1195, t1197)
                        t1198 = inline2405
                        var t1199 bool
                        var inline2402 uint8 = 58
                        var inline2403 bool = t1198 == inline2402
                        t1199 = inline2403
                        var t1200 bool = !t1199
                        jp1182 = t1200
                    }
                    if jp1182 {
                        var t1183 string
                        var inline2409 string = "expected object colon"
                        var inline2410 string = "" + inline2409
                        var inline2411 string = inline2410 + " at byte "
                        var inline2412 *ref_int_x = value__36.index
                        var inline2413 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2412)
                        var inline2414 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2413)
                        var inline2415 string = inline2411 + inline2414
                        t1183 = inline2415
                        var t1184 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1183,
                        }
                        return t1184
                    } else {
                        var t1185 *ref_int_x = value__36.index
                        var t1186 *ref_int_x = value__36.index
                        var t1187 int
                        var inline2419 int = ref_get__Ref_3int(t1186)
                        t1187 = inline2419
                        var t1188 int = t1187 + 1
                        ref_set__Ref_3int(t1185, t1188)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                        var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__36)
                        var jp1145 _goml_m_std_p_json_p_Value
                        switch mtmp68.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp1145 = x69
                            var t1146 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp1142,
                                _1: jp1145,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10180, t1146)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                            var t1148 *ref_int_x = value__36.index
                            var t1149 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1148)
                            var t1150 string = value__36.input
                            var t1151 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1150)
                            var t1152 bool = t1149 >= t1151
                            if t1152 {
                                var t1153 string
                                var inline2421 string = "unterminated object"
                                var inline2422 string = "" + inline2421
                                var inline2423 string = inline2422 + " at byte "
                                var inline2424 *ref_int_x = value__36.index
                                var inline2425 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2424)
                                var inline2426 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2425)
                                var inline2427 string = inline2423 + inline2426
                                t1153 = inline2427
                                var t1154 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1153,
                                }
                                return t1154
                            } else {
                                var t1156 string = value__36.input
                                var t1157 *ref_int_x = value__36.index
                                var t1158 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1157)
                                var t1159 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1156, t1158)
                                var t1160 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1159, 125)
                                if t1160 {
                                    var t1161 *ref_int_x = value__36.index
                                    var t1162 *ref_int_x = value__36.index
                                    var t1163 int
                                    var inline2431 int = ref_get__Ref_3int(t1162)
                                    t1163 = inline2431
                                    var t1164 int = t1163 + 1
                                    ref_set__Ref_3int(t1161, t1164)
                                    var t1165 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10180,
                                    }
                                    var t1166 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1165,
                                    }
                                    return t1166
                                } else {
                                    var t1168 string = value__36.input
                                    var t1169 *ref_int_x = value__36.index
                                    var t1170 int
                                    var inline2442 int = ref_get__Ref_3int(t1169)
                                    t1170 = inline2442
                                    var t1171 uint8
                                    var inline2440 uint8 = _goml_runtime_core_string_byte_get(t1168, t1170)
                                    t1171 = inline2440
                                    var t1172 bool
                                    var inline2437 uint8 = 44
                                    var inline2438 bool = t1171 == inline2437
                                    t1172 = inline2438
                                    if t1172 {
                                        var t1173 *ref_int_x = value__36.index
                                        var t1174 *ref_int_x = value__36.index
                                        var t1175 int
                                        var inline2435 int = ref_get__Ref_3int(t1174)
                                        t1175 = inline2435
                                        var t1176 int = t1175 + 1
                                        ref_set__Ref_3int(t1173, t1176)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                                        continue
                                    } else {
                                        var t1178 string = _goml_m_std_p_json_p_json__error(value__36, "expected object separator")
                                        var t1179 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1178,
                                        }
                                        return t1179
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t1180 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x70,
                            }
                            return t1180
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x64 string = mtmp62.(Result__string__string_Err)._0
                    var t1201 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x64,
                    }
                    return t1201
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1135
            }
        }
        var t1133 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
        var t1134 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1133,
        }
        return t1134
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__40 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__40)
    var t1224 *ref_int_x = value__40.index
    var t1225 int
    var inline2472 int = ref_get__Ref_3int(t1224)
    t1225 = inline2472
    var t1226 string = value__40.input
    var t1227 int
    var inline2470 int = _goml_runtime_core_string_len(t1226)
    t1227 = inline2470
    var t1228 bool = t1225 >= t1227
    if t1228 {
        var t1229 string
        var inline2444 string = "expected JSON value"
        var inline2445 string = "" + inline2444
        var inline2446 string = inline2445 + " at byte "
        var inline2447 *ref_int_x = value__40.index
        var inline2448 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2447)
        var inline2449 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2448)
        var inline2450 string = inline2446 + inline2449
        t1229 = inline2450
        var t1230 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1229,
        }
        return t1230
    } else {
        var t1231 string = value__40.input
        var t1232 *ref_int_x = value__40.index
        var t1233 int
        var inline2468 int = ref_get__Ref_3int(t1232)
        t1233 = inline2468
        var mtmp77 uint8
        var inline2466 uint8 = _goml_runtime_core_string_byte_get(t1231, t1233)
        mtmp77 = inline2466
        switch mtmp77 {
        case 123:
            var t1236 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__40)
            return t1236
        case 91:
            var t1237 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__40)
            return t1237
        case 34:
            var mtmp78 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__40)
            switch mtmp78.(type) {
            case Result__string__string_Ok:
                var x79 string = mtmp78.(Result__string__string_Ok)._0
                var t1240 _goml_m_std_p_json_p_Value = String{
                    _0: x79,
                }
                var t1241 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1240,
                }
                return t1241
            case Result__string__string_Err:
                var x80 string = mtmp78.(Result__string__string_Err)._0
                var t1242 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x80,
                }
                return t1242
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t1243 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t1244 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "true", t1243)
            return t1244
        case 102:
            var t1245 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t1246 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "false", t1245)
            return t1246
        case 110:
            var t1247 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "null", Null{})
            return t1247
        default:
            var t1255 bool
            var inline2463 uint8 = 45
            var inline2464 bool = mtmp77 == inline2463
            t1255 = inline2464
            var jp1251 bool
            if t1255 {
                jp1251 = true
            } else {
                var inline2452 bool = mtmp77 >= 48
                if inline2452 {
                    var inline2453 bool = mtmp77 <= 57
                    jp1251 = inline2453
                } else {
                    jp1251 = false
                }
            }
            if jp1251 {
                var t1252 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__40)
                return t1252
            } else {
                var t1253 string
                var inline2455 string = "unexpected JSON token"
                var inline2456 string = "" + inline2455
                var inline2457 string = inline2456 + " at byte "
                var inline2458 *ref_int_x = value__40.index
                var inline2459 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2458)
                var inline2460 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2459)
                var inline2461 string = inline2457 + inline2460
                t1253 = inline2461
                var t1254 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1253,
                }
                return t1254
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__44 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__45 _goml_m_std_p_json_p_JsonParser
    var inline2488 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline2489 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__44,
        index: inline2488,
    }
    parser__45 = inline2489
    var mtmp81 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__45)
    var jp1260 _goml_m_std_p_json_p_Value
    switch mtmp81.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x82 _goml_m_std_p_json_p_Value = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1260 = x82
        _goml_m_std_p_json_p_skip__json__whitespace(parser__45)
        var t1263 *ref_int_x = parser__45.index
        var t1264 int
        var inline2486 int = ref_get__Ref_3int(t1263)
        t1264 = inline2486
        var t1265 int
        var inline2484 int = _goml_runtime_core_string_len(input__44)
        t1265 = inline2484
        var t1266 bool
        var inline2482 bool = t1264 == t1265
        t1266 = inline2482
        if t1266 {
            var t1267 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp1260,
            }
            return t1267
        } else {
            var t1268 string
            var inline2474 string = "trailing JSON data"
            var inline2475 string = "" + inline2474
            var inline2476 string = inline2475 + " at byte "
            var inline2477 *ref_int_x = parser__45.index
            var inline2478 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2477)
            var inline2479 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2478)
            var inline2480 string = inline2476 + inline2479
            t1268 = inline2480
            var t1269 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1268,
            }
            return t1269
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x83 string = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t1270 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x83,
        }
        return t1270
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__48 _goml_m_std_p_text_p_StringBuilder, value__49 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    var start__50 int = 0
    var for_index86 int = 0
    var for_limit87 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    Loop_loop1284:
    for {
        var t1285 bool = for_index86 < for_limit87
        if t1285 {
            var for_item88 int = for_index86
            var t1286 int = for_index86 + 1
            for_index86 = t1286
            var byte__52 uint8
            var inline2550 uint8 = _goml_runtime_core_string_byte_get(value__49, for_item88)
            byte__52 = inline2550
            var t1339 bool
            var inline2547 uint8 = 34
            var inline2548 bool = byte__52 == inline2547
            t1339 = inline2548
            var jp1337 bool
            if t1339 {
                jp1337 = true
            } else {
                var inline2494 uint8 = 92
                var inline2495 bool = byte__52 == inline2494
                jp1337 = inline2495
            }
            var jp1334 bool
            if jp1337 {
                jp1334 = true
            } else {
                var inline2497 uint8 = 8
                var inline2498 bool = byte__52 == inline2497
                jp1334 = inline2498
            }
            var jp1331 bool
            if jp1334 {
                jp1331 = true
            } else {
                var inline2500 uint8 = 9
                var inline2501 bool = byte__52 == inline2500
                jp1331 = inline2501
            }
            var jp1328 bool
            if jp1331 {
                jp1328 = true
            } else {
                var inline2503 uint8 = 10
                var inline2504 bool = byte__52 == inline2503
                jp1328 = inline2504
            }
            var jp1325 bool
            if jp1328 {
                jp1325 = true
            } else {
                var inline2506 uint8 = 12
                var inline2507 bool = byte__52 == inline2506
                jp1325 = inline2507
            }
            var jp1322 bool
            if jp1325 {
                jp1322 = true
            } else {
                var inline2509 uint8 = 13
                var inline2510 bool = byte__52 == inline2509
                jp1322 = inline2510
            }
            var jp1289 bool
            if jp1322 {
                jp1289 = true
            } else {
                var t1323 bool = byte__52 < 32
                jp1289 = t1323
            }
            if jp1289 {
                var t1318 bool = start__50 < for_item88
                if t1318 {
                    var t1319 string
                    var inline2512 string = string_byte_slice(value__49, start__50, for_item88)
                    t1319 = inline2512
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1319)
                } else {}
                var t1293 bool
                var inline2544 uint8 = 34
                var inline2545 bool = byte__52 == inline2544
                t1293 = inline2545
                if t1293 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\"")
                } else {
                    var t1296 bool
                    var inline2541 uint8 = 92
                    var inline2542 bool = byte__52 == inline2541
                    t1296 = inline2542
                    if t1296 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\\")
                    } else {
                        var t1299 bool
                        var inline2538 uint8 = 8
                        var inline2539 bool = byte__52 == inline2538
                        t1299 = inline2539
                        if t1299 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\b")
                        } else {
                            var t1302 bool
                            var inline2535 uint8 = 9
                            var inline2536 bool = byte__52 == inline2535
                            t1302 = inline2536
                            if t1302 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\t")
                            } else {
                                var t1305 bool
                                var inline2532 uint8 = 10
                                var inline2533 bool = byte__52 == inline2532
                                t1305 = inline2533
                                if t1305 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\n")
                                } else {
                                    var t1308 bool
                                    var inline2529 uint8 = 12
                                    var inline2530 bool = byte__52 == inline2529
                                    t1308 = inline2530
                                    if t1308 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\f")
                                    } else {
                                        var t1311 bool
                                        var inline2526 uint8 = 13
                                        var inline2527 bool = byte__52 == inline2526
                                        t1311 = inline2527
                                        if t1311 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\u00")
                                            var t1313 uint8 = byte__52 / 16
                                            var t1314 rune
                                            var inline2523 int = int(uint8(t1313))
                                            var inline2524 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline2523)
                                            t1314 = inline2524
                                            var inline2520 string = _goml_m_inherent_i_char_i_char_i_to__string(t1314)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline2520)
                                            var t1315_rhs uint8 = 16
                                            var t1315 uint8 = byte__52 % t1315_rhs
                                            var t1316 rune
                                            var inline2517 int = int(uint8(t1315))
                                            var inline2518 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline2517)
                                            t1316 = inline2518
                                            var inline2514 string = _goml_m_inherent_i_char_i_char_i_to__string(t1316)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline2514)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1292 int = for_item88 + 1
                start__50 = t1292
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop1284
        }
    }
    var t1279 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    var t1280 bool = start__50 < t1279
    if t1280 {
        var t1281 int
        var inline2554 int = _goml_runtime_core_string_len(value__49)
        t1281 = inline2554
        var t1282 string
        var inline2552 string = string_byte_slice(value__49, start__50, t1281)
        t1282 = inline2552
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1282)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__53 _goml_m_std_p_text_p_StringBuilder, value__54 _goml_m_std_p_json_p_Value) struct{} {
    switch value__54.(type) {
    case Object:
        var x97 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__54.(Object)._0
        var inline2568 rune = 123
        var inline2569 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2568)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2569)
        var index__56 int = 0
        var for_limit104 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97)
        var for_index105 int = 0
        Loop_loop1345:
        for {
            var t1346 bool = for_index105 < for_limit104
            if t1346 {
                var for_item106 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97, for_index105)
                var t1347 int = for_index105 + 1
                for_index105 = t1347
                var t1353 bool = index__56 > 0
                if t1353 {
                    var inline2556 rune = 44
                    var inline2557 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2556)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2557)
                } else {}
                var t1349 string = for_item106._0
                _goml_m_std_p_json_p_write__json__string(builder__53, t1349)
                var inline2560 rune = 58
                var inline2561 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2560)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2561)
                var t1350 _goml_m_std_p_json_p_Value = for_item106._1
                _goml_m_std_p_json_p_write__json__value(builder__53, t1350)
                var compound_old112 int = index__56
                var compound_value113 int = 1
                var t1351 int = compound_old112 + compound_value113
                index__56 = t1351
                continue
            } else {
                break Loop_loop1345
            }
        }
        var inline2564 rune = 125
        var inline2565 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2564)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2565)
        return struct{}{}
    case Array:
        var x98 *_goml_vec__goml_m_std_p_json_p_Value = value__54.(Array)._0
        var inline2580 rune = 91
        var inline2581 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2580)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2581)
        var index__59 int = 0
        var for_limit118 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x98)
        var for_index119 int = 0
        Loop_loop1357:
        for {
            var t1358 bool = for_index119 < for_limit118
            if t1358 {
                var for_item120 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x98, for_index119)
                var t1359 int = for_index119 + 1
                for_index119 = t1359
                var t1363 bool = index__59 > 0
                if t1363 {
                    var inline2572 rune = 44
                    var inline2573 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2572)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2573)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__53, for_item120)
                var compound_old124 int = index__59
                var compound_value125 int = 1
                var t1361 int = compound_old124 + compound_value125
                index__59 = t1361
                continue
            } else {
                break Loop_loop1357
            }
        }
        var inline2576 rune = 93
        var inline2577 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2576)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2577)
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
        var jp1368 string
        if x101 {
            jp1368 = "true"
        } else {
            jp1368 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, jp1368)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__64 _goml_m_std_p_json_p_Value) string {
    var builder__65 _goml_m_std_p_text_p_StringBuilder
    var inline2590 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline2591 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline2590,
    }
    builder__65 = inline2591
    _goml_m_std_p_json_p_write__json__value(builder__65, value__64)
    var inline2584 *_goml_vec_uint8 = builder__65.values
    var inline2585 Tuple2_4bool_6string = string_from_utf8(inline2584)
    var inline2587 string = inline2585._1
    return inline2587
}

func _goml_m_std_p_json_p_field(value__66 _goml_m_std_p_json_p_Value, name__67 string) _goml_m_Option____std_p_json_p_Value {
    switch value__66.(type) {
    case Object:
        var x129 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__66.(Object)._0
        var for_limit135 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129)
        var for_index136 int = 0
        Loop_loop1379:
        for {
            var t1380 bool = for_index136 < for_limit135
            if t1380 {
                var for_item137 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129, for_index136)
                var t1381 int = for_index136 + 1
                for_index136 = t1381
                var t1383 string = for_item137._0
                var t1384 bool
                var inline2593 bool = t1383 == name__67
                t1384 = inline2593
                if t1384 {
                    var t1385 _goml_m_std_p_json_p_Value = for_item137._1
                    var t1386 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1385,
                    }
                    return t1386
                } else {
                    continue
                }
            } else {
                break Loop_loop1379
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__72 string) Option__int {
    var t1396 int
    var inline2612 int = _goml_runtime_core_string_len(value__72)
    t1396 = inline2612
    var t1397 bool
    var inline2609 int = 0
    var inline2610 bool = t1396 == inline2609
    t1397 = inline2610
    if t1397 {
        return Option__int_None{}
    } else {
        var t1398 uint8
        var inline2606 int = 0
        var inline2607 uint8 = _goml_runtime_core_string_byte_get(value__72, inline2606)
        t1398 = inline2607
        var negative__73 bool
        var inline2603 uint8 = 45
        var inline2604 bool = t1398 == inline2603
        negative__73 = inline2604
        var jp1400 int
        if negative__73 {
            jp1400 = 1
        } else {
            jp1400 = 0
        }
        var index__74 int = jp1400
        var result__75 int = 0
        var t1421 int
        var inline2601 int = _goml_runtime_core_string_len(value__72)
        t1421 = inline2601
        var t1422 bool
        var inline2599 bool = index__74 == t1421
        t1422 = inline2599
        if t1422 {
            return Option__int_None{}
        } else {
            Loop_loop1407:
            for {
                var t1408 int
                var inline2597 int = _goml_runtime_core_string_len(value__72)
                t1408 = inline2597
                var t1409 bool = index__74 < t1408
                if t1409 {
                    var byte__76 uint8
                    var inline2595 uint8 = _goml_runtime_core_string_byte_get(value__72, index__74)
                    byte__76 = inline2595
                    var t1419 bool = byte__76 < 48
                    var jp1414 bool
                    if t1419 {
                        jp1414 = true
                    } else {
                        var t1420 bool = byte__76 > 57
                        jp1414 = t1420
                    }
                    if jp1414 {
                        return Option__int_None{}
                    } else {
                        var t1415 int = result__75 * 10
                        var t1416 uint8 = byte__76 - 48
                        var t1417 int = int(uint8(t1416))
                        var t1418 int = t1415 + t1417
                        result__75 = t1418
                        var compound_old148 int = index__74
                        var compound_value149 int = 1
                        var t1411 int = compound_old148 + compound_value149
                        index__74 = t1411
                        continue
                    }
                } else {
                    break Loop_loop1407
                }
            }
            var jp1404 int
            if negative__73 {
                var t1406 int = 0 - result__75
                jp1404 = t1406
            } else {
                jp1404 = result__75
            }
            var t1405 Option__int = Option__int_Some{
                _0: jp1404,
            }
            return t1405
        }
    }
}

func main0() struct{} {
    var mtmp177 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1443 _goml_m_std_p_json_p_Value
    switch mtmp177.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x178 _goml_m_std_p_json_p_Value = mtmp177.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1443 = x178
        var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1443, "name")
        switch mtmp181.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline2617 string = "missing name"
            var inline2618 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2617)
            _goml_runtime_core_string_println(inline2618)
            var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1443, "version")
            switch mtmp186.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline2632 string = "missing version"
                var inline2633 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2632)
                _goml_runtime_core_string_println(inline2633)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp188 Option__int
                switch x187.(type) {
                case Number:
                    var inline2643 string = x187.(Number)._0
                    var inline2645 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2643)
                    mtmp188 = inline2645
                default:
                    mtmp188 = Option__int_None{}
                }
                switch mtmp188.(type) {
                case Option__int_None:
                    var inline2636 string = "invalid version"
                    var inline2637 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2636)
                    _goml_runtime_core_string_println(inline2637)
                case Option__int_Some:
                    var x189 int = mtmp188.(Option__int_Some)._0
                    var inline2640 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x189)
                    _goml_runtime_core_string_println(inline2640)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1443, "stable")
            switch mtmp191.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline2647 string = "missing stable"
                var inline2648 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2647)
                _goml_runtime_core_string_println(inline2648)
                var t1447 string = _goml_m_std_p_json_p_encode(jp1443)
                println__T_string(t1447)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field2807 bool
                switch x192.(type) {
                case Bool:
                    var inline2658 bool = x192.(Bool)._0
                    commute_field2807 = inline2658
                    var inline2655 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2807)
                    _goml_runtime_core_string_println(inline2655)
                    var t1447 string = _goml_m_std_p_json_p_encode(jp1443)
                    println__T_string(t1447)
                    return struct{}{}
                default:
                    var inline2651 string = "invalid stable"
                    var inline2652 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2651)
                    _goml_runtime_core_string_println(inline2652)
                    var t1447 string = _goml_m_std_p_json_p_encode(jp1443)
                    println__T_string(t1447)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field2813 string
            switch x182.(type) {
            case String:
                var inline2628 string = x182.(String)._0
                commute_field2813 = inline2628
                var inline2625 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field2813)
                _goml_runtime_core_string_println(inline2625)
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1443, "version")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2632 string = "missing version"
                    var inline2633 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2632)
                    _goml_runtime_core_string_println(inline2633)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp188 Option__int
                    switch x187.(type) {
                    case Number:
                        var inline2643 string = x187.(Number)._0
                        var inline2645 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2643)
                        mtmp188 = inline2645
                    default:
                        mtmp188 = Option__int_None{}
                    }
                    switch mtmp188.(type) {
                    case Option__int_None:
                        var inline2636 string = "invalid version"
                        var inline2637 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2636)
                        _goml_runtime_core_string_println(inline2637)
                    case Option__int_Some:
                        var x189 int = mtmp188.(Option__int_Some)._0
                        var inline2640 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x189)
                        _goml_runtime_core_string_println(inline2640)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1443, "stable")
                switch mtmp191.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2647 string = "missing stable"
                    var inline2648 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2647)
                    _goml_runtime_core_string_println(inline2648)
                    var t1447 string = _goml_m_std_p_json_p_encode(jp1443)
                    println__T_string(t1447)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field2807 bool
                    switch x192.(type) {
                    case Bool:
                        var inline2658 bool = x192.(Bool)._0
                        commute_field2807 = inline2658
                        var inline2655 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2807)
                        _goml_runtime_core_string_println(inline2655)
                        var t1447 string = _goml_m_std_p_json_p_encode(jp1443)
                        println__T_string(t1447)
                        return struct{}{}
                    default:
                        var inline2651 string = "invalid stable"
                        var inline2652 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2651)
                        _goml_runtime_core_string_println(inline2652)
                        var t1447 string = _goml_m_std_p_json_p_encode(jp1443)
                        println__T_string(t1447)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline2621 string = "invalid name"
                var inline2622 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2621)
                _goml_runtime_core_string_println(inline2622)
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1443, "version")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2632 string = "missing version"
                    var inline2633 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2632)
                    _goml_runtime_core_string_println(inline2633)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp188 Option__int
                    switch x187.(type) {
                    case Number:
                        var inline2643 string = x187.(Number)._0
                        var inline2645 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2643)
                        mtmp188 = inline2645
                    default:
                        mtmp188 = Option__int_None{}
                    }
                    switch mtmp188.(type) {
                    case Option__int_None:
                        var inline2636 string = "invalid version"
                        var inline2637 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2636)
                        _goml_runtime_core_string_println(inline2637)
                    case Option__int_Some:
                        var x189 int = mtmp188.(Option__int_Some)._0
                        var inline2640 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x189)
                        _goml_runtime_core_string_println(inline2640)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1443, "stable")
                switch mtmp191.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2647 string = "missing stable"
                    var inline2648 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2647)
                    _goml_runtime_core_string_println(inline2648)
                    var t1447 string = _goml_m_std_p_json_p_encode(jp1443)
                    println__T_string(t1447)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field2807 bool
                    switch x192.(type) {
                    case Bool:
                        var inline2658 bool = x192.(Bool)._0
                        commute_field2807 = inline2658
                        var inline2655 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2807)
                        _goml_runtime_core_string_println(inline2655)
                        var t1447 string = _goml_m_std_p_json_p_encode(jp1443)
                        println__T_string(t1447)
                        return struct{}{}
                    default:
                        var inline2651 string = "invalid stable"
                        var inline2652 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2651)
                        _goml_runtime_core_string_println(inline2652)
                        var t1447 string = _goml_m_std_p_json_p_encode(jp1443)
                        println__T_string(t1447)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            panic("non-exhaustive match")
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x179 string = mtmp177.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var inline2614 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x179)
        _goml_runtime_core_string_println(inline2614)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t1463 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t1463
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1503:
    for {
        var t1504 int
        var inline2675 int = _goml_runtime_core_string_len(x12)
        t1504 = inline2675
        var t1505 bool = index__26 < t1504
        if t1505 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t1507 int = compound_old17 + x16
                index__26 = t1507
                continue
            } else {
                var t1509 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1509
            }
        } else {
            break Loop_loop1503
        }
    }
    var t1502 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t1502
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t1512 int = _goml_runtime_core_string_len(self__38)
    return t1512
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t1515 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t1515
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__36 rune) string {
    var inline2677 uint32 = uint32(rune(self__36))
    var inline2678 bool = utf8_valid_scalar(inline2677)
    if inline2678 {
        var inline2679 string = _goml_runtime_core_char_to_string(self__36)
        return inline2679
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__98 uint8, other__99 uint8) bool {
    var t1521 bool = self__98 == other__99
    return t1521
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline2682 bool = string_is_char_boundary(self__43, start__44)
    var inline2684 bool
    if inline2682 {
        var inline2687 bool = string_is_char_boundary(self__43, end__45)
        inline2684 = inline2687
    } else {
        inline2684 = false
    }
    if inline2684 {
        var inline2685 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline2685
    } else {
        var inline2686 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline2686
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__236 int) *ref_int_x {
    var t1556 *ref_int_x = ref__Ref_3int(value__236)
    return t1556
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__237 *ref_int_x) int {
    var t1559 int = ref_get__Ref_3int(self__237)
    return t1559
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t1562 string = _goml_runtime_core_int_to_string(self__34)
    return t1562
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__238 *ref_int_x, value__239 int) struct{} {
    ref_set__Ref_3int(self__238, value__239)
    return struct{}{}
}

func char_from_uint32(value__32 uint32) Option__char {
    var t1569 bool
    var inline2699 bool = value__32 <= 1114111
    if inline2699 {
        var inline2700 bool = value__32 >= 55296
        var inline2702 bool
        if inline2700 {
            var inline2704 bool = value__32 <= 57343
            inline2702 = inline2704
        } else {
            inline2702 = false
        }
        var inline2703 bool = !inline2702
        t1569 = inline2703
    } else {
        t1569 = false
    }
    if t1569 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t1570 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1570
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t1573 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t1573
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__155 *_goml_vec__goml_m_std_p_json_p_Value, elem__156 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__155, elem__156)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t1578 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t1578
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__155 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__156 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__155, elem__156)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline2706 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline2707 bool = inline2706._0
    var inline2708 rune = inline2706._1
    if inline2707 {
        return inline2708
    } else {
        var inline2712 rune = _goml_runtime_core_string_get("", -1)
        return inline2712
    }
}

func println__T_string(value__31 string) struct{} {
    var t1585 string
    t1585 = value__31
    _goml_runtime_core_string_println(t1585)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1711 bool = index__6 < 0
    var jp1709 bool
    if t1711 {
        jp1709 = true
    } else {
        var t1712 bool = index__6 >= length__7
        jp1709 = t1712
    }
    if jp1709 {
        var inline2719 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2719
    } else {
        var t1596 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1596))
        var t1599 bool = first__8 < 128
        if t1599 {
            var inline2721 int = 1
            var inline2722 Option__char = char_from_uint32(first__8)
            switch inline2722.(type) {
            case Option__char_None:
                var inline2723 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline2723
            case Option__char_Some:
                var inline2724 rune = inline2722.(Option__char_Some)._0
                var inline2726 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline2724,
                    _2: inline2721,
                }
                return inline2726
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1603 bool = first__8 < 194
            if t1603 {
                var inline2728 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline2728
            } else {
                var t1607 bool = first__8 < 224
                if t1607 {
                    var t1620 int = length__7 - index__6
                    var t1621 bool = t1620 < 2
                    if t1621 {
                        var inline2730 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline2730
                    } else {
                        var t1609 int = index__6 + 1
                        var t1610 uint8
                        var inline2744 uint8 = _goml_runtime_core_string_byte_get(value__5, t1609)
                        t1610 = inline2744
                        var second__9 uint32 = uint32(uint8(t1610))
                        var t1613 bool
                        var inline2741 bool = second__9 < 128
                        if inline2741 {
                            t1613 = true
                        } else {
                            var inline2742 bool = second__9 > 191
                            t1613 = inline2742
                        }
                        if t1613 {
                            var inline2732 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2732
                        } else {
                            var t1615_rhs uint32 = 31
                            var t1615 uint32 = first__8 & t1615_rhs
                            var t1616_rhs int = 6
                            var t1616 uint32 = t1615 << t1616_rhs
                            var t1617_rhs uint32 = 63
                            var t1617 uint32 = second__9 & t1617_rhs
                            var t1618 uint32 = t1616 | t1617
                            var inline2734 int = 2
                            var inline2735 Option__char = char_from_uint32(t1618)
                            switch inline2735.(type) {
                            case Option__char_None:
                                var inline2736 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline2736
                            case Option__char_Some:
                                var inline2737 rune = inline2735.(Option__char_Some)._0
                                var inline2739 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline2737,
                                    _2: inline2734,
                                }
                                return inline2739
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1625 bool = first__8 < 240
                    if t1625 {
                        var t1658 int = length__7 - index__6
                        var t1659 bool = t1658 < 3
                        if t1659 {
                            var inline2746 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2746
                        } else {
                            var t1627 int = index__6 + 1
                            var t1628 uint8
                            var inline2761 uint8 = _goml_runtime_core_string_byte_get(value__5, t1627)
                            t1628 = inline2761
                            var second__10 uint32 = uint32(uint8(t1628))
                            var t1629 int = index__6 + 2
                            var t1630 uint8
                            var inline2759 uint8 = _goml_runtime_core_string_byte_get(value__5, t1629)
                            t1630 = inline2759
                            var third__11 uint32 = uint32(uint8(t1630))
                            var t1656 bool = utf8_invalid_continuation(second__10)
                            var jp1651 bool
                            if t1656 {
                                jp1651 = true
                            } else {
                                var inline2748 bool = third__11 < 128
                                if inline2748 {
                                    jp1651 = true
                                } else {
                                    var inline2749 bool = third__11 > 191
                                    jp1651 = inline2749
                                }
                            }
                            var jp1645 bool
                            if jp1651 {
                                jp1645 = true
                            } else {
                                var t1654 bool
                                var inline2751 uint32 = 224
                                var inline2752 bool = first__8 == inline2751
                                t1654 = inline2752
                                if t1654 {
                                    var t1655 bool = second__10 < 160
                                    jp1645 = t1655
                                } else {
                                    jp1645 = false
                                }
                            }
                            var jp1634 bool
                            if jp1645 {
                                jp1634 = true
                            } else {
                                var t1648 bool
                                var inline2754 uint32 = 237
                                var inline2755 bool = first__8 == inline2754
                                t1648 = inline2755
                                if t1648 {
                                    var t1649 bool = second__10 >= 160
                                    jp1634 = t1649
                                } else {
                                    jp1634 = false
                                }
                            }
                            if jp1634 {
                                var inline2757 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2757
                            } else {
                                var t1636_rhs uint32 = 15
                                var t1636 uint32 = first__8 & t1636_rhs
                                var t1637_rhs int = 12
                                var t1637 uint32 = t1636 << t1637_rhs
                                var t1638_rhs uint32 = 63
                                var t1638 uint32 = second__10 & t1638_rhs
                                var t1639_rhs int = 6
                                var t1639 uint32 = t1638 << t1639_rhs
                                var t1640 uint32 = t1637 | t1639
                                var t1641_rhs uint32 = 63
                                var t1641 uint32 = third__11 & t1641_rhs
                                var t1642 uint32 = t1640 | t1641
                                var t1643 Tuple3_4bool_4char_3int = utf8_valid_decode(t1642, 3)
                                return t1643
                            }
                        }
                    } else {
                        var t1663 bool = first__8 < 245
                        if t1663 {
                            var t1704 int = length__7 - index__6
                            var t1705 bool = t1704 < 4
                            if t1705 {
                                var t1706 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1706
                            } else {
                                var t1665 int = index__6 + 1
                                var t1666 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1665)
                                var second__12 uint32 = uint32(uint8(t1666))
                                var t1667 int = index__6 + 2
                                var t1668 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1667)
                                var third__13 uint32 = uint32(uint8(t1668))
                                var t1669 int = index__6 + 3
                                var t1670 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1669)
                                var fourth__14 uint32 = uint32(uint8(t1670))
                                var t1702 bool = utf8_invalid_continuation(second__12)
                                var jp1700 bool
                                if t1702 {
                                    jp1700 = true
                                } else {
                                    var t1703 bool = utf8_invalid_continuation(third__13)
                                    jp1700 = t1703
                                }
                                var jp1694 bool
                                if jp1700 {
                                    jp1694 = true
                                } else {
                                    var t1701 bool = utf8_invalid_continuation(fourth__14)
                                    jp1694 = t1701
                                }
                                var jp1688 bool
                                if jp1694 {
                                    jp1688 = true
                                } else {
                                    var t1697 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t1697 {
                                        var t1698 bool = second__12 < 144
                                        jp1688 = t1698
                                    } else {
                                        jp1688 = false
                                    }
                                }
                                var jp1674 bool
                                if jp1688 {
                                    jp1674 = true
                                } else {
                                    var t1691 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t1691 {
                                        var t1692 bool = second__12 > 143
                                        jp1674 = t1692
                                    } else {
                                        jp1674 = false
                                    }
                                }
                                if jp1674 {
                                    var t1675 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1675
                                } else {
                                    var t1676_rhs uint32 = 7
                                    var t1676 uint32 = first__8 & t1676_rhs
                                    var t1677_rhs int = 18
                                    var t1677 uint32 = t1676 << t1677_rhs
                                    var t1678_rhs uint32 = 63
                                    var t1678 uint32 = second__12 & t1678_rhs
                                    var t1679_rhs int = 12
                                    var t1679 uint32 = t1678 << t1679_rhs
                                    var t1680 uint32 = t1677 | t1679
                                    var t1681_rhs uint32 = 63
                                    var t1681 uint32 = third__13 & t1681_rhs
                                    var t1682_rhs int = 6
                                    var t1682 uint32 = t1681 << t1682_rhs
                                    var t1683 uint32 = t1680 | t1682
                                    var t1684_rhs uint32 = 63
                                    var t1684 uint32 = fourth__14 & t1684_rhs
                                    var t1685 uint32 = t1683 | t1684
                                    var t1686 Tuple3_4bool_4char_3int = utf8_valid_decode(t1685, 4)
                                    return t1686
                                }
                            }
                        } else {
                            var t1707 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1707
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t1717 uint32 = uint32(rune(value__29))
    var t1718 bool
    var inline2763 bool = t1717 <= 1114111
    if inline2763 {
        var inline2764 bool = t1717 >= 55296
        var inline2766 bool
        if inline2764 {
            var inline2768 bool = t1717 <= 57343
            inline2766 = inline2768
        } else {
            inline2766 = false
        }
        var inline2767 bool = !inline2766
        t1718 = inline2767
    } else {
        t1718 = false
    }
    if t1718 {
        var t1719 string = _goml_runtime_core_char_to_string(value__29)
        return t1719
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t1734 bool = index__16 < 0
    var jp1725 bool
    if t1734 {
        jp1725 = true
    } else {
        var t1735 int
        var inline2770 int = _goml_runtime_core_string_len(value__15)
        t1735 = inline2770
        var t1736 bool = index__16 > t1735
        jp1725 = t1736
    }
    if jp1725 {
        return false
    } else {
        var t1728 int
        var inline2779 int = _goml_runtime_core_string_len(value__15)
        t1728 = inline2779
        var t1729 bool
        var inline2777 bool = index__16 == t1728
        t1729 = inline2777
        if t1729 {
            return true
        } else {
            var t1730 uint8
            var inline2775 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1730 = inline2775
            var t1731_rhs uint8 = 192
            var t1731 uint8 = t1730 & t1731_rhs
            var t1732 bool
            var inline2772 uint8 = 128
            var inline2773 bool = t1731 == inline2772
            t1732 = inline2773
            var t1733 bool = !t1732
            return t1733
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t1745 bool = string_is_char_boundary(value__21, start__22)
    var jp1742 bool
    if t1745 {
        var t1746 bool = string_is_char_boundary(value__21, end__23)
        jp1742 = t1746
    } else {
        jp1742 = false
    }
    if jp1742 {
        var t1743 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t1743
    } else {
        var t1744 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t1744
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1753 bool = value__4 <= 1114111
    if t1753 {
        var t1757 bool = value__4 >= 55296
        var jp1755 bool
        if t1757 {
            var t1758 bool = value__4 <= 57343
            jp1755 = t1758
        } else {
            jp1755 = false
        }
        var t1756 bool = !jp1755
        return t1756
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t1768 string = _goml_runtime_core_int_to_string(self__69)
    return t1768
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t1771 string = _goml_runtime_core_bool_to_string(self__66)
    return t1771
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1774 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1774
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field2816 rune
    var inline2783 bool = utf8_valid_scalar(value__0)
    if inline2783 {
        var inline2784 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline2786 rune = inline2784._1
        commute_field2816 = inline2786
        var t1780 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field2816,
            _2: width__1,
        }
        return t1780
    } else {
        var inline2781 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2781
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1785 bool = value__3 < 128
    if t1785 {
        return true
    } else {
        var t1786 bool = value__3 > 191
        return t1786
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t1789 bool = self__102 == other__103
    return t1789
}

func main() {
    main0()
}
