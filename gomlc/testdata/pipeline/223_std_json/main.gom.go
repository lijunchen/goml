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
    var inline1799 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline1799
    var t260 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t260
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline1814 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline1814
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
            var inline1810 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t280 = inline1810
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
    var inline1816 string = char_to_string(value__8)
    t283 = inline1816
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t283)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var t533 string = "" + message__2
    var t534 string = t533 + " at byte "
    var t535 *ref_int_x = value__1.index
    var t536 int
    var inline2027 int = ref_get__Ref_3int(t535)
    t536 = inline2027
    var t537 string
    var inline2025 string = _goml_runtime_core_int_to_string(t536)
    t537 = inline2025
    var t538 string = t534 + t537
    return t538
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop553:
    for {
        var t561 *ref_int_x = value__4.index
        var t562 int
        var inline2060 int = ref_get__Ref_3int(t561)
        t562 = inline2060
        var t563 string = value__4.input
        var t564 int
        var inline2058 int = _goml_runtime_core_string_len(t563)
        t564 = inline2058
        var t565 bool = t562 < t564
        var jp555 bool
        if t565 {
            var t566 string = value__4.input
            var t567 *ref_int_x = value__4.index
            var t568 int
            var inline2052 int = ref_get__Ref_3int(t567)
            t568 = inline2052
            var t569 uint8
            var inline2050 uint8 = _goml_runtime_core_string_byte_get(t566, t568)
            t569 = inline2050
            var inline2041 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t569, 9)
            var inline2043 bool
            if inline2041 {
                inline2043 = true
            } else {
                var inline2048 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t569, 10)
                inline2043 = inline2048
            }
            var inline2045 bool
            if inline2043 {
                inline2045 = true
            } else {
                var inline2047 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t569, 13)
                inline2045 = inline2047
            }
            if inline2045 {
                jp555 = true
            } else {
                var inline2046 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t569, 32)
                jp555 = inline2046
            }
        } else {
            jp555 = false
        }
        if jp555 {
            var t556 *ref_int_x = value__4.index
            var t557 *ref_int_x = value__4.index
            var t558 int
            var inline2056 int = ref_get__Ref_3int(t557)
            t558 = inline2056
            var t559 int = t558 + 1
            ref_set__Ref_3int(t556, t559)
            continue
        } else {
            break Loop_loop553
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var t600 bool = value__5 >= 48
    var jp576 bool
    if t600 {
        var t601 bool = value__5 <= 57
        jp576 = t601
    } else {
        jp576 = false
    }
    if jp576 {
        var t577 uint8 = value__5 - 48
        var t578 uint32 = uint32(uint8(t577))
        var t579 Option__uint32 = Option__uint32_Some{
            _0: t578,
        }
        return t579
    } else {
        var t598 bool = value__5 >= 65
        var jp583 bool
        if t598 {
            var t599 bool = value__5 <= 70
            jp583 = t599
        } else {
            jp583 = false
        }
        if jp583 {
            var t584 uint8 = value__5 - 65
            var t585 uint8 = t584 + 10
            var t586 uint32 = uint32(uint8(t585))
            var t587 Option__uint32 = Option__uint32_Some{
                _0: t586,
            }
            return t587
        } else {
            var t596 bool = value__5 >= 97
            var jp591 bool
            if t596 {
                var t597 bool = value__5 <= 102
                jp591 = t597
            } else {
                jp591 = false
            }
            if jp591 {
                var t592 uint8 = value__5 - 97
                var t593 uint8 = t592 + 10
                var t594 uint32 = uint32(uint8(t593))
                var t595 Option__uint32 = Option__uint32_Some{
                    _0: t594,
                }
                return t595
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t606 *ref_int_x = value__6.index
    var t607 int
    var inline2088 int = ref_get__Ref_3int(t606)
    t607 = inline2088
    var t608 int = t607 + 4
    var t609 string = value__6.input
    var t610 int
    var inline2086 int = _goml_runtime_core_string_len(t609)
    t610 = inline2086
    var t611 bool = t608 > t610
    if t611 {
        var t612 string
        var inline2062 string = "incomplete unicode escape"
        var inline2063 string = "" + inline2062
        var inline2064 string = inline2063 + " at byte "
        var inline2065 *ref_int_x = value__6.index
        var inline2066 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2065)
        var inline2067 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2066)
        var inline2068 string = inline2064 + inline2067
        t612 = inline2068
        var t613 Result__uint32__string = Result__uint32__string_Err{
            _0: t612,
        }
        return t613
    } else {
        var result__7_source int = 0
        var result__7 uint32 = uint32(int(result__7_source))
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop620:
        for {
            var t621 bool = for_index0 < for_limit1
            if t621 {
                var for_item2 int = for_index0
                var t622 int = for_index0 + 1
                for_index0 = t622
                var t623 string = value__6.input
                var t624 *ref_int_x = value__6.index
                var t625 int
                var inline2080 int = ref_get__Ref_3int(t624)
                t625 = inline2080
                var t626 int = t625 + for_item2
                var t627 uint8
                var inline2078 uint8 = _goml_runtime_core_string_byte_get(t623, t626)
                t627 = inline2078
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t627)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t629 string
                    var inline2070 string = "invalid unicode escape"
                    var inline2071 string = "" + inline2070
                    var inline2072 string = inline2071 + " at byte "
                    var inline2073 *ref_int_x = value__6.index
                    var inline2074 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2073)
                    var inline2075 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2074)
                    var inline2076 string = inline2072 + inline2075
                    t629 = inline2076
                    var t630 Result__uint32__string = Result__uint32__string_Err{
                        _0: t629,
                    }
                    return t630
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var t631 uint32 = result__7 * 16
                    var t632 uint32 = t631 + x5
                    result__7 = t632
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop620
            }
        }
        var t615 *ref_int_x = value__6.index
        var t616 *ref_int_x = value__6.index
        var t617 int
        var inline2084 int = ref_get__Ref_3int(t616)
        t617 = inline2084
        var t618 int = t617 + 4
        ref_set__Ref_3int(t615, t618)
        var t619 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__7,
        }
        return t619
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var commute_field2763 rune
    var inline2101 bool = utf8_valid_scalar(codepoint__12)
    if inline2101 {
        var inline2102 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__12)
        var inline2104 rune = inline2102._1
        commute_field2763 = inline2104
        var inline2098 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field2763)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__11, inline2098)
        var t639 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t639
    } else {
        var t637 string
        var inline2090 string = "invalid unicode codepoint"
        var inline2091 string = "" + inline2090
        var inline2092 string = inline2091 + " at byte "
        var inline2093 *ref_int_x = value__10.index
        var inline2094 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2093)
        var inline2095 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2094)
        var inline2096 string = inline2092 + inline2095
        t637 = inline2096
        var t638 Result__unit__string = Result__unit__string_Err{
            _0: t637,
        }
        return t638
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp12 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp643 uint32
    switch mtmp12.(type) {
    case Result__uint32__string_Ok:
        var x13 uint32 = mtmp12.(Result__uint32__string_Ok)._0
        jp643 = x13
        var t705 bool = jp643 >= 55296
        var jp647 bool
        if t705 {
            var t706 bool = jp643 <= 56319
            jp647 = t706
        } else {
            jp647 = false
        }
        if jp647 {
            var t684 *ref_int_x = value__14.index
            var t685 int
            var inline2152 int = ref_get__Ref_3int(t684)
            t685 = inline2152
            var t686 int = t685 + 2
            var t687 string = value__14.input
            var t688 int
            var inline2150 int = _goml_runtime_core_string_len(t687)
            t688 = inline2150
            var t689 bool = t686 > t688
            var jp676 bool
            if t689 {
                jp676 = true
            } else {
                var t690 string = value__14.input
                var t691 *ref_int_x = value__14.index
                var t692 int
                var inline2113 int = ref_get__Ref_3int(t691)
                t692 = inline2113
                var t693 uint8
                var inline2111 uint8 = _goml_runtime_core_string_byte_get(t690, t692)
                t693 = inline2111
                var t694 bool
                var inline2108 uint8 = 92
                var inline2109 bool = t693 == inline2108
                t694 = inline2109
                var t695 bool = !t694
                jp676 = t695
            }
            var jp651 bool
            if jp676 {
                jp651 = true
            } else {
                var t677 string = value__14.input
                var t678 *ref_int_x = value__14.index
                var t679 int
                var inline2120 int = ref_get__Ref_3int(t678)
                t679 = inline2120
                var t680 int = t679 + 1
                var t681 uint8
                var inline2118 uint8 = _goml_runtime_core_string_byte_get(t677, t680)
                t681 = inline2118
                var t682 bool
                var inline2115 uint8 = 117
                var inline2116 bool = t681 == inline2115
                t682 = inline2116
                var t683 bool = !t682
                jp651 = t683
            }
            if jp651 {
                var t652 string
                var inline2122 string = "missing low surrogate"
                var inline2123 string = "" + inline2122
                var inline2124 string = inline2123 + " at byte "
                var inline2125 *ref_int_x = value__14.index
                var inline2126 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2125)
                var inline2127 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2126)
                var inline2128 string = inline2124 + inline2127
                t652 = inline2128
                var t653 Result__unit__string = Result__unit__string_Err{
                    _0: t652,
                }
                return t653
            } else {
                var t654 *ref_int_x = value__14.index
                var t655 *ref_int_x = value__14.index
                var t656 int
                var inline2148 int = ref_get__Ref_3int(t655)
                t656 = inline2148
                var t657 int = t656 + 2
                ref_set__Ref_3int(t654, t657)
                var mtmp16 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp659 uint32
                switch mtmp16.(type) {
                case Result__uint32__string_Ok:
                    var x17 uint32 = mtmp16.(Result__uint32__string_Ok)._0
                    jp659 = x17
                    var t672 bool = jp659 < 56320
                    var jp663 bool
                    if t672 {
                        jp663 = true
                    } else {
                        var t673 bool = jp659 > 57343
                        jp663 = t673
                    }
                    if jp663 {
                        var t664 string
                        var inline2130 string = "invalid low surrogate"
                        var inline2131 string = "" + inline2130
                        var inline2132 string = inline2131 + " at byte "
                        var inline2133 *ref_int_x = value__14.index
                        var inline2134 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2133)
                        var inline2135 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2134)
                        var inline2136 string = inline2132 + inline2135
                        t664 = inline2136
                        var t665 Result__unit__string = Result__unit__string_Err{
                            _0: t664,
                        }
                        return t665
                    } else {
                        var t666 uint32 = jp643 - 55296
                        var t667 uint32 = t666 * 1024
                        var t668 uint32 = 65536 + t667
                        var t669 uint32 = t668 + jp659
                        var t670 uint32 = t669 - 56320
                        var inline2138 Option__char = char_from_uint32(t670)
                        switch inline2138.(type) {
                        case Option__char_None:
                            var inline2139 string = _goml_m_std_p_json_p_json__error(value__14, "invalid unicode codepoint")
                            var inline2140 Result__unit__string = Result__unit__string_Err{
                                _0: inline2139,
                            }
                            return inline2140
                        case Option__char_Some:
                            var inline2141 rune = inline2138.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__15, inline2141)
                            var inline2144 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline2144
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x18 string = mtmp16.(Result__uint32__string_Err)._0
                    var t674 Result__unit__string = Result__unit__string_Err{
                        _0: x18,
                    }
                    return t674
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t703 bool = jp643 >= 56320
            var jp699 bool
            if t703 {
                var t704 bool = jp643 <= 57343
                jp699 = t704
            } else {
                jp699 = false
            }
            if jp699 {
                var t700 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t701 Result__unit__string = Result__unit__string_Err{
                    _0: t700,
                }
                return t701
            } else {
                var t702 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, jp643)
                return t702
            }
        }
    case Result__uint32__string_Err:
        var x14 string = mtmp12.(Result__uint32__string_Err)._0
        var t707 Result__unit__string = Result__unit__string_Err{
            _0: x14,
        }
        return t707
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__18 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t823 *ref_int_x = value__18.index
    var t824 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t823)
    var t825 string = value__18.input
    var t826 int = _goml_m_inherent_i_string_i_string_i_byte__len(t825)
    var t827 bool = t824 >= t826
    var jp815 bool
    if t827 {
        jp815 = true
    } else {
        var t828 string = value__18.input
        var t829 *ref_int_x = value__18.index
        var t830 int
        var inline2159 int = ref_get__Ref_3int(t829)
        t830 = inline2159
        var t831 uint8
        var inline2157 uint8 = _goml_runtime_core_string_byte_get(t828, t830)
        t831 = inline2157
        var t832 bool
        var inline2154 uint8 = 34
        var inline2155 bool = t831 == inline2154
        t832 = inline2155
        var t833 bool = !t832
        jp815 = t833
    }
    if jp815 {
        var t816 string
        var inline2161 string = "expected string"
        var inline2162 string = "" + inline2161
        var inline2163 string = inline2162 + " at byte "
        var inline2164 *ref_int_x = value__18.index
        var inline2165 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2164)
        var inline2166 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2165)
        var inline2167 string = inline2163 + inline2166
        t816 = inline2167
        var t817 Result__string__string = Result__string__string_Err{
            _0: t816,
        }
        return t817
    } else {
        var t818 *ref_int_x = value__18.index
        var t819 *ref_int_x = value__18.index
        var t820 int
        var inline2171 int = ref_get__Ref_3int(t819)
        t820 = inline2171
        var t821 int = t820 + 1
        ref_set__Ref_3int(t818, t821)
        var builder__19 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t711 *ref_int_x = value__18.index
        var segment__20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t711)
        Loop_loop715:
        for {
            var t716 *ref_int_x = value__18.index
            var t717 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t716)
            var t718 string = value__18.input
            var t719 int = _goml_m_inherent_i_string_i_string_i_byte__len(t718)
            var t720 bool = t717 < t719
            if t720 {
                var t721 string = value__18.input
                var t722 *ref_int_x = value__18.index
                var t723 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t722)
                var byte__21 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t721, t723)
                var t725 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 34)
                if t725 {
                    var t733 *ref_int_x = value__18.index
                    var t734 int
                    var inline2187 int = ref_get__Ref_3int(t733)
                    t734 = inline2187
                    var t735 bool = segment__20 < t734
                    if t735 {
                        var t736 string = value__18.input
                        var t737 *ref_int_x = value__18.index
                        var t738 int
                        var inline2175 int = ref_get__Ref_3int(t737)
                        t738 = inline2175
                        var t739 string
                        var inline2173 string = string_byte_slice(t736, segment__20, t738)
                        t739 = inline2173
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t739)
                    } else {}
                    var t727 *ref_int_x = value__18.index
                    var t728 *ref_int_x = value__18.index
                    var t729 int
                    var inline2185 int = ref_get__Ref_3int(t728)
                    t729 = inline2185
                    var t730 int = t729 + 1
                    ref_set__Ref_3int(t727, t730)
                    var t731 string
                    var inline2177 *_goml_vec_uint8 = builder__19.values
                    var inline2178 Tuple2_4bool_6string = string_from_utf8(inline2177)
                    var inline2180 string = inline2178._1
                    t731 = inline2180
                    var t732 Result__string__string = Result__string__string_Ok{
                        _0: t731,
                    }
                    return t732
                } else {
                    var t742 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 92)
                    if t742 {
                        var t797 *ref_int_x = value__18.index
                        var t798 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t797)
                        var t799 bool = segment__20 < t798
                        if t799 {
                            var t800 string = value__18.input
                            var t801 *ref_int_x = value__18.index
                            var t802 int
                            var inline2191 int = ref_get__Ref_3int(t801)
                            t802 = inline2191
                            var t803 string
                            var inline2189 string = string_byte_slice(t800, segment__20, t802)
                            t803 = inline2189
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t803)
                        } else {}
                        var t744 *ref_int_x = value__18.index
                        var t745 *ref_int_x = value__18.index
                        var t746 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t745)
                        var t747 int = t746 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t744, t747)
                        var t790 *ref_int_x = value__18.index
                        var t791 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t790)
                        var t792 string = value__18.input
                        var t793 int = _goml_m_inherent_i_string_i_string_i_byte__len(t792)
                        var t794 bool = t791 >= t793
                        if t794 {
                            var t795 string
                            var inline2193 string = "incomplete escape"
                            var inline2194 string = "" + inline2193
                            var inline2195 string = inline2194 + " at byte "
                            var inline2196 *ref_int_x = value__18.index
                            var inline2197 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2196)
                            var inline2198 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2197)
                            var inline2199 string = inline2195 + inline2198
                            t795 = inline2199
                            var t796 Result__string__string = Result__string__string_Err{
                                _0: t795,
                            }
                            return t796
                        } else {
                            var t749 string = value__18.input
                            var t750 *ref_int_x = value__18.index
                            var t751 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t750)
                            var escape__22 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t749, t751)
                            var t752 *ref_int_x = value__18.index
                            var t753 *ref_int_x = value__18.index
                            var t754 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t753)
                            var t755 int = t754 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t752, t755)
                            var t759 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 34)
                            if t759 {
                                var inline2201 rune = 34
                                var inline2202 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2201)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, inline2202)
                                var t757 *ref_int_x = value__18.index
                                var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
                                segment__20 = t758
                                continue
                            } else {
                                var t762 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 92)
                                if t762 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 92)
                                    var t757 *ref_int_x = value__18.index
                                    var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
                                    segment__20 = t758
                                    continue
                                } else {
                                    var t765 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 47)
                                    if t765 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 47)
                                        var t757 *ref_int_x = value__18.index
                                        var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
                                        segment__20 = t758
                                        continue
                                    } else {
                                        var t768 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 98)
                                        if t768 {
                                            var mtmp26 Option__char = char_from_uint32(8)
                                            switch mtmp26.(type) {
                                            case Option__char_None:
                                                var t757 *ref_int_x = value__18.index
                                                var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
                                                segment__20 = t758
                                                continue
                                            case Option__char_Some:
                                                var x27 rune = mtmp26.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x27)
                                                var t757 *ref_int_x = value__18.index
                                                var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
                                                segment__20 = t758
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t772 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 102)
                                            if t772 {
                                                var mtmp28 Option__char = char_from_uint32(12)
                                                switch mtmp28.(type) {
                                                case Option__char_None:
                                                    var t757 *ref_int_x = value__18.index
                                                    var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
                                                    segment__20 = t758
                                                    continue
                                                case Option__char_Some:
                                                    var x29 rune = mtmp28.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x29)
                                                    var t757 *ref_int_x = value__18.index
                                                    var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
                                                    segment__20 = t758
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t776 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 110)
                                                if t776 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 10)
                                                    var t757 *ref_int_x = value__18.index
                                                    var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
                                                    segment__20 = t758
                                                    continue
                                                } else {
                                                    var t779 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 114)
                                                    if t779 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 13)
                                                        var t757 *ref_int_x = value__18.index
                                                        var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
                                                        segment__20 = t758
                                                        continue
                                                    } else {
                                                        var t782 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 116)
                                                        if t782 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 9)
                                                            var t757 *ref_int_x = value__18.index
                                                            var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
                                                            segment__20 = t758
                                                            continue
                                                        } else {
                                                            var t785 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 117)
                                                            if t785 {
                                                                var mtmp30 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__18, builder__19)
                                                                switch mtmp30.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t757 *ref_int_x = value__18.index
                                                                    var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
                                                                    segment__20 = t758
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x32 string = mtmp30.(Result__unit__string_Err)._0
                                                                    var t787 Result__string__string = Result__string__string_Err{
                                                                        _0: x32,
                                                                    }
                                                                    return t787
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t788 string = _goml_m_std_p_json_p_json__error(value__18, "invalid escape")
                                                                var t789 Result__string__string = Result__string__string_Err{
                                                                    _0: t788,
                                                                }
                                                                return t789
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
                        var t806 bool = byte__21 < 32
                        if t806 {
                            var t807 string = _goml_m_std_p_json_p_json__error(value__18, "unescaped control character")
                            var t808 Result__string__string = Result__string__string_Err{
                                _0: t807,
                            }
                            return t808
                        } else {
                            var t809 *ref_int_x = value__18.index
                            var t810 *ref_int_x = value__18.index
                            var t811 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t810)
                            var t812 int = t811 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t809, t812)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop715
            }
        }
        var t713 string = _goml_m_std_p_json_p_json__error(value__18, "unterminated string")
        var t714 Result__string__string = Result__string__string_Err{
            _0: t713,
        }
        return t714
    }
}

func _goml_m_std_p_json_p_parse__digits(value__26 _goml_m_std_p_json_p_JsonParser) bool {
    var t842 *ref_int_x = value__26.index
    var start__27 int
    var inline2222 int = ref_get__Ref_3int(t842)
    start__27 = inline2222
    Loop_loop847:
    for {
        var t855 *ref_int_x = value__26.index
        var t856 int
        var inline2218 int = ref_get__Ref_3int(t855)
        t856 = inline2218
        var t857 string = value__26.input
        var t858 int
        var inline2216 int = _goml_runtime_core_string_len(t857)
        t858 = inline2216
        var t859 bool = t856 < t858
        var jp849 bool
        if t859 {
            var t860 string = value__26.input
            var t861 *ref_int_x = value__26.index
            var t862 int
            var inline2210 int = ref_get__Ref_3int(t861)
            t862 = inline2210
            var t863 uint8
            var inline2208 uint8 = _goml_runtime_core_string_byte_get(t860, t862)
            t863 = inline2208
            var inline2205 bool = t863 >= 48
            if inline2205 {
                var inline2206 bool = t863 <= 57
                jp849 = inline2206
            } else {
                jp849 = false
            }
        } else {
            jp849 = false
        }
        if jp849 {
            var t850 *ref_int_x = value__26.index
            var t851 *ref_int_x = value__26.index
            var t852 int
            var inline2214 int = ref_get__Ref_3int(t851)
            t852 = inline2214
            var t853 int = t852 + 1
            ref_set__Ref_3int(t850, t853)
            continue
        } else {
            break Loop_loop847
        }
    }
    var t844 *ref_int_x = value__26.index
    var t845 int
    var inline2220 int = ref_get__Ref_3int(t844)
    t845 = inline2220
    var t846 bool = t845 > start__27
    return t846
}

func _goml_m_std_p_json_p_parse__json__number(value__28 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t867 *ref_int_x = value__28.index
    var start__29 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t867)
    var t989 string = value__28.input
    var t990 *ref_int_x = value__28.index
    var t991 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t990)
    var t992 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t989, t991)
    var t993 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t992, 45)
    if t993 {
        var t994 *ref_int_x = value__28.index
        var t995 *ref_int_x = value__28.index
        var t996 int
        var inline2226 int = ref_get__Ref_3int(t995)
        t996 = inline2226
        var t997 int = t996 + 1
        ref_set__Ref_3int(t994, t997)
    } else {}
    var t952 *ref_int_x = value__28.index
    var t953 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t952)
    var t954 string = value__28.input
    var t955 int = _goml_m_inherent_i_string_i_string_i_byte__len(t954)
    var t956 bool = t953 >= t955
    if t956 {
        var t957 string
        var inline2228 string = "incomplete number"
        var inline2229 string = "" + inline2228
        var inline2230 string = inline2229 + " at byte "
        var inline2231 *ref_int_x = value__28.index
        var inline2232 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2231)
        var inline2233 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2232)
        var inline2234 string = inline2230 + inline2233
        t957 = inline2234
        var t958 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t957,
        }
        return t958
    } else {
        var t960 string = value__28.input
        var t961 *ref_int_x = value__28.index
        var t962 int
        var inline2272 int = ref_get__Ref_3int(t961)
        t962 = inline2272
        var t963 uint8
        var inline2270 uint8 = _goml_runtime_core_string_byte_get(t960, t962)
        t963 = inline2270
        var t964 bool
        var inline2267 uint8 = 48
        var inline2268 bool = t963 == inline2267
        t964 = inline2268
        if t964 {
            var t965 *ref_int_x = value__28.index
            var t966 *ref_int_x = value__28.index
            var t967 int
            var inline2257 int = ref_get__Ref_3int(t966)
            t967 = inline2257
            var t968 int = t967 + 1
            ref_set__Ref_3int(t965, t968)
            var t974 *ref_int_x = value__28.index
            var t975 int
            var inline2253 int = ref_get__Ref_3int(t974)
            t975 = inline2253
            var t976 string = value__28.input
            var t977 int
            var inline2251 int = _goml_runtime_core_string_len(t976)
            t977 = inline2251
            var t978 bool = t975 < t977
            var jp971 bool
            if t978 {
                var t979 string = value__28.input
                var t980 *ref_int_x = value__28.index
                var t981 int
                var inline2241 int = ref_get__Ref_3int(t980)
                t981 = inline2241
                var t982 uint8
                var inline2239 uint8 = _goml_runtime_core_string_byte_get(t979, t981)
                t982 = inline2239
                var inline2236 bool = t982 >= 48
                if inline2236 {
                    var inline2237 bool = t982 <= 57
                    jp971 = inline2237
                } else {
                    jp971 = false
                }
            } else {
                jp971 = false
            }
            if jp971 {
                var t972 string
                var inline2243 string = "invalid leading zero"
                var inline2244 string = "" + inline2243
                var inline2245 string = inline2244 + " at byte "
                var inline2246 *ref_int_x = value__28.index
                var inline2247 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2246)
                var inline2248 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2247)
                var inline2249 string = inline2245 + inline2248
                t972 = inline2249
                var t973 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t972,
                }
                return t973
            } else {
                var t942 *ref_int_x = value__28.index
                var t943 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t942)
                var t944 string = value__28.input
                var t945 int = _goml_m_inherent_i_string_i_string_i_byte__len(t944)
                var t946 bool = t943 < t945
                var jp932 bool
                if t946 {
                    var t947 string = value__28.input
                    var t948 *ref_int_x = value__28.index
                    var t949 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t948)
                    var t950 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t947, t949)
                    var t951 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t950, 46)
                    jp932 = t951
                } else {
                    jp932 = false
                }
                if jp932 {
                    var t933 *ref_int_x = value__28.index
                    var t934 *ref_int_x = value__28.index
                    var t935 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t934)
                    var t936 int = t935 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t933, t936)
                    var t938 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t939 bool = !t938
                    if t939 {
                        var t940 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t941 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t940,
                        }
                        return t941
                    } else {
                        var t914 *ref_int_x = value__28.index
                        var t915 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t914)
                        var t916 string = value__28.input
                        var t917 int = _goml_m_inherent_i_string_i_string_i_byte__len(t916)
                        var t918 bool = t915 < t917
                        var jp879 bool
                        if t918 {
                            var t921 string = value__28.input
                            var t922 *ref_int_x = value__28.index
                            var t923 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t922)
                            var t924 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t921, t923)
                            var t925 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t924, 101)
                            if t925 {
                                jp879 = true
                            } else {
                                var t926 string = value__28.input
                                var t927 *ref_int_x = value__28.index
                                var t928 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t927)
                                var t929 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t926, t928)
                                var t930 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t929, 69)
                                jp879 = t930
                            }
                        } else {
                            jp879 = false
                        }
                        if jp879 {
                            var t880 *ref_int_x = value__28.index
                            var t881 *ref_int_x = value__28.index
                            var t882 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t881)
                            var t883 int = t882 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t880, t883)
                            var t897 *ref_int_x = value__28.index
                            var t898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t897)
                            var t899 string = value__28.input
                            var t900 int = _goml_m_inherent_i_string_i_string_i_byte__len(t899)
                            var t901 bool = t898 < t900
                            var jp891 bool
                            if t901 {
                                var t904 string = value__28.input
                                var t905 *ref_int_x = value__28.index
                                var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                                var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                                var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 43)
                                if t908 {
                                    jp891 = true
                                } else {
                                    var t909 string = value__28.input
                                    var t910 *ref_int_x = value__28.index
                                    var t911 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t910)
                                    var t912 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t909, t911)
                                    var t913 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t912, 45)
                                    jp891 = t913
                                }
                            } else {
                                jp891 = false
                            }
                            if jp891 {
                                var t892 *ref_int_x = value__28.index
                                var t893 *ref_int_x = value__28.index
                                var t894 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t893)
                                var t895 int = t894 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t892, t895)
                            } else {}
                            var t886 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t887 bool = !t886
                            if t887 {
                                var t888 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t889 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t888,
                                }
                                return t889
                            } else {
                                var t872 string = value__28.input
                                var t873 *ref_int_x = value__28.index
                                var t874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t873)
                                var t875 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t872, start__29, t874)
                                var t876 _goml_m_std_p_json_p_Value = Number{
                                    _0: t875,
                                }
                                var t877 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t876,
                                }
                                return t877
                            }
                        } else {
                            var t872 string = value__28.input
                            var t873 *ref_int_x = value__28.index
                            var t874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t873)
                            var t875 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t872, start__29, t874)
                            var t876 _goml_m_std_p_json_p_Value = Number{
                                _0: t875,
                            }
                            var t877 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t876,
                            }
                            return t877
                        }
                    }
                } else {
                    var t914 *ref_int_x = value__28.index
                    var t915 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t914)
                    var t916 string = value__28.input
                    var t917 int = _goml_m_inherent_i_string_i_string_i_byte__len(t916)
                    var t918 bool = t915 < t917
                    var jp879 bool
                    if t918 {
                        var t921 string = value__28.input
                        var t922 *ref_int_x = value__28.index
                        var t923 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t922)
                        var t924 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t921, t923)
                        var t925 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t924, 101)
                        if t925 {
                            jp879 = true
                        } else {
                            var t926 string = value__28.input
                            var t927 *ref_int_x = value__28.index
                            var t928 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t927)
                            var t929 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t926, t928)
                            var t930 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t929, 69)
                            jp879 = t930
                        }
                    } else {
                        jp879 = false
                    }
                    if jp879 {
                        var t880 *ref_int_x = value__28.index
                        var t881 *ref_int_x = value__28.index
                        var t882 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t881)
                        var t883 int = t882 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t880, t883)
                        var t897 *ref_int_x = value__28.index
                        var t898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t897)
                        var t899 string = value__28.input
                        var t900 int = _goml_m_inherent_i_string_i_string_i_byte__len(t899)
                        var t901 bool = t898 < t900
                        var jp891 bool
                        if t901 {
                            var t904 string = value__28.input
                            var t905 *ref_int_x = value__28.index
                            var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                            var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                            var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 43)
                            if t908 {
                                jp891 = true
                            } else {
                                var t909 string = value__28.input
                                var t910 *ref_int_x = value__28.index
                                var t911 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t910)
                                var t912 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t909, t911)
                                var t913 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t912, 45)
                                jp891 = t913
                            }
                        } else {
                            jp891 = false
                        }
                        if jp891 {
                            var t892 *ref_int_x = value__28.index
                            var t893 *ref_int_x = value__28.index
                            var t894 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t893)
                            var t895 int = t894 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t892, t895)
                        } else {}
                        var t886 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t887 bool = !t886
                        if t887 {
                            var t888 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t889 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t888,
                            }
                            return t889
                        } else {
                            var t872 string = value__28.input
                            var t873 *ref_int_x = value__28.index
                            var t874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t873)
                            var t875 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t872, start__29, t874)
                            var t876 _goml_m_std_p_json_p_Value = Number{
                                _0: t875,
                            }
                            var t877 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t876,
                            }
                            return t877
                        }
                    } else {
                        var t872 string = value__28.input
                        var t873 *ref_int_x = value__28.index
                        var t874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t873)
                        var t875 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t872, start__29, t874)
                        var t876 _goml_m_std_p_json_p_Value = Number{
                            _0: t875,
                        }
                        var t877 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t876,
                        }
                        return t877
                    }
                }
            }
        } else {
            var t985 bool = _goml_m_std_p_json_p_parse__digits(value__28)
            var t986 bool = !t985
            if t986 {
                var t987 string
                var inline2259 string = "expected number"
                var inline2260 string = "" + inline2259
                var inline2261 string = inline2260 + " at byte "
                var inline2262 *ref_int_x = value__28.index
                var inline2263 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2262)
                var inline2264 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2263)
                var inline2265 string = inline2261 + inline2264
                t987 = inline2265
                var t988 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t987,
                }
                return t988
            } else {
                var t942 *ref_int_x = value__28.index
                var t943 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t942)
                var t944 string = value__28.input
                var t945 int = _goml_m_inherent_i_string_i_string_i_byte__len(t944)
                var t946 bool = t943 < t945
                var jp932 bool
                if t946 {
                    var t947 string = value__28.input
                    var t948 *ref_int_x = value__28.index
                    var t949 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t948)
                    var t950 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t947, t949)
                    var t951 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t950, 46)
                    jp932 = t951
                } else {
                    jp932 = false
                }
                if jp932 {
                    var t933 *ref_int_x = value__28.index
                    var t934 *ref_int_x = value__28.index
                    var t935 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t934)
                    var t936 int = t935 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t933, t936)
                    var t938 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t939 bool = !t938
                    if t939 {
                        var t940 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t941 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t940,
                        }
                        return t941
                    } else {
                        var t914 *ref_int_x = value__28.index
                        var t915 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t914)
                        var t916 string = value__28.input
                        var t917 int = _goml_m_inherent_i_string_i_string_i_byte__len(t916)
                        var t918 bool = t915 < t917
                        var jp879 bool
                        if t918 {
                            var t921 string = value__28.input
                            var t922 *ref_int_x = value__28.index
                            var t923 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t922)
                            var t924 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t921, t923)
                            var t925 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t924, 101)
                            if t925 {
                                jp879 = true
                            } else {
                                var t926 string = value__28.input
                                var t927 *ref_int_x = value__28.index
                                var t928 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t927)
                                var t929 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t926, t928)
                                var t930 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t929, 69)
                                jp879 = t930
                            }
                        } else {
                            jp879 = false
                        }
                        if jp879 {
                            var t880 *ref_int_x = value__28.index
                            var t881 *ref_int_x = value__28.index
                            var t882 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t881)
                            var t883 int = t882 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t880, t883)
                            var t897 *ref_int_x = value__28.index
                            var t898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t897)
                            var t899 string = value__28.input
                            var t900 int = _goml_m_inherent_i_string_i_string_i_byte__len(t899)
                            var t901 bool = t898 < t900
                            var jp891 bool
                            if t901 {
                                var t904 string = value__28.input
                                var t905 *ref_int_x = value__28.index
                                var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                                var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                                var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 43)
                                if t908 {
                                    jp891 = true
                                } else {
                                    var t909 string = value__28.input
                                    var t910 *ref_int_x = value__28.index
                                    var t911 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t910)
                                    var t912 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t909, t911)
                                    var t913 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t912, 45)
                                    jp891 = t913
                                }
                            } else {
                                jp891 = false
                            }
                            if jp891 {
                                var t892 *ref_int_x = value__28.index
                                var t893 *ref_int_x = value__28.index
                                var t894 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t893)
                                var t895 int = t894 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t892, t895)
                            } else {}
                            var t886 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t887 bool = !t886
                            if t887 {
                                var t888 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t889 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t888,
                                }
                                return t889
                            } else {
                                var t872 string = value__28.input
                                var t873 *ref_int_x = value__28.index
                                var t874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t873)
                                var t875 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t872, start__29, t874)
                                var t876 _goml_m_std_p_json_p_Value = Number{
                                    _0: t875,
                                }
                                var t877 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t876,
                                }
                                return t877
                            }
                        } else {
                            var t872 string = value__28.input
                            var t873 *ref_int_x = value__28.index
                            var t874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t873)
                            var t875 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t872, start__29, t874)
                            var t876 _goml_m_std_p_json_p_Value = Number{
                                _0: t875,
                            }
                            var t877 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t876,
                            }
                            return t877
                        }
                    }
                } else {
                    var t914 *ref_int_x = value__28.index
                    var t915 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t914)
                    var t916 string = value__28.input
                    var t917 int = _goml_m_inherent_i_string_i_string_i_byte__len(t916)
                    var t918 bool = t915 < t917
                    var jp879 bool
                    if t918 {
                        var t921 string = value__28.input
                        var t922 *ref_int_x = value__28.index
                        var t923 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t922)
                        var t924 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t921, t923)
                        var t925 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t924, 101)
                        if t925 {
                            jp879 = true
                        } else {
                            var t926 string = value__28.input
                            var t927 *ref_int_x = value__28.index
                            var t928 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t927)
                            var t929 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t926, t928)
                            var t930 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t929, 69)
                            jp879 = t930
                        }
                    } else {
                        jp879 = false
                    }
                    if jp879 {
                        var t880 *ref_int_x = value__28.index
                        var t881 *ref_int_x = value__28.index
                        var t882 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t881)
                        var t883 int = t882 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t880, t883)
                        var t897 *ref_int_x = value__28.index
                        var t898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t897)
                        var t899 string = value__28.input
                        var t900 int = _goml_m_inherent_i_string_i_string_i_byte__len(t899)
                        var t901 bool = t898 < t900
                        var jp891 bool
                        if t901 {
                            var t904 string = value__28.input
                            var t905 *ref_int_x = value__28.index
                            var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                            var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                            var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 43)
                            if t908 {
                                jp891 = true
                            } else {
                                var t909 string = value__28.input
                                var t910 *ref_int_x = value__28.index
                                var t911 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t910)
                                var t912 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t909, t911)
                                var t913 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t912, 45)
                                jp891 = t913
                            }
                        } else {
                            jp891 = false
                        }
                        if jp891 {
                            var t892 *ref_int_x = value__28.index
                            var t893 *ref_int_x = value__28.index
                            var t894 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t893)
                            var t895 int = t894 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t892, t895)
                        } else {}
                        var t886 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t887 bool = !t886
                        if t887 {
                            var t888 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t889 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t888,
                            }
                            return t889
                        } else {
                            var t872 string = value__28.input
                            var t873 *ref_int_x = value__28.index
                            var t874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t873)
                            var t875 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t872, start__29, t874)
                            var t876 _goml_m_std_p_json_p_Value = Number{
                                _0: t875,
                            }
                            var t877 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t876,
                            }
                            return t877
                        }
                    } else {
                        var t872 string = value__28.input
                        var t873 *ref_int_x = value__28.index
                        var t874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t873)
                        var t875 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t872, start__29, t874)
                        var t876 _goml_m_std_p_json_p_Value = Number{
                            _0: t875,
                        }
                        var t877 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t876,
                        }
                        return t877
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__30 _goml_m_std_p_json_p_JsonParser, expected__31 string, result__32 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t1013 *ref_int_x = value__30.index
    var t1014 int
    var inline2302 int = ref_get__Ref_3int(t1013)
    t1014 = inline2302
    var t1015 int
    var inline2300 int = _goml_runtime_core_string_len(expected__31)
    t1015 = inline2300
    var t1016 int = t1014 + t1015
    var t1017 string = value__30.input
    var t1018 int
    var inline2298 int = _goml_runtime_core_string_len(t1017)
    t1018 = inline2298
    var t1019 bool = t1016 <= t1018
    var jp1004 bool
    if t1019 {
        var t1020 string = value__30.input
        var t1021 *ref_int_x = value__30.index
        var t1022 int
        var inline2282 int = ref_get__Ref_3int(t1021)
        t1022 = inline2282
        var t1023 *ref_int_x = value__30.index
        var t1024 int
        var inline2280 int = ref_get__Ref_3int(t1023)
        t1024 = inline2280
        var t1025 int
        var inline2278 int = _goml_runtime_core_string_len(expected__31)
        t1025 = inline2278
        var t1026 int = t1024 + t1025
        var t1027 string
        var inline2276 string = string_byte_slice(t1020, t1022, t1026)
        t1027 = inline2276
        var inline2274 bool = t1027 == expected__31
        jp1004 = inline2274
    } else {
        jp1004 = false
    }
    if jp1004 {
        var t1005 *ref_int_x = value__30.index
        var t1006 *ref_int_x = value__30.index
        var t1007 int
        var inline2288 int = ref_get__Ref_3int(t1006)
        t1007 = inline2288
        var t1008 int
        var inline2286 int = _goml_runtime_core_string_len(expected__31)
        t1008 = inline2286
        var t1009 int = t1007 + t1008
        ref_set__Ref_3int(t1005, t1009)
        var t1010 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__32,
        }
        return t1010
    } else {
        var t1011 string
        var inline2290 string = "invalid literal"
        var inline2291 string = "" + inline2290
        var inline2292 string = inline2291 + " at byte "
        var inline2293 *ref_int_x = value__30.index
        var inline2294 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2293)
        var inline2295 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2294)
        var inline2296 string = inline2292 + inline2295
        t1011 = inline2296
        var t1012 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1011,
        }
        return t1012
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1031 *ref_int_x = value__33.index
    var t1032 *ref_int_x = value__33.index
    var t1033 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1032)
    var t1034 int = t1033 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1031, t1034)
    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
    var vec_literal__8961 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t1089 *ref_int_x = value__33.index
    var t1090 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1089)
    var t1091 string = value__33.input
    var t1092 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1091)
    var t1093 bool = t1090 < t1092
    var jp1082 bool
    if t1093 {
        var t1094 string = value__33.input
        var t1095 *ref_int_x = value__33.index
        var t1096 int
        var inline2309 int = ref_get__Ref_3int(t1095)
        t1096 = inline2309
        var t1097 uint8
        var inline2307 uint8 = _goml_runtime_core_string_byte_get(t1094, t1096)
        t1097 = inline2307
        var inline2304 uint8 = 93
        var inline2305 bool = t1097 == inline2304
        jp1082 = inline2305
    } else {
        jp1082 = false
    }
    if jp1082 {
        var t1083 *ref_int_x = value__33.index
        var t1084 *ref_int_x = value__33.index
        var t1085 int
        var inline2313 int = ref_get__Ref_3int(t1084)
        t1085 = inline2313
        var t1086 int = t1085 + 1
        ref_set__Ref_3int(t1083, t1086)
        var t1087 _goml_m_std_p_json_p_Value = Array{
            _0: vec_literal__8961,
        }
        var t1088 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1087,
        }
        return t1088
    } else {
        Loop_loop1039:
        for {
            var t1040 *ref_int_x = value__33.index
            var t1041 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1040)
            var t1042 string = value__33.input
            var t1043 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1042)
            var t1044 bool = t1041 < t1043
            if t1044 {
                var mtmp50 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__33)
                var jp1046 _goml_m_std_p_json_p_Value
                switch mtmp50.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x51 _goml_m_std_p_json_p_Value = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp1046 = x51
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8961, jp1046)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                    var t1048 *ref_int_x = value__33.index
                    var t1049 int
                    var inline2355 int = ref_get__Ref_3int(t1048)
                    t1049 = inline2355
                    var t1050 string = value__33.input
                    var t1051 int
                    var inline2353 int = _goml_runtime_core_string_len(t1050)
                    t1051 = inline2353
                    var t1052 bool = t1049 >= t1051
                    if t1052 {
                        var t1053 string
                        var inline2315 string = "unterminated array"
                        var inline2316 string = "" + inline2315
                        var inline2317 string = inline2316 + " at byte "
                        var inline2318 *ref_int_x = value__33.index
                        var inline2319 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2318)
                        var inline2320 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2319)
                        var inline2321 string = inline2317 + inline2320
                        t1053 = inline2321
                        var t1054 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1053,
                        }
                        return t1054
                    } else {
                        var t1056 string = value__33.input
                        var t1057 *ref_int_x = value__33.index
                        var t1058 int
                        var inline2351 int = ref_get__Ref_3int(t1057)
                        t1058 = inline2351
                        var t1059 uint8
                        var inline2349 uint8 = _goml_runtime_core_string_byte_get(t1056, t1058)
                        t1059 = inline2349
                        var t1060 bool
                        var inline2346 uint8 = 93
                        var inline2347 bool = t1059 == inline2346
                        t1060 = inline2347
                        if t1060 {
                            var t1061 *ref_int_x = value__33.index
                            var t1062 *ref_int_x = value__33.index
                            var t1063 int
                            var inline2325 int = ref_get__Ref_3int(t1062)
                            t1063 = inline2325
                            var t1064 int = t1063 + 1
                            ref_set__Ref_3int(t1061, t1064)
                            var t1065 _goml_m_std_p_json_p_Value = Array{
                                _0: vec_literal__8961,
                            }
                            var t1066 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1065,
                            }
                            return t1066
                        } else {
                            var t1068 string = value__33.input
                            var t1069 *ref_int_x = value__33.index
                            var t1070 int
                            var inline2344 int = ref_get__Ref_3int(t1069)
                            t1070 = inline2344
                            var t1071 uint8
                            var inline2342 uint8 = _goml_runtime_core_string_byte_get(t1068, t1070)
                            t1071 = inline2342
                            var t1072 bool
                            var inline2339 uint8 = 44
                            var inline2340 bool = t1071 == inline2339
                            t1072 = inline2340
                            if t1072 {
                                var t1073 *ref_int_x = value__33.index
                                var t1074 *ref_int_x = value__33.index
                                var t1075 int
                                var inline2329 int = ref_get__Ref_3int(t1074)
                                t1075 = inline2329
                                var t1076 int = t1075 + 1
                                ref_set__Ref_3int(t1073, t1076)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                                continue
                            } else {
                                var t1078 string
                                var inline2331 string = "expected array separator"
                                var inline2332 string = "" + inline2331
                                var inline2333 string = inline2332 + " at byte "
                                var inline2334 *ref_int_x = value__33.index
                                var inline2335 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2334)
                                var inline2336 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2335)
                                var inline2337 string = inline2333 + inline2336
                                t1078 = inline2337
                                var t1079 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1078,
                                }
                                return t1079
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x52 string = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t1080 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x52,
                    }
                    return t1080
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1039
            }
        }
        var t1037 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
        var t1038 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1037,
        }
        return t1038
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__36 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1101 *ref_int_x = value__36.index
    var t1102 *ref_int_x = value__36.index
    var t1103 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1102)
    var t1104 int = t1103 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1101, t1104)
    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
    var vec_literal__10180 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t1184 *ref_int_x = value__36.index
    var t1185 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1184)
    var t1186 string = value__36.input
    var t1187 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1186)
    var t1188 bool = t1185 < t1187
    var jp1177 bool
    if t1188 {
        var t1189 string = value__36.input
        var t1190 *ref_int_x = value__36.index
        var t1191 int
        var inline2362 int = ref_get__Ref_3int(t1190)
        t1191 = inline2362
        var t1192 uint8
        var inline2360 uint8 = _goml_runtime_core_string_byte_get(t1189, t1191)
        t1192 = inline2360
        var inline2357 uint8 = 125
        var inline2358 bool = t1192 == inline2357
        jp1177 = inline2358
    } else {
        jp1177 = false
    }
    if jp1177 {
        var t1178 *ref_int_x = value__36.index
        var t1179 *ref_int_x = value__36.index
        var t1180 int
        var inline2366 int = ref_get__Ref_3int(t1179)
        t1180 = inline2366
        var t1181 int = t1180 + 1
        ref_set__Ref_3int(t1178, t1181)
        var t1182 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10180,
        }
        var t1183 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1182,
        }
        return t1183
    } else {
        Loop_loop1109:
        for {
            var t1110 *ref_int_x = value__36.index
            var t1111 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1110)
            var t1112 string = value__36.input
            var t1113 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1112)
            var t1114 bool = t1111 < t1113
            if t1114 {
                var mtmp62 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__36)
                var jp1116 string
                switch mtmp62.(type) {
                case Result__string__string_Ok:
                    var x63 string = mtmp62.(Result__string__string_Ok)._0
                    jp1116 = x63
                    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                    var t1164 *ref_int_x = value__36.index
                    var t1165 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1164)
                    var t1166 string = value__36.input
                    var t1167 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1166)
                    var t1168 bool = t1165 >= t1167
                    var jp1156 bool
                    if t1168 {
                        jp1156 = true
                    } else {
                        var t1169 string = value__36.input
                        var t1170 *ref_int_x = value__36.index
                        var t1171 int
                        var inline2373 int = ref_get__Ref_3int(t1170)
                        t1171 = inline2373
                        var t1172 uint8
                        var inline2371 uint8 = _goml_runtime_core_string_byte_get(t1169, t1171)
                        t1172 = inline2371
                        var t1173 bool
                        var inline2368 uint8 = 58
                        var inline2369 bool = t1172 == inline2368
                        t1173 = inline2369
                        var t1174 bool = !t1173
                        jp1156 = t1174
                    }
                    if jp1156 {
                        var t1157 string
                        var inline2375 string = "expected object colon"
                        var inline2376 string = "" + inline2375
                        var inline2377 string = inline2376 + " at byte "
                        var inline2378 *ref_int_x = value__36.index
                        var inline2379 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2378)
                        var inline2380 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2379)
                        var inline2381 string = inline2377 + inline2380
                        t1157 = inline2381
                        var t1158 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1157,
                        }
                        return t1158
                    } else {
                        var t1159 *ref_int_x = value__36.index
                        var t1160 *ref_int_x = value__36.index
                        var t1161 int
                        var inline2385 int = ref_get__Ref_3int(t1160)
                        t1161 = inline2385
                        var t1162 int = t1161 + 1
                        ref_set__Ref_3int(t1159, t1162)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                        var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__36)
                        var jp1119 _goml_m_std_p_json_p_Value
                        switch mtmp68.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp1119 = x69
                            var t1120 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp1116,
                                _1: jp1119,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10180, t1120)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                            var t1122 *ref_int_x = value__36.index
                            var t1123 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1122)
                            var t1124 string = value__36.input
                            var t1125 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1124)
                            var t1126 bool = t1123 >= t1125
                            if t1126 {
                                var t1127 string
                                var inline2387 string = "unterminated object"
                                var inline2388 string = "" + inline2387
                                var inline2389 string = inline2388 + " at byte "
                                var inline2390 *ref_int_x = value__36.index
                                var inline2391 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2390)
                                var inline2392 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2391)
                                var inline2393 string = inline2389 + inline2392
                                t1127 = inline2393
                                var t1128 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1127,
                                }
                                return t1128
                            } else {
                                var t1130 string = value__36.input
                                var t1131 *ref_int_x = value__36.index
                                var t1132 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1131)
                                var t1133 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1130, t1132)
                                var t1134 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1133, 125)
                                if t1134 {
                                    var t1135 *ref_int_x = value__36.index
                                    var t1136 *ref_int_x = value__36.index
                                    var t1137 int
                                    var inline2397 int = ref_get__Ref_3int(t1136)
                                    t1137 = inline2397
                                    var t1138 int = t1137 + 1
                                    ref_set__Ref_3int(t1135, t1138)
                                    var t1139 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10180,
                                    }
                                    var t1140 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1139,
                                    }
                                    return t1140
                                } else {
                                    var t1142 string = value__36.input
                                    var t1143 *ref_int_x = value__36.index
                                    var t1144 int
                                    var inline2408 int = ref_get__Ref_3int(t1143)
                                    t1144 = inline2408
                                    var t1145 uint8
                                    var inline2406 uint8 = _goml_runtime_core_string_byte_get(t1142, t1144)
                                    t1145 = inline2406
                                    var t1146 bool
                                    var inline2403 uint8 = 44
                                    var inline2404 bool = t1145 == inline2403
                                    t1146 = inline2404
                                    if t1146 {
                                        var t1147 *ref_int_x = value__36.index
                                        var t1148 *ref_int_x = value__36.index
                                        var t1149 int
                                        var inline2401 int = ref_get__Ref_3int(t1148)
                                        t1149 = inline2401
                                        var t1150 int = t1149 + 1
                                        ref_set__Ref_3int(t1147, t1150)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                                        continue
                                    } else {
                                        var t1152 string = _goml_m_std_p_json_p_json__error(value__36, "expected object separator")
                                        var t1153 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1152,
                                        }
                                        return t1153
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t1154 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x70,
                            }
                            return t1154
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x64 string = mtmp62.(Result__string__string_Err)._0
                    var t1175 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x64,
                    }
                    return t1175
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1109
            }
        }
        var t1107 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
        var t1108 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1107,
        }
        return t1108
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__40 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__40)
    var t1198 *ref_int_x = value__40.index
    var t1199 int
    var inline2438 int = ref_get__Ref_3int(t1198)
    t1199 = inline2438
    var t1200 string = value__40.input
    var t1201 int
    var inline2436 int = _goml_runtime_core_string_len(t1200)
    t1201 = inline2436
    var t1202 bool = t1199 >= t1201
    if t1202 {
        var t1203 string
        var inline2410 string = "expected JSON value"
        var inline2411 string = "" + inline2410
        var inline2412 string = inline2411 + " at byte "
        var inline2413 *ref_int_x = value__40.index
        var inline2414 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2413)
        var inline2415 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2414)
        var inline2416 string = inline2412 + inline2415
        t1203 = inline2416
        var t1204 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1203,
        }
        return t1204
    } else {
        var t1205 string = value__40.input
        var t1206 *ref_int_x = value__40.index
        var t1207 int
        var inline2434 int = ref_get__Ref_3int(t1206)
        t1207 = inline2434
        var mtmp77 uint8
        var inline2432 uint8 = _goml_runtime_core_string_byte_get(t1205, t1207)
        mtmp77 = inline2432
        switch mtmp77 {
        case 123:
            var t1210 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__40)
            return t1210
        case 91:
            var t1211 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__40)
            return t1211
        case 34:
            var mtmp78 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__40)
            switch mtmp78.(type) {
            case Result__string__string_Ok:
                var x79 string = mtmp78.(Result__string__string_Ok)._0
                var t1214 _goml_m_std_p_json_p_Value = String{
                    _0: x79,
                }
                var t1215 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1214,
                }
                return t1215
            case Result__string__string_Err:
                var x80 string = mtmp78.(Result__string__string_Err)._0
                var t1216 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x80,
                }
                return t1216
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t1217 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t1218 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "true", t1217)
            return t1218
        case 102:
            var t1219 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t1220 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "false", t1219)
            return t1220
        case 110:
            var t1221 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "null", Null{})
            return t1221
        default:
            var t1229 bool
            var inline2429 uint8 = 45
            var inline2430 bool = mtmp77 == inline2429
            t1229 = inline2430
            var jp1225 bool
            if t1229 {
                jp1225 = true
            } else {
                var inline2418 bool = mtmp77 >= 48
                if inline2418 {
                    var inline2419 bool = mtmp77 <= 57
                    jp1225 = inline2419
                } else {
                    jp1225 = false
                }
            }
            if jp1225 {
                var t1226 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__40)
                return t1226
            } else {
                var t1227 string
                var inline2421 string = "unexpected JSON token"
                var inline2422 string = "" + inline2421
                var inline2423 string = inline2422 + " at byte "
                var inline2424 *ref_int_x = value__40.index
                var inline2425 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2424)
                var inline2426 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2425)
                var inline2427 string = inline2423 + inline2426
                t1227 = inline2427
                var t1228 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1227,
                }
                return t1228
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__44 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__45 _goml_m_std_p_json_p_JsonParser
    var inline2454 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline2455 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__44,
        index: inline2454,
    }
    parser__45 = inline2455
    var mtmp81 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__45)
    var jp1234 _goml_m_std_p_json_p_Value
    switch mtmp81.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x82 _goml_m_std_p_json_p_Value = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1234 = x82
        _goml_m_std_p_json_p_skip__json__whitespace(parser__45)
        var t1237 *ref_int_x = parser__45.index
        var t1238 int
        var inline2452 int = ref_get__Ref_3int(t1237)
        t1238 = inline2452
        var t1239 int
        var inline2450 int = _goml_runtime_core_string_len(input__44)
        t1239 = inline2450
        var t1240 bool
        var inline2448 bool = t1238 == t1239
        t1240 = inline2448
        if t1240 {
            var t1241 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp1234,
            }
            return t1241
        } else {
            var t1242 string
            var inline2440 string = "trailing JSON data"
            var inline2441 string = "" + inline2440
            var inline2442 string = inline2441 + " at byte "
            var inline2443 *ref_int_x = parser__45.index
            var inline2444 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2443)
            var inline2445 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2444)
            var inline2446 string = inline2442 + inline2445
            t1242 = inline2446
            var t1243 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1242,
            }
            return t1243
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x83 string = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t1244 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x83,
        }
        return t1244
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__48 _goml_m_std_p_text_p_StringBuilder, value__49 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    var start__50 int = 0
    var for_index86 int = 0
    var for_limit87 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    Loop_loop1258:
    for {
        var t1259 bool = for_index86 < for_limit87
        if t1259 {
            var for_item88 int = for_index86
            var t1260 int = for_index86 + 1
            for_index86 = t1260
            var byte__52 uint8
            var inline2516 uint8 = _goml_runtime_core_string_byte_get(value__49, for_item88)
            byte__52 = inline2516
            var t1313 bool
            var inline2513 uint8 = 34
            var inline2514 bool = byte__52 == inline2513
            t1313 = inline2514
            var jp1311 bool
            if t1313 {
                jp1311 = true
            } else {
                var inline2460 uint8 = 92
                var inline2461 bool = byte__52 == inline2460
                jp1311 = inline2461
            }
            var jp1308 bool
            if jp1311 {
                jp1308 = true
            } else {
                var inline2463 uint8 = 8
                var inline2464 bool = byte__52 == inline2463
                jp1308 = inline2464
            }
            var jp1305 bool
            if jp1308 {
                jp1305 = true
            } else {
                var inline2466 uint8 = 9
                var inline2467 bool = byte__52 == inline2466
                jp1305 = inline2467
            }
            var jp1302 bool
            if jp1305 {
                jp1302 = true
            } else {
                var inline2469 uint8 = 10
                var inline2470 bool = byte__52 == inline2469
                jp1302 = inline2470
            }
            var jp1299 bool
            if jp1302 {
                jp1299 = true
            } else {
                var inline2472 uint8 = 12
                var inline2473 bool = byte__52 == inline2472
                jp1299 = inline2473
            }
            var jp1296 bool
            if jp1299 {
                jp1296 = true
            } else {
                var inline2475 uint8 = 13
                var inline2476 bool = byte__52 == inline2475
                jp1296 = inline2476
            }
            var jp1263 bool
            if jp1296 {
                jp1263 = true
            } else {
                var t1297 bool = byte__52 < 32
                jp1263 = t1297
            }
            if jp1263 {
                var t1292 bool = start__50 < for_item88
                if t1292 {
                    var t1293 string
                    var inline2478 string = string_byte_slice(value__49, start__50, for_item88)
                    t1293 = inline2478
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1293)
                } else {}
                var t1267 bool
                var inline2510 uint8 = 34
                var inline2511 bool = byte__52 == inline2510
                t1267 = inline2511
                if t1267 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\"")
                } else {
                    var t1270 bool
                    var inline2507 uint8 = 92
                    var inline2508 bool = byte__52 == inline2507
                    t1270 = inline2508
                    if t1270 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\\")
                    } else {
                        var t1273 bool
                        var inline2504 uint8 = 8
                        var inline2505 bool = byte__52 == inline2504
                        t1273 = inline2505
                        if t1273 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\b")
                        } else {
                            var t1276 bool
                            var inline2501 uint8 = 9
                            var inline2502 bool = byte__52 == inline2501
                            t1276 = inline2502
                            if t1276 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\t")
                            } else {
                                var t1279 bool
                                var inline2498 uint8 = 10
                                var inline2499 bool = byte__52 == inline2498
                                t1279 = inline2499
                                if t1279 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\n")
                                } else {
                                    var t1282 bool
                                    var inline2495 uint8 = 12
                                    var inline2496 bool = byte__52 == inline2495
                                    t1282 = inline2496
                                    if t1282 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\f")
                                    } else {
                                        var t1285 bool
                                        var inline2492 uint8 = 13
                                        var inline2493 bool = byte__52 == inline2492
                                        t1285 = inline2493
                                        if t1285 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\u00")
                                            var t1287 uint8 = byte__52 / 16
                                            var t1288 rune
                                            var inline2489 int = int(uint8(t1287))
                                            var inline2490 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline2489)
                                            t1288 = inline2490
                                            var inline2486 string = _goml_m_inherent_i_char_i_char_i_to__string(t1288)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline2486)
                                            var t1289_rhs uint8 = 16
                                            var t1289 uint8 = byte__52 % t1289_rhs
                                            var t1290 rune
                                            var inline2483 int = int(uint8(t1289))
                                            var inline2484 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline2483)
                                            t1290 = inline2484
                                            var inline2480 string = _goml_m_inherent_i_char_i_char_i_to__string(t1290)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline2480)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1266 int = for_item88 + 1
                start__50 = t1266
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop1258
        }
    }
    var t1253 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    var t1254 bool = start__50 < t1253
    if t1254 {
        var t1255 int
        var inline2520 int = _goml_runtime_core_string_len(value__49)
        t1255 = inline2520
        var t1256 string
        var inline2518 string = string_byte_slice(value__49, start__50, t1255)
        t1256 = inline2518
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1256)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__53 _goml_m_std_p_text_p_StringBuilder, value__54 _goml_m_std_p_json_p_Value) struct{} {
    switch value__54.(type) {
    case Object:
        var x97 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__54.(Object)._0
        var inline2534 rune = 123
        var inline2535 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2534)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2535)
        var index__56 int = 0
        var for_limit104 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97)
        var for_index105 int = 0
        Loop_loop1319:
        for {
            var t1320 bool = for_index105 < for_limit104
            if t1320 {
                var for_item106 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97, for_index105)
                var t1321 int = for_index105 + 1
                for_index105 = t1321
                var t1327 bool = index__56 > 0
                if t1327 {
                    var inline2522 rune = 44
                    var inline2523 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2522)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2523)
                } else {}
                var t1323 string = for_item106._0
                _goml_m_std_p_json_p_write__json__string(builder__53, t1323)
                var inline2526 rune = 58
                var inline2527 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2526)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2527)
                var t1324 _goml_m_std_p_json_p_Value = for_item106._1
                _goml_m_std_p_json_p_write__json__value(builder__53, t1324)
                var compound_old112 int = index__56
                var compound_value113 int = 1
                var t1325 int = compound_old112 + compound_value113
                index__56 = t1325
                continue
            } else {
                break Loop_loop1319
            }
        }
        var inline2530 rune = 125
        var inline2531 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2530)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2531)
        return struct{}{}
    case Array:
        var x98 *_goml_vec__goml_m_std_p_json_p_Value = value__54.(Array)._0
        var inline2546 rune = 91
        var inline2547 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2546)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2547)
        var index__59 int = 0
        var for_limit118 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x98)
        var for_index119 int = 0
        Loop_loop1331:
        for {
            var t1332 bool = for_index119 < for_limit118
            if t1332 {
                var for_item120 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x98, for_index119)
                var t1333 int = for_index119 + 1
                for_index119 = t1333
                var t1337 bool = index__59 > 0
                if t1337 {
                    var inline2538 rune = 44
                    var inline2539 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2538)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2539)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__53, for_item120)
                var compound_old124 int = index__59
                var compound_value125 int = 1
                var t1335 int = compound_old124 + compound_value125
                index__59 = t1335
                continue
            } else {
                break Loop_loop1331
            }
        }
        var inline2542 rune = 93
        var inline2543 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2542)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2543)
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
        var jp1342 string
        if x101 {
            jp1342 = "true"
        } else {
            jp1342 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, jp1342)
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
    var inline2556 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline2557 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline2556,
    }
    builder__65 = inline2557
    _goml_m_std_p_json_p_write__json__value(builder__65, value__64)
    var inline2550 *_goml_vec_uint8 = builder__65.values
    var inline2551 Tuple2_4bool_6string = string_from_utf8(inline2550)
    var inline2553 string = inline2551._1
    return inline2553
}

func _goml_m_std_p_json_p_field(value__66 _goml_m_std_p_json_p_Value, name__67 string) _goml_m_Option____std_p_json_p_Value {
    switch value__66.(type) {
    case Object:
        var x129 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__66.(Object)._0
        var for_limit135 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129)
        var for_index136 int = 0
        Loop_loop1353:
        for {
            var t1354 bool = for_index136 < for_limit135
            if t1354 {
                var for_item137 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129, for_index136)
                var t1355 int = for_index136 + 1
                for_index136 = t1355
                var t1357 string = for_item137._0
                var t1358 bool
                var inline2559 bool = t1357 == name__67
                t1358 = inline2559
                if t1358 {
                    var t1359 _goml_m_std_p_json_p_Value = for_item137._1
                    var t1360 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1359,
                    }
                    return t1360
                } else {
                    continue
                }
            } else {
                break Loop_loop1353
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__72 string) Option__int {
    var t1370 int
    var inline2578 int = _goml_runtime_core_string_len(value__72)
    t1370 = inline2578
    var t1371 bool
    var inline2575 int = 0
    var inline2576 bool = t1370 == inline2575
    t1371 = inline2576
    if t1371 {
        return Option__int_None{}
    } else {
        var t1372 uint8
        var inline2572 int = 0
        var inline2573 uint8 = _goml_runtime_core_string_byte_get(value__72, inline2572)
        t1372 = inline2573
        var negative__73 bool
        var inline2569 uint8 = 45
        var inline2570 bool = t1372 == inline2569
        negative__73 = inline2570
        var jp1374 int
        if negative__73 {
            jp1374 = 1
        } else {
            jp1374 = 0
        }
        var index__74 int = jp1374
        var result__75 int = 0
        var t1395 int
        var inline2567 int = _goml_runtime_core_string_len(value__72)
        t1395 = inline2567
        var t1396 bool
        var inline2565 bool = index__74 == t1395
        t1396 = inline2565
        if t1396 {
            return Option__int_None{}
        } else {
            Loop_loop1381:
            for {
                var t1382 int
                var inline2563 int = _goml_runtime_core_string_len(value__72)
                t1382 = inline2563
                var t1383 bool = index__74 < t1382
                if t1383 {
                    var byte__76 uint8
                    var inline2561 uint8 = _goml_runtime_core_string_byte_get(value__72, index__74)
                    byte__76 = inline2561
                    var t1393 bool = byte__76 < 48
                    var jp1388 bool
                    if t1393 {
                        jp1388 = true
                    } else {
                        var t1394 bool = byte__76 > 57
                        jp1388 = t1394
                    }
                    if jp1388 {
                        return Option__int_None{}
                    } else {
                        var t1389 int = result__75 * 10
                        var t1390 uint8 = byte__76 - 48
                        var t1391 int = int(uint8(t1390))
                        var t1392 int = t1389 + t1391
                        result__75 = t1392
                        var compound_old148 int = index__74
                        var compound_value149 int = 1
                        var t1385 int = compound_old148 + compound_value149
                        index__74 = t1385
                        continue
                    }
                } else {
                    break Loop_loop1381
                }
            }
            var jp1378 int
            if negative__73 {
                var t1380 int = 0 - result__75
                jp1378 = t1380
            } else {
                jp1378 = result__75
            }
            var t1379 Option__int = Option__int_Some{
                _0: jp1378,
            }
            return t1379
        }
    }
}

func main0() struct{} {
    var mtmp177 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1417 _goml_m_std_p_json_p_Value
    switch mtmp177.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x178 _goml_m_std_p_json_p_Value = mtmp177.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1417 = x178
        var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1417, "name")
        switch mtmp181.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline2583 string = "missing name"
            var inline2584 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2583)
            _goml_runtime_core_string_println(inline2584)
            var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1417, "version")
            switch mtmp186.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline2598 string = "missing version"
                var inline2599 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2598)
                _goml_runtime_core_string_println(inline2599)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp188 Option__int
                switch x187.(type) {
                case Number:
                    var inline2609 string = x187.(Number)._0
                    var inline2611 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2609)
                    mtmp188 = inline2611
                default:
                    mtmp188 = Option__int_None{}
                }
                switch mtmp188.(type) {
                case Option__int_None:
                    var inline2602 string = "invalid version"
                    var inline2603 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2602)
                    _goml_runtime_core_string_println(inline2603)
                case Option__int_Some:
                    var x189 int = mtmp188.(Option__int_Some)._0
                    var inline2606 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x189)
                    _goml_runtime_core_string_println(inline2606)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1417, "stable")
            switch mtmp191.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline2613 string = "missing stable"
                var inline2614 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2613)
                _goml_runtime_core_string_println(inline2614)
                var t1421 string = _goml_m_std_p_json_p_encode(jp1417)
                println__T_string(t1421)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field2773 bool
                switch x192.(type) {
                case Bool:
                    var inline2624 bool = x192.(Bool)._0
                    commute_field2773 = inline2624
                    var inline2621 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2773)
                    _goml_runtime_core_string_println(inline2621)
                    var t1421 string = _goml_m_std_p_json_p_encode(jp1417)
                    println__T_string(t1421)
                    return struct{}{}
                default:
                    var inline2617 string = "invalid stable"
                    var inline2618 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2617)
                    _goml_runtime_core_string_println(inline2618)
                    var t1421 string = _goml_m_std_p_json_p_encode(jp1417)
                    println__T_string(t1421)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field2779 string
            switch x182.(type) {
            case String:
                var inline2594 string = x182.(String)._0
                commute_field2779 = inline2594
                var inline2591 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field2779)
                _goml_runtime_core_string_println(inline2591)
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1417, "version")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2598 string = "missing version"
                    var inline2599 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2598)
                    _goml_runtime_core_string_println(inline2599)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp188 Option__int
                    switch x187.(type) {
                    case Number:
                        var inline2609 string = x187.(Number)._0
                        var inline2611 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2609)
                        mtmp188 = inline2611
                    default:
                        mtmp188 = Option__int_None{}
                    }
                    switch mtmp188.(type) {
                    case Option__int_None:
                        var inline2602 string = "invalid version"
                        var inline2603 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2602)
                        _goml_runtime_core_string_println(inline2603)
                    case Option__int_Some:
                        var x189 int = mtmp188.(Option__int_Some)._0
                        var inline2606 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x189)
                        _goml_runtime_core_string_println(inline2606)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1417, "stable")
                switch mtmp191.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2613 string = "missing stable"
                    var inline2614 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2613)
                    _goml_runtime_core_string_println(inline2614)
                    var t1421 string = _goml_m_std_p_json_p_encode(jp1417)
                    println__T_string(t1421)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field2773 bool
                    switch x192.(type) {
                    case Bool:
                        var inline2624 bool = x192.(Bool)._0
                        commute_field2773 = inline2624
                        var inline2621 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2773)
                        _goml_runtime_core_string_println(inline2621)
                        var t1421 string = _goml_m_std_p_json_p_encode(jp1417)
                        println__T_string(t1421)
                        return struct{}{}
                    default:
                        var inline2617 string = "invalid stable"
                        var inline2618 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2617)
                        _goml_runtime_core_string_println(inline2618)
                        var t1421 string = _goml_m_std_p_json_p_encode(jp1417)
                        println__T_string(t1421)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline2587 string = "invalid name"
                var inline2588 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2587)
                _goml_runtime_core_string_println(inline2588)
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1417, "version")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2598 string = "missing version"
                    var inline2599 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2598)
                    _goml_runtime_core_string_println(inline2599)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp188 Option__int
                    switch x187.(type) {
                    case Number:
                        var inline2609 string = x187.(Number)._0
                        var inline2611 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2609)
                        mtmp188 = inline2611
                    default:
                        mtmp188 = Option__int_None{}
                    }
                    switch mtmp188.(type) {
                    case Option__int_None:
                        var inline2602 string = "invalid version"
                        var inline2603 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2602)
                        _goml_runtime_core_string_println(inline2603)
                    case Option__int_Some:
                        var x189 int = mtmp188.(Option__int_Some)._0
                        var inline2606 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x189)
                        _goml_runtime_core_string_println(inline2606)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1417, "stable")
                switch mtmp191.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2613 string = "missing stable"
                    var inline2614 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2613)
                    _goml_runtime_core_string_println(inline2614)
                    var t1421 string = _goml_m_std_p_json_p_encode(jp1417)
                    println__T_string(t1421)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field2773 bool
                    switch x192.(type) {
                    case Bool:
                        var inline2624 bool = x192.(Bool)._0
                        commute_field2773 = inline2624
                        var inline2621 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2773)
                        _goml_runtime_core_string_println(inline2621)
                        var t1421 string = _goml_m_std_p_json_p_encode(jp1417)
                        println__T_string(t1421)
                        return struct{}{}
                    default:
                        var inline2617 string = "invalid stable"
                        var inline2618 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2617)
                        _goml_runtime_core_string_println(inline2618)
                        var t1421 string = _goml_m_std_p_json_p_encode(jp1417)
                        println__T_string(t1421)
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
        var inline2580 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x179)
        _goml_runtime_core_string_println(inline2580)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t1437 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t1437
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1477:
    for {
        var t1478 int
        var inline2641 int = _goml_runtime_core_string_len(x12)
        t1478 = inline2641
        var t1479 bool = index__26 < t1478
        if t1479 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t1481 int = compound_old17 + x16
                index__26 = t1481
                continue
            } else {
                var t1483 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1483
            }
        } else {
            break Loop_loop1477
        }
    }
    var t1476 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t1476
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t1486 int = _goml_runtime_core_string_len(self__38)
    return t1486
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t1489 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t1489
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__36 rune) string {
    var inline2643 uint32 = uint32(rune(self__36))
    var inline2644 bool = utf8_valid_scalar(inline2643)
    if inline2644 {
        var inline2645 string = _goml_runtime_core_char_to_string(self__36)
        return inline2645
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__98 uint8, other__99 uint8) bool {
    var t1495 bool = self__98 == other__99
    return t1495
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline2648 bool = string_is_char_boundary(self__43, start__44)
    var inline2650 bool
    if inline2648 {
        var inline2653 bool = string_is_char_boundary(self__43, end__45)
        inline2650 = inline2653
    } else {
        inline2650 = false
    }
    if inline2650 {
        var inline2651 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline2651
    } else {
        var inline2652 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline2652
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__236 int) *ref_int_x {
    var t1530 *ref_int_x = ref__Ref_3int(value__236)
    return t1530
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__237 *ref_int_x) int {
    var t1533 int = ref_get__Ref_3int(self__237)
    return t1533
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t1536 string = _goml_runtime_core_int_to_string(self__34)
    return t1536
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__238 *ref_int_x, value__239 int) struct{} {
    ref_set__Ref_3int(self__238, value__239)
    return struct{}{}
}

func char_from_uint32(value__32 uint32) Option__char {
    var t1543 bool
    var inline2665 bool = value__32 <= 1114111
    if inline2665 {
        var inline2666 bool = value__32 >= 55296
        var inline2668 bool
        if inline2666 {
            var inline2670 bool = value__32 <= 57343
            inline2668 = inline2670
        } else {
            inline2668 = false
        }
        var inline2669 bool = !inline2668
        t1543 = inline2669
    } else {
        t1543 = false
    }
    if t1543 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t1544 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1544
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t1547 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t1547
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__155 *_goml_vec__goml_m_std_p_json_p_Value, elem__156 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__155, elem__156)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t1552 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t1552
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__155 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__156 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__155, elem__156)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline2672 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline2673 bool = inline2672._0
    var inline2674 rune = inline2672._1
    if inline2673 {
        return inline2674
    } else {
        var inline2678 rune = _goml_runtime_core_string_get("", -1)
        return inline2678
    }
}

func println__T_string(value__31 string) struct{} {
    var t1559 string
    t1559 = value__31
    _goml_runtime_core_string_println(t1559)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1685 bool = index__6 < 0
    var jp1683 bool
    if t1685 {
        jp1683 = true
    } else {
        var t1686 bool = index__6 >= length__7
        jp1683 = t1686
    }
    if jp1683 {
        var inline2685 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2685
    } else {
        var t1570 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1570))
        var t1573 bool = first__8 < 128
        if t1573 {
            var inline2687 int = 1
            var inline2688 Option__char = char_from_uint32(first__8)
            switch inline2688.(type) {
            case Option__char_None:
                var inline2689 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline2689
            case Option__char_Some:
                var inline2690 rune = inline2688.(Option__char_Some)._0
                var inline2692 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline2690,
                    _2: inline2687,
                }
                return inline2692
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1577 bool = first__8 < 194
            if t1577 {
                var inline2694 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline2694
            } else {
                var t1581 bool = first__8 < 224
                if t1581 {
                    var t1594 int = length__7 - index__6
                    var t1595 bool = t1594 < 2
                    if t1595 {
                        var inline2696 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline2696
                    } else {
                        var t1583 int = index__6 + 1
                        var t1584 uint8
                        var inline2710 uint8 = _goml_runtime_core_string_byte_get(value__5, t1583)
                        t1584 = inline2710
                        var second__9 uint32 = uint32(uint8(t1584))
                        var t1587 bool
                        var inline2707 bool = second__9 < 128
                        if inline2707 {
                            t1587 = true
                        } else {
                            var inline2708 bool = second__9 > 191
                            t1587 = inline2708
                        }
                        if t1587 {
                            var inline2698 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2698
                        } else {
                            var t1589_rhs uint32 = 31
                            var t1589 uint32 = first__8 & t1589_rhs
                            var t1590_rhs int = 6
                            var t1590 uint32 = t1589 << t1590_rhs
                            var t1591_rhs uint32 = 63
                            var t1591 uint32 = second__9 & t1591_rhs
                            var t1592 uint32 = t1590 | t1591
                            var inline2700 int = 2
                            var inline2701 Option__char = char_from_uint32(t1592)
                            switch inline2701.(type) {
                            case Option__char_None:
                                var inline2702 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline2702
                            case Option__char_Some:
                                var inline2703 rune = inline2701.(Option__char_Some)._0
                                var inline2705 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline2703,
                                    _2: inline2700,
                                }
                                return inline2705
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1599 bool = first__8 < 240
                    if t1599 {
                        var t1632 int = length__7 - index__6
                        var t1633 bool = t1632 < 3
                        if t1633 {
                            var inline2712 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2712
                        } else {
                            var t1601 int = index__6 + 1
                            var t1602 uint8
                            var inline2727 uint8 = _goml_runtime_core_string_byte_get(value__5, t1601)
                            t1602 = inline2727
                            var second__10 uint32 = uint32(uint8(t1602))
                            var t1603 int = index__6 + 2
                            var t1604 uint8
                            var inline2725 uint8 = _goml_runtime_core_string_byte_get(value__5, t1603)
                            t1604 = inline2725
                            var third__11 uint32 = uint32(uint8(t1604))
                            var t1630 bool = utf8_invalid_continuation(second__10)
                            var jp1625 bool
                            if t1630 {
                                jp1625 = true
                            } else {
                                var inline2714 bool = third__11 < 128
                                if inline2714 {
                                    jp1625 = true
                                } else {
                                    var inline2715 bool = third__11 > 191
                                    jp1625 = inline2715
                                }
                            }
                            var jp1619 bool
                            if jp1625 {
                                jp1619 = true
                            } else {
                                var t1628 bool
                                var inline2717 uint32 = 224
                                var inline2718 bool = first__8 == inline2717
                                t1628 = inline2718
                                if t1628 {
                                    var t1629 bool = second__10 < 160
                                    jp1619 = t1629
                                } else {
                                    jp1619 = false
                                }
                            }
                            var jp1608 bool
                            if jp1619 {
                                jp1608 = true
                            } else {
                                var t1622 bool
                                var inline2720 uint32 = 237
                                var inline2721 bool = first__8 == inline2720
                                t1622 = inline2721
                                if t1622 {
                                    var t1623 bool = second__10 >= 160
                                    jp1608 = t1623
                                } else {
                                    jp1608 = false
                                }
                            }
                            if jp1608 {
                                var inline2723 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2723
                            } else {
                                var t1610_rhs uint32 = 15
                                var t1610 uint32 = first__8 & t1610_rhs
                                var t1611_rhs int = 12
                                var t1611 uint32 = t1610 << t1611_rhs
                                var t1612_rhs uint32 = 63
                                var t1612 uint32 = second__10 & t1612_rhs
                                var t1613_rhs int = 6
                                var t1613 uint32 = t1612 << t1613_rhs
                                var t1614 uint32 = t1611 | t1613
                                var t1615_rhs uint32 = 63
                                var t1615 uint32 = third__11 & t1615_rhs
                                var t1616 uint32 = t1614 | t1615
                                var t1617 Tuple3_4bool_4char_3int = utf8_valid_decode(t1616, 3)
                                return t1617
                            }
                        }
                    } else {
                        var t1637 bool = first__8 < 245
                        if t1637 {
                            var t1678 int = length__7 - index__6
                            var t1679 bool = t1678 < 4
                            if t1679 {
                                var t1680 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1680
                            } else {
                                var t1639 int = index__6 + 1
                                var t1640 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1639)
                                var second__12 uint32 = uint32(uint8(t1640))
                                var t1641 int = index__6 + 2
                                var t1642 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1641)
                                var third__13 uint32 = uint32(uint8(t1642))
                                var t1643 int = index__6 + 3
                                var t1644 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1643)
                                var fourth__14 uint32 = uint32(uint8(t1644))
                                var t1676 bool = utf8_invalid_continuation(second__12)
                                var jp1674 bool
                                if t1676 {
                                    jp1674 = true
                                } else {
                                    var t1677 bool = utf8_invalid_continuation(third__13)
                                    jp1674 = t1677
                                }
                                var jp1668 bool
                                if jp1674 {
                                    jp1668 = true
                                } else {
                                    var t1675 bool = utf8_invalid_continuation(fourth__14)
                                    jp1668 = t1675
                                }
                                var jp1662 bool
                                if jp1668 {
                                    jp1662 = true
                                } else {
                                    var t1671 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t1671 {
                                        var t1672 bool = second__12 < 144
                                        jp1662 = t1672
                                    } else {
                                        jp1662 = false
                                    }
                                }
                                var jp1648 bool
                                if jp1662 {
                                    jp1648 = true
                                } else {
                                    var t1665 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t1665 {
                                        var t1666 bool = second__12 > 143
                                        jp1648 = t1666
                                    } else {
                                        jp1648 = false
                                    }
                                }
                                if jp1648 {
                                    var t1649 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1649
                                } else {
                                    var t1650_rhs uint32 = 7
                                    var t1650 uint32 = first__8 & t1650_rhs
                                    var t1651_rhs int = 18
                                    var t1651 uint32 = t1650 << t1651_rhs
                                    var t1652_rhs uint32 = 63
                                    var t1652 uint32 = second__12 & t1652_rhs
                                    var t1653_rhs int = 12
                                    var t1653 uint32 = t1652 << t1653_rhs
                                    var t1654 uint32 = t1651 | t1653
                                    var t1655_rhs uint32 = 63
                                    var t1655 uint32 = third__13 & t1655_rhs
                                    var t1656_rhs int = 6
                                    var t1656 uint32 = t1655 << t1656_rhs
                                    var t1657 uint32 = t1654 | t1656
                                    var t1658_rhs uint32 = 63
                                    var t1658 uint32 = fourth__14 & t1658_rhs
                                    var t1659 uint32 = t1657 | t1658
                                    var t1660 Tuple3_4bool_4char_3int = utf8_valid_decode(t1659, 4)
                                    return t1660
                                }
                            }
                        } else {
                            var t1681 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1681
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t1691 uint32 = uint32(rune(value__29))
    var t1692 bool
    var inline2729 bool = t1691 <= 1114111
    if inline2729 {
        var inline2730 bool = t1691 >= 55296
        var inline2732 bool
        if inline2730 {
            var inline2734 bool = t1691 <= 57343
            inline2732 = inline2734
        } else {
            inline2732 = false
        }
        var inline2733 bool = !inline2732
        t1692 = inline2733
    } else {
        t1692 = false
    }
    if t1692 {
        var t1693 string = _goml_runtime_core_char_to_string(value__29)
        return t1693
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t1708 bool = index__16 < 0
    var jp1699 bool
    if t1708 {
        jp1699 = true
    } else {
        var t1709 int
        var inline2736 int = _goml_runtime_core_string_len(value__15)
        t1709 = inline2736
        var t1710 bool = index__16 > t1709
        jp1699 = t1710
    }
    if jp1699 {
        return false
    } else {
        var t1702 int
        var inline2745 int = _goml_runtime_core_string_len(value__15)
        t1702 = inline2745
        var t1703 bool
        var inline2743 bool = index__16 == t1702
        t1703 = inline2743
        if t1703 {
            return true
        } else {
            var t1704 uint8
            var inline2741 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1704 = inline2741
            var t1705_rhs uint8 = 192
            var t1705 uint8 = t1704 & t1705_rhs
            var t1706 bool
            var inline2738 uint8 = 128
            var inline2739 bool = t1705 == inline2738
            t1706 = inline2739
            var t1707 bool = !t1706
            return t1707
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t1719 bool = string_is_char_boundary(value__21, start__22)
    var jp1716 bool
    if t1719 {
        var t1720 bool = string_is_char_boundary(value__21, end__23)
        jp1716 = t1720
    } else {
        jp1716 = false
    }
    if jp1716 {
        var t1717 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t1717
    } else {
        var t1718 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t1718
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1727 bool = value__4 <= 1114111
    if t1727 {
        var t1731 bool = value__4 >= 55296
        var jp1729 bool
        if t1731 {
            var t1732 bool = value__4 <= 57343
            jp1729 = t1732
        } else {
            jp1729 = false
        }
        var t1730 bool = !jp1729
        return t1730
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t1742 string = _goml_runtime_core_int_to_string(self__69)
    return t1742
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t1745 string = _goml_runtime_core_bool_to_string(self__66)
    return t1745
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1748 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1748
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field2782 rune
    var inline2749 bool = utf8_valid_scalar(value__0)
    if inline2749 {
        var inline2750 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline2752 rune = inline2750._1
        commute_field2782 = inline2752
        var t1754 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field2782,
            _2: width__1,
        }
        return t1754
    } else {
        var inline2747 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2747
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1759 bool = value__3 < 128
    if t1759 {
        return true
    } else {
        var t1760 bool = value__3 > 191
        return t1760
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t1763 bool = self__102 == other__103
    return t1763
}

func main() {
    main0()
}
