package main

import (
    _goml_fmt "fmt"
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

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
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

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_4char_3int struct {
    _0 rune
    _1 int
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Ordering int32

type _goml_m_Option_____o_char_c_int_q_ interface {
    is_goml_m_Option_____o_char_c_int_q_()
}

type _goml_m_Option_____o_char_c_int_q__None struct {}

func (_ _goml_m_Option_____o_char_c_int_q__None) is_goml_m_Option_____o_char_c_int_q_() {}

type _goml_m_Option_____o_char_c_int_q__Some struct {
    _0 Tuple2_4char_3int
}

func (_ _goml_m_Option_____o_char_c_int_q__Some) is_goml_m_Option_____o_char_c_int_q_() {}

type Option__char interface {
    isOption__char()
}

type Option__char_None struct {}

func (_ Option__char_None) isOption__char() {}

type Option__char_Some struct {
    _0 rune
}

func (_ Option__char_Some) isOption__char() {}

func check_utf8(bytes__0 *_goml_vec_uint8, expected__1 bool) struct{} {
    var expected_length__2 int
    var inline729 int = vec_len__Vec_5uint8(bytes__0)
    expected_length__2 = inline729
    var mtmp408 Tuple2_4bool_6string = string_from_utf8(bytes__0)
    var x409 bool = mtmp408._0
    var x410 string = mtmp408._1
    var t513 bool = x409 == expected__1
    var inline726 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t513)
    _goml_runtime_core_string_println(inline726)
    if x409 {
        var t515 int
        var inline721 int = _goml_runtime_core_string_len(x410)
        t515 = inline721
        var t516 bool = t515 == expected_length__2
        var inline718 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t516)
        _goml_runtime_core_string_println(inline718)
        return struct{}{}
    } else {
        var t518 bool = x410 == ""
        var inline723 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t518)
        _goml_runtime_core_string_println(inline723)
        return struct{}{}
    }
}

func check_scalar(bytes__5 *_goml_vec_uint8, expected__6 uint32, expected_width__7 int) struct{} {
    var mtmp412 Tuple2_4bool_6string = string_from_utf8(bytes__5)
    var x413 bool = mtmp412._0
    var x414 string = mtmp412._1
    var inline756 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x413)
    _goml_runtime_core_string_println(inline756)
    var commute_field872 Tuple2_4char_3int
    var inline745 int = 0
    var inline746 Tuple3_4bool_4char_3int = string_decode_utf8_at(x414, inline745)
    var inline747 bool = inline746._0
    var inline748 rune = inline746._1
    var inline749 int = inline746._2
    if inline747 {
        var inline753 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline748,
            _1: inline749,
        }
        commute_field872 = inline753
        var x420 rune = commute_field872._0
        var x421 int = commute_field872._1
        var t523 uint32 = uint32(rune(x420))
        var t524 bool = t523 == expected__6
        var inline742 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t524)
        _goml_runtime_core_string_println(inline742)
        var t525 bool = x421 == expected_width__7
        var inline739 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t525)
        _goml_runtime_core_string_println(inline739)
        return struct{}{}
    } else {
        var inline735 bool = false
        var inline736 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline735)
        _goml_runtime_core_string_println(inline736)
        var inline731 bool = false
        var inline732 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline731)
        _goml_runtime_core_string_println(inline732)
        return struct{}{}
    }
}

func main0() struct{} {
    var vec_literal__795 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    check_utf8(vec_literal__795, true)
    var vec_literal__826 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__826, 0)
    check_utf8(vec_literal__826, true)
    var vec_literal__858 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__858, 127)
    check_utf8(vec_literal__858, true)
    var vec_literal__894 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__894, 194)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__894, 128)
    check_scalar(vec_literal__894, 128, 2)
    var vec_literal__937 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__937, 223)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__937, 191)
    check_scalar(vec_literal__937, 2047, 2)
    var vec_literal__981 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__981, 224)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__981, 160)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__981, 128)
    check_scalar(vec_literal__981, 2048, 3)
    var vec_literal__1030 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1030, 237)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1030, 159)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1030, 191)
    check_scalar(vec_literal__1030, 55295, 3)
    var vec_literal__1080 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1080, 238)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1080, 128)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1080, 128)
    check_scalar(vec_literal__1080, 57344, 3)
    var vec_literal__1130 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1130, 239)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1130, 191)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1130, 189)
    check_scalar(vec_literal__1130, 65533, 3)
    var vec_literal__1180 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1180, 239)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1180, 191)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1180, 191)
    check_scalar(vec_literal__1180, 65535, 3)
    var vec_literal__1230 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1230, 240)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1230, 144)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1230, 128)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1230, 128)
    check_scalar(vec_literal__1230, 65536, 4)
    var vec_literal__1285 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1285, 244)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1285, 143)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1285, 191)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1285, 191)
    check_scalar(vec_literal__1285, 1114111, 4)
    var vec_literal__1340 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1340, 128)
    check_utf8(vec_literal__1340, false)
    var vec_literal__1375 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1375, 191)
    check_utf8(vec_literal__1375, false)
    var vec_literal__1410 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1410, 192)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1410, 128)
    check_utf8(vec_literal__1410, false)
    var vec_literal__1450 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1450, 193)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1450, 191)
    check_utf8(vec_literal__1450, false)
    var vec_literal__1490 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1490, 194)
    check_utf8(vec_literal__1490, false)
    var vec_literal__1525 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1525, 194)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1525, 127)
    check_utf8(vec_literal__1525, false)
    var vec_literal__1565 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1565, 224)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1565, 159)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1565, 191)
    check_utf8(vec_literal__1565, false)
    var vec_literal__1610 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1610, 225)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1610, 128)
    check_utf8(vec_literal__1610, false)
    var vec_literal__1650 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1650, 225)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1650, 128)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1650, 127)
    check_utf8(vec_literal__1650, false)
    var vec_literal__1695 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1695, 237)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1695, 160)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1695, 128)
    check_utf8(vec_literal__1695, false)
    var vec_literal__1740 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1740, 240)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1740, 143)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1740, 191)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1740, 191)
    check_utf8(vec_literal__1740, false)
    var vec_literal__1790 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1790, 240)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1790, 144)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1790, 128)
    check_utf8(vec_literal__1790, false)
    var vec_literal__1835 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1835, 244)
    var inline809 uint8 = 144
    vec_push__Vec_5uint8(vec_literal__1835, inline809)
    var inline806 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1835, inline806)
    var inline803 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1835, inline803)
    var inline788 bool = false
    var inline789 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1835)
    var inline790 Tuple2_4bool_6string = string_from_utf8(vec_literal__1835)
    var inline791 bool = inline790._0
    var inline792 string = inline790._1
    var inline795 bool = inline791 == inline788
    println__T_bool(inline795)
    if inline791 {
        var inline797 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline792)
        var inline798 bool = inline797 == inline789
        println__T_bool(inline798)
    } else {
        var inline800 bool = inline792 == ""
        println__T_bool(inline800)
    }
    var vec_literal__1885 *_goml_vec_uint8
    var inline786 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__1885 = inline786
    var inline783 uint8 = 245
    vec_push__Vec_5uint8(vec_literal__1885, inline783)
    var inline780 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline780)
    var inline777 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline777)
    var inline774 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline774)
    var inline759 bool = false
    var inline760 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1885)
    var inline761 Tuple2_4bool_6string = string_from_utf8(vec_literal__1885)
    var inline762 bool = inline761._0
    var inline763 string = inline761._1
    var inline766 bool = inline762 == inline759
    println__T_bool(inline766)
    if inline762 {
        var inline768 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline763)
        var inline769 bool = inline768 == inline760
        println__T_bool(inline769)
        return struct{}{}
    } else {
        var inline771 bool = inline763 == ""
        println__T_bool(inline771)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__273 *_goml_vec_uint8) int {
    var t531 int = vec_len__Vec_5uint8(self__273)
    return t531
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop536:
    for {
        var t537 int
        var inline812 int = _goml_runtime_core_string_len(x12)
        t537 = inline812
        var t538 bool = index__26 < t537
        if t538 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t540 int = compound_old17 + x16
                index__26 = t540
                continue
            } else {
                var t542 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t542
            }
        } else {
            break Loop_loop536
        }
    }
    var t535 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t535
}

func println__T_bool(value__1 bool) struct{} {
    var t544 string
    var inline814 string = _goml_runtime_core_bool_to_string(value__1)
    t544 = inline814
    _goml_runtime_core_string_println(t544)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t548 int = _goml_runtime_core_string_len(self__36)
    return t548
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t557 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t557
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__258 *_goml_vec_uint8, elem__259 uint8) struct{} {
    vec_push__Vec_5uint8(self__258, elem__259)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t678 bool = index__6 < 0
    var jp676 bool
    if t678 {
        jp676 = true
    } else {
        var t679 bool = index__6 >= length__7
        jp676 = t679
    }
    if jp676 {
        var inline816 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline816
    } else {
        var t563 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t563))
        var t566 bool = first__8 < 128
        if t566 {
            var inline818 int = 1
            var inline819 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline819.(type) {
            case Option__char_None:
                var inline820 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline820
            case Option__char_Some:
                var inline821 rune = inline819.(Option__char_Some)._0
                var inline823 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline821,
                    _2: inline818,
                }
                return inline823
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t570 bool = first__8 < 194
            if t570 {
                var inline825 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline825
            } else {
                var t574 bool = first__8 < 224
                if t574 {
                    var t587 int = length__7 - index__6
                    var t588 bool = t587 < 2
                    if t588 {
                        var inline827 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline827
                    } else {
                        var t576 int = index__6 + 1
                        var t577 uint8
                        var inline841 uint8 = _goml_runtime_core_string_byte_get(value__5, t576)
                        t577 = inline841
                        var second__9 uint32 = uint32(uint8(t577))
                        var t580 bool
                        var inline838 bool = second__9 < 128
                        if inline838 {
                            t580 = true
                        } else {
                            var inline839 bool = second__9 > 191
                            t580 = inline839
                        }
                        if t580 {
                            var inline829 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline829
                        } else {
                            var t582_rhs uint32 = 31
                            var t582 uint32 = first__8 & t582_rhs
                            var t583_rhs int = 6
                            var t583 uint32 = t582 << t583_rhs
                            var t584_rhs uint32 = 63
                            var t584 uint32 = second__9 & t584_rhs
                            var t585 uint32 = t583 | t584
                            var inline831 int = 2
                            var inline832 Option__char = __goml_builtin_char_from_uint32(t585)
                            switch inline832.(type) {
                            case Option__char_None:
                                var inline833 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline833
                            case Option__char_Some:
                                var inline834 rune = inline832.(Option__char_Some)._0
                                var inline836 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline834,
                                    _2: inline831,
                                }
                                return inline836
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t592 bool = first__8 < 240
                    if t592 {
                        var t625 int = length__7 - index__6
                        var t626 bool = t625 < 3
                        if t626 {
                            var inline843 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline843
                        } else {
                            var t594 int = index__6 + 1
                            var t595 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t594)
                            var second__10 uint32 = uint32(uint8(t595))
                            var t596 int = index__6 + 2
                            var t597 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t596)
                            var third__11 uint32 = uint32(uint8(t597))
                            var t623 bool = utf8_invalid_continuation(second__10)
                            var jp618 bool
                            if t623 {
                                jp618 = true
                            } else {
                                var inline845 bool = third__11 < 128
                                if inline845 {
                                    jp618 = true
                                } else {
                                    var inline846 bool = third__11 > 191
                                    jp618 = inline846
                                }
                            }
                            var jp612 bool
                            if jp618 {
                                jp612 = true
                            } else {
                                var t621 bool = first__8 == 224
                                if t621 {
                                    var t622 bool = second__10 < 160
                                    jp612 = t622
                                } else {
                                    jp612 = false
                                }
                            }
                            var jp601 bool
                            if jp612 {
                                jp601 = true
                            } else {
                                var t615 bool = first__8 == 237
                                if t615 {
                                    var t616 bool = second__10 >= 160
                                    jp601 = t616
                                } else {
                                    jp601 = false
                                }
                            }
                            if jp601 {
                                var inline848 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline848
                            } else {
                                var t603_rhs uint32 = 15
                                var t603 uint32 = first__8 & t603_rhs
                                var t604_rhs int = 12
                                var t604 uint32 = t603 << t604_rhs
                                var t605_rhs uint32 = 63
                                var t605 uint32 = second__10 & t605_rhs
                                var t606_rhs int = 6
                                var t606 uint32 = t605 << t606_rhs
                                var t607 uint32 = t604 | t606
                                var t608_rhs uint32 = 63
                                var t608 uint32 = third__11 & t608_rhs
                                var t609 uint32 = t607 | t608
                                var inline850 int = 3
                                var inline851 Option__char = __goml_builtin_char_from_uint32(t609)
                                switch inline851.(type) {
                                case Option__char_None:
                                    var inline852 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline852
                                case Option__char_Some:
                                    var inline853 rune = inline851.(Option__char_Some)._0
                                    var inline855 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline853,
                                        _2: inline850,
                                    }
                                    return inline855
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t630 bool = first__8 < 245
                        if t630 {
                            var t671 int = length__7 - index__6
                            var t672 bool = t671 < 4
                            if t672 {
                                var t673 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t673
                            } else {
                                var t632 int = index__6 + 1
                                var t633 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t632)
                                var second__12 uint32 = uint32(uint8(t633))
                                var t634 int = index__6 + 2
                                var t635 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t634)
                                var third__13 uint32 = uint32(uint8(t635))
                                var t636 int = index__6 + 3
                                var t637 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t636)
                                var fourth__14 uint32 = uint32(uint8(t637))
                                var t669 bool = utf8_invalid_continuation(second__12)
                                var jp667 bool
                                if t669 {
                                    jp667 = true
                                } else {
                                    var t670 bool = utf8_invalid_continuation(third__13)
                                    jp667 = t670
                                }
                                var jp661 bool
                                if jp667 {
                                    jp661 = true
                                } else {
                                    var t668 bool = utf8_invalid_continuation(fourth__14)
                                    jp661 = t668
                                }
                                var jp655 bool
                                if jp661 {
                                    jp655 = true
                                } else {
                                    var t664 bool = first__8 == 240
                                    if t664 {
                                        var t665 bool = second__12 < 144
                                        jp655 = t665
                                    } else {
                                        jp655 = false
                                    }
                                }
                                var jp641 bool
                                if jp655 {
                                    jp641 = true
                                } else {
                                    var t658 bool = first__8 == 244
                                    if t658 {
                                        var t659 bool = second__12 > 143
                                        jp641 = t659
                                    } else {
                                        jp641 = false
                                    }
                                }
                                if jp641 {
                                    var t642 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t642
                                } else {
                                    var t643_rhs uint32 = 7
                                    var t643 uint32 = first__8 & t643_rhs
                                    var t644_rhs int = 18
                                    var t644 uint32 = t643 << t644_rhs
                                    var t645_rhs uint32 = 63
                                    var t645 uint32 = second__12 & t645_rhs
                                    var t646_rhs int = 12
                                    var t646 uint32 = t645 << t646_rhs
                                    var t647 uint32 = t644 | t646
                                    var t648_rhs uint32 = 63
                                    var t648 uint32 = third__13 & t648_rhs
                                    var t649_rhs int = 6
                                    var t649 uint32 = t648 << t649_rhs
                                    var t650 uint32 = t647 | t649
                                    var t651_rhs uint32 = 63
                                    var t651 uint32 = fourth__14 & t651_rhs
                                    var t652 uint32 = t650 | t651
                                    var t653 Tuple3_4bool_4char_3int = utf8_valid_decode(t652, 4)
                                    return t653
                                }
                            }
                        } else {
                            var t674 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t674
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t682 string = _goml_runtime_core_bool_to_string(self__148)
    return t682
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t685 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t685
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t688 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t688
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field875 rune
    var inline859 bool = utf8_valid_scalar(value__0)
    if inline859 {
        var inline860 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline861 rune = inline860._1
        commute_field875 = inline861
        var t694 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field875,
            _2: width__1,
        }
        return t694
    } else {
        var inline857 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline857
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t699 bool = value__3 < 128
    if t699 {
        return true
    } else {
        var t700 bool = value__3 > 191
        return t700
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t705 bool
    var inline865 bool = value__30 <= 1114111
    if inline865 {
        var inline866 bool = value__30 >= 55296
        var inline868 bool
        if inline866 {
            var inline870 bool = value__30 <= 57343
            inline868 = inline870
        } else {
            inline868 = false
        }
        var inline869 bool = !inline868
        t705 = inline869
    } else {
        t705 = false
    }
    if t705 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t706 Option__char = Option__char_Some{
            _0: x24,
        }
        return t706
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t711 bool = value__4 <= 1114111
    if t711 {
        var t715 bool = value__4 >= 55296
        var jp713 bool
        if t715 {
            var t716 bool = value__4 <= 57343
            jp713 = t716
        } else {
            jp713 = false
        }
        var t714 bool = !jp713
        return t714
    } else {
        return false
    }
}

func main() {
    main0()
}
