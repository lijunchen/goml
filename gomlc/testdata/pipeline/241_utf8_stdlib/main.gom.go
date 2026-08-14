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
    var inline508 int = vec_len__Vec_5uint8(bytes__0)
    expected_length__2 = inline508
    var mtmp187 Tuple2_4bool_6string = string_from_utf8(bytes__0)
    var x188 bool = mtmp187._0
    var x189 string = mtmp187._1
    var t292 bool = x188 == expected__1
    var inline505 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t292)
    _goml_runtime_core_string_println(inline505)
    if x188 {
        var t294 int
        var inline500 int = _goml_runtime_core_string_len(x189)
        t294 = inline500
        var t295 bool = t294 == expected_length__2
        var inline497 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t295)
        _goml_runtime_core_string_println(inline497)
        return struct{}{}
    } else {
        var t297 bool = x189 == ""
        var inline502 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t297)
        _goml_runtime_core_string_println(inline502)
        return struct{}{}
    }
}

func check_scalar(bytes__5 *_goml_vec_uint8, expected__6 uint32, expected_width__7 int) struct{} {
    var mtmp191 Tuple2_4bool_6string = string_from_utf8(bytes__5)
    var x192 bool = mtmp191._0
    var x193 string = mtmp191._1
    var inline535 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x192)
    _goml_runtime_core_string_println(inline535)
    var commute_field651 Tuple2_4char_3int
    var inline524 int = 0
    var inline525 Tuple3_4bool_4char_3int = string_decode_utf8_at(x193, inline524)
    var inline526 bool = inline525._0
    var inline527 rune = inline525._1
    var inline528 int = inline525._2
    if inline526 {
        var inline532 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline527,
            _1: inline528,
        }
        commute_field651 = inline532
        var x199 rune = commute_field651._0
        var x200 int = commute_field651._1
        var t302 uint32 = uint32(rune(x199))
        var t303 bool = t302 == expected__6
        var inline521 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t303)
        _goml_runtime_core_string_println(inline521)
        var t304 bool = x200 == expected_width__7
        var inline518 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t304)
        _goml_runtime_core_string_println(inline518)
        return struct{}{}
    } else {
        var inline514 bool = false
        var inline515 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline514)
        _goml_runtime_core_string_println(inline515)
        var inline510 bool = false
        var inline511 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline510)
        _goml_runtime_core_string_println(inline511)
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
    var inline588 uint8 = 144
    vec_push__Vec_5uint8(vec_literal__1835, inline588)
    var inline585 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1835, inline585)
    var inline582 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1835, inline582)
    var inline567 bool = false
    var inline568 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1835)
    var inline569 Tuple2_4bool_6string = string_from_utf8(vec_literal__1835)
    var inline570 bool = inline569._0
    var inline571 string = inline569._1
    var inline574 bool = inline570 == inline567
    println__T_bool(inline574)
    if inline570 {
        var inline576 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline571)
        var inline577 bool = inline576 == inline568
        println__T_bool(inline577)
    } else {
        var inline579 bool = inline571 == ""
        println__T_bool(inline579)
    }
    var vec_literal__1885 *_goml_vec_uint8
    var inline565 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__1885 = inline565
    var inline562 uint8 = 245
    vec_push__Vec_5uint8(vec_literal__1885, inline562)
    var inline559 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline559)
    var inline556 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline556)
    var inline553 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline553)
    var inline538 bool = false
    var inline539 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1885)
    var inline540 Tuple2_4bool_6string = string_from_utf8(vec_literal__1885)
    var inline541 bool = inline540._0
    var inline542 string = inline540._1
    var inline545 bool = inline541 == inline538
    println__T_bool(inline545)
    if inline541 {
        var inline547 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline542)
        var inline548 bool = inline547 == inline539
        println__T_bool(inline548)
        return struct{}{}
    } else {
        var inline550 bool = inline542 == ""
        println__T_bool(inline550)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__189 *_goml_vec_uint8) int {
    var t310 int = vec_len__Vec_5uint8(self__189)
    return t310
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop315:
    for {
        var t316 int
        var inline591 int = _goml_runtime_core_string_len(x12)
        t316 = inline591
        var t317 bool = index__26 < t316
        if t317 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t319 int = compound_old17 + x16
                index__26 = t319
                continue
            } else {
                var t321 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t321
            }
        } else {
            break Loop_loop315
        }
    }
    var t314 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t314
}

func println__T_bool(value__1 bool) struct{} {
    var t323 string
    var inline593 string = _goml_runtime_core_bool_to_string(value__1)
    t323 = inline593
    _goml_runtime_core_string_println(t323)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t327 int = _goml_runtime_core_string_len(self__36)
    return t327
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t336 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t336
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__174 *_goml_vec_uint8, elem__175 uint8) struct{} {
    vec_push__Vec_5uint8(self__174, elem__175)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t457 bool = index__6 < 0
    var jp455 bool
    if t457 {
        jp455 = true
    } else {
        var t458 bool = index__6 >= length__7
        jp455 = t458
    }
    if jp455 {
        var inline595 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline595
    } else {
        var t342 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t342))
        var t345 bool = first__8 < 128
        if t345 {
            var inline597 int = 1
            var inline598 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline598.(type) {
            case Option__char_None:
                var inline599 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline599
            case Option__char_Some:
                var inline600 rune = inline598.(Option__char_Some)._0
                var inline602 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline600,
                    _2: inline597,
                }
                return inline602
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t349 bool = first__8 < 194
            if t349 {
                var inline604 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline604
            } else {
                var t353 bool = first__8 < 224
                if t353 {
                    var t366 int = length__7 - index__6
                    var t367 bool = t366 < 2
                    if t367 {
                        var inline606 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline606
                    } else {
                        var t355 int = index__6 + 1
                        var t356 uint8
                        var inline620 uint8 = _goml_runtime_core_string_byte_get(value__5, t355)
                        t356 = inline620
                        var second__9 uint32 = uint32(uint8(t356))
                        var t359 bool
                        var inline617 bool = second__9 < 128
                        if inline617 {
                            t359 = true
                        } else {
                            var inline618 bool = second__9 > 191
                            t359 = inline618
                        }
                        if t359 {
                            var inline608 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline608
                        } else {
                            var t361_rhs uint32 = 31
                            var t361 uint32 = first__8 & t361_rhs
                            var t362_rhs int = 6
                            var t362 uint32 = t361 << t362_rhs
                            var t363_rhs uint32 = 63
                            var t363 uint32 = second__9 & t363_rhs
                            var t364 uint32 = t362 | t363
                            var inline610 int = 2
                            var inline611 Option__char = __goml_builtin_char_from_uint32(t364)
                            switch inline611.(type) {
                            case Option__char_None:
                                var inline612 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline612
                            case Option__char_Some:
                                var inline613 rune = inline611.(Option__char_Some)._0
                                var inline615 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline613,
                                    _2: inline610,
                                }
                                return inline615
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t371 bool = first__8 < 240
                    if t371 {
                        var t404 int = length__7 - index__6
                        var t405 bool = t404 < 3
                        if t405 {
                            var inline622 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline622
                        } else {
                            var t373 int = index__6 + 1
                            var t374 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t373)
                            var second__10 uint32 = uint32(uint8(t374))
                            var t375 int = index__6 + 2
                            var t376 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t375)
                            var third__11 uint32 = uint32(uint8(t376))
                            var t402 bool = utf8_invalid_continuation(second__10)
                            var jp397 bool
                            if t402 {
                                jp397 = true
                            } else {
                                var inline624 bool = third__11 < 128
                                if inline624 {
                                    jp397 = true
                                } else {
                                    var inline625 bool = third__11 > 191
                                    jp397 = inline625
                                }
                            }
                            var jp391 bool
                            if jp397 {
                                jp391 = true
                            } else {
                                var t400 bool = first__8 == 224
                                if t400 {
                                    var t401 bool = second__10 < 160
                                    jp391 = t401
                                } else {
                                    jp391 = false
                                }
                            }
                            var jp380 bool
                            if jp391 {
                                jp380 = true
                            } else {
                                var t394 bool = first__8 == 237
                                if t394 {
                                    var t395 bool = second__10 >= 160
                                    jp380 = t395
                                } else {
                                    jp380 = false
                                }
                            }
                            if jp380 {
                                var inline627 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline627
                            } else {
                                var t382_rhs uint32 = 15
                                var t382 uint32 = first__8 & t382_rhs
                                var t383_rhs int = 12
                                var t383 uint32 = t382 << t383_rhs
                                var t384_rhs uint32 = 63
                                var t384 uint32 = second__10 & t384_rhs
                                var t385_rhs int = 6
                                var t385 uint32 = t384 << t385_rhs
                                var t386 uint32 = t383 | t385
                                var t387_rhs uint32 = 63
                                var t387 uint32 = third__11 & t387_rhs
                                var t388 uint32 = t386 | t387
                                var inline629 int = 3
                                var inline630 Option__char = __goml_builtin_char_from_uint32(t388)
                                switch inline630.(type) {
                                case Option__char_None:
                                    var inline631 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline631
                                case Option__char_Some:
                                    var inline632 rune = inline630.(Option__char_Some)._0
                                    var inline634 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline632,
                                        _2: inline629,
                                    }
                                    return inline634
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t409 bool = first__8 < 245
                        if t409 {
                            var t450 int = length__7 - index__6
                            var t451 bool = t450 < 4
                            if t451 {
                                var t452 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t452
                            } else {
                                var t411 int = index__6 + 1
                                var t412 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t411)
                                var second__12 uint32 = uint32(uint8(t412))
                                var t413 int = index__6 + 2
                                var t414 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t413)
                                var third__13 uint32 = uint32(uint8(t414))
                                var t415 int = index__6 + 3
                                var t416 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t415)
                                var fourth__14 uint32 = uint32(uint8(t416))
                                var t448 bool = utf8_invalid_continuation(second__12)
                                var jp446 bool
                                if t448 {
                                    jp446 = true
                                } else {
                                    var t449 bool = utf8_invalid_continuation(third__13)
                                    jp446 = t449
                                }
                                var jp440 bool
                                if jp446 {
                                    jp440 = true
                                } else {
                                    var t447 bool = utf8_invalid_continuation(fourth__14)
                                    jp440 = t447
                                }
                                var jp434 bool
                                if jp440 {
                                    jp434 = true
                                } else {
                                    var t443 bool = first__8 == 240
                                    if t443 {
                                        var t444 bool = second__12 < 144
                                        jp434 = t444
                                    } else {
                                        jp434 = false
                                    }
                                }
                                var jp420 bool
                                if jp434 {
                                    jp420 = true
                                } else {
                                    var t437 bool = first__8 == 244
                                    if t437 {
                                        var t438 bool = second__12 > 143
                                        jp420 = t438
                                    } else {
                                        jp420 = false
                                    }
                                }
                                if jp420 {
                                    var t421 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t421
                                } else {
                                    var t422_rhs uint32 = 7
                                    var t422 uint32 = first__8 & t422_rhs
                                    var t423_rhs int = 18
                                    var t423 uint32 = t422 << t423_rhs
                                    var t424_rhs uint32 = 63
                                    var t424 uint32 = second__12 & t424_rhs
                                    var t425_rhs int = 12
                                    var t425 uint32 = t424 << t425_rhs
                                    var t426 uint32 = t423 | t425
                                    var t427_rhs uint32 = 63
                                    var t427 uint32 = third__13 & t427_rhs
                                    var t428_rhs int = 6
                                    var t428 uint32 = t427 << t428_rhs
                                    var t429 uint32 = t426 | t428
                                    var t430_rhs uint32 = 63
                                    var t430 uint32 = fourth__14 & t430_rhs
                                    var t431 uint32 = t429 | t430
                                    var t432 Tuple3_4bool_4char_3int = utf8_valid_decode(t431, 4)
                                    return t432
                                }
                            }
                        } else {
                            var t453 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t453
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t461 string = _goml_runtime_core_bool_to_string(self__64)
    return t461
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t464 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t464
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t467 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t467
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field654 rune
    var inline638 bool = utf8_valid_scalar(value__0)
    if inline638 {
        var inline639 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline640 rune = inline639._1
        commute_field654 = inline640
        var t473 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field654,
            _2: width__1,
        }
        return t473
    } else {
        var inline636 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline636
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t478 bool = value__3 < 128
    if t478 {
        return true
    } else {
        var t479 bool = value__3 > 191
        return t479
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t484 bool
    var inline644 bool = value__30 <= 1114111
    if inline644 {
        var inline645 bool = value__30 >= 55296
        var inline647 bool
        if inline645 {
            var inline649 bool = value__30 <= 57343
            inline647 = inline649
        } else {
            inline647 = false
        }
        var inline648 bool = !inline647
        t484 = inline648
    } else {
        t484 = false
    }
    if t484 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t485 Option__char = Option__char_Some{
            _0: x24,
        }
        return t485
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t490 bool = value__4 <= 1114111
    if t490 {
        var t494 bool = value__4 >= 55296
        var jp492 bool
        if t494 {
            var t495 bool = value__4 <= 57343
            jp492 = t495
        } else {
            jp492 = false
        }
        var t493 bool = !jp492
        return t493
    } else {
        return false
    }
}

func main() {
    main0()
}
