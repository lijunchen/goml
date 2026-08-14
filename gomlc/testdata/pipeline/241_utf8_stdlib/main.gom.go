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
    var inline503 int = vec_len__Vec_5uint8(bytes__0)
    expected_length__2 = inline503
    var mtmp182 Tuple2_4bool_6string = string_from_utf8(bytes__0)
    var x183 bool = mtmp182._0
    var x184 string = mtmp182._1
    var t287 bool = x183 == expected__1
    var inline500 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t287)
    _goml_runtime_core_string_println(inline500)
    if x183 {
        var t289 int
        var inline495 int = _goml_runtime_core_string_len(x184)
        t289 = inline495
        var t290 bool = t289 == expected_length__2
        var inline492 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t290)
        _goml_runtime_core_string_println(inline492)
        return struct{}{}
    } else {
        var t292 bool = x184 == ""
        var inline497 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t292)
        _goml_runtime_core_string_println(inline497)
        return struct{}{}
    }
}

func check_scalar(bytes__5 *_goml_vec_uint8, expected__6 uint32, expected_width__7 int) struct{} {
    var mtmp186 Tuple2_4bool_6string = string_from_utf8(bytes__5)
    var x187 bool = mtmp186._0
    var x188 string = mtmp186._1
    var inline530 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x187)
    _goml_runtime_core_string_println(inline530)
    var commute_field646 Tuple2_4char_3int
    var inline519 int = 0
    var inline520 Tuple3_4bool_4char_3int = string_decode_utf8_at(x188, inline519)
    var inline521 bool = inline520._0
    var inline522 rune = inline520._1
    var inline523 int = inline520._2
    if inline521 {
        var inline527 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline522,
            _1: inline523,
        }
        commute_field646 = inline527
        var x194 rune = commute_field646._0
        var x195 int = commute_field646._1
        var t297 uint32 = uint32(rune(x194))
        var t298 bool = t297 == expected__6
        var inline516 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t298)
        _goml_runtime_core_string_println(inline516)
        var t299 bool = x195 == expected_width__7
        var inline513 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t299)
        _goml_runtime_core_string_println(inline513)
        return struct{}{}
    } else {
        var inline509 bool = false
        var inline510 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline509)
        _goml_runtime_core_string_println(inline510)
        var inline505 bool = false
        var inline506 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline505)
        _goml_runtime_core_string_println(inline506)
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
    var inline583 uint8 = 144
    vec_push__Vec_5uint8(vec_literal__1835, inline583)
    var inline580 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1835, inline580)
    var inline577 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1835, inline577)
    var inline562 bool = false
    var inline563 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1835)
    var inline564 Tuple2_4bool_6string = string_from_utf8(vec_literal__1835)
    var inline565 bool = inline564._0
    var inline566 string = inline564._1
    var inline569 bool = inline565 == inline562
    println__T_bool(inline569)
    if inline565 {
        var inline571 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline566)
        var inline572 bool = inline571 == inline563
        println__T_bool(inline572)
    } else {
        var inline574 bool = inline566 == ""
        println__T_bool(inline574)
    }
    var vec_literal__1885 *_goml_vec_uint8
    var inline560 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__1885 = inline560
    var inline557 uint8 = 245
    vec_push__Vec_5uint8(vec_literal__1885, inline557)
    var inline554 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline554)
    var inline551 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline551)
    var inline548 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline548)
    var inline533 bool = false
    var inline534 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1885)
    var inline535 Tuple2_4bool_6string = string_from_utf8(vec_literal__1885)
    var inline536 bool = inline535._0
    var inline537 string = inline535._1
    var inline540 bool = inline536 == inline533
    println__T_bool(inline540)
    if inline536 {
        var inline542 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline537)
        var inline543 bool = inline542 == inline534
        println__T_bool(inline543)
        return struct{}{}
    } else {
        var inline545 bool = inline537 == ""
        println__T_bool(inline545)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__189 *_goml_vec_uint8) int {
    var t305 int = vec_len__Vec_5uint8(self__189)
    return t305
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop310:
    for {
        var t311 int
        var inline586 int = _goml_runtime_core_string_len(x12)
        t311 = inline586
        var t312 bool = index__26 < t311
        if t312 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t314 int = compound_old17 + x16
                index__26 = t314
                continue
            } else {
                var t316 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t316
            }
        } else {
            break Loop_loop310
        }
    }
    var t309 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t309
}

func println__T_bool(value__1 bool) struct{} {
    var t318 string
    var inline588 string = _goml_runtime_core_bool_to_string(value__1)
    t318 = inline588
    _goml_runtime_core_string_println(t318)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t322 int = _goml_runtime_core_string_len(self__36)
    return t322
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t331 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t331
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__174 *_goml_vec_uint8, elem__175 uint8) struct{} {
    vec_push__Vec_5uint8(self__174, elem__175)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t452 bool = index__6 < 0
    var jp450 bool
    if t452 {
        jp450 = true
    } else {
        var t453 bool = index__6 >= length__7
        jp450 = t453
    }
    if jp450 {
        var inline590 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline590
    } else {
        var t337 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t337))
        var t340 bool = first__8 < 128
        if t340 {
            var inline592 int = 1
            var inline593 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline593.(type) {
            case Option__char_None:
                var inline594 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline594
            case Option__char_Some:
                var inline595 rune = inline593.(Option__char_Some)._0
                var inline597 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline595,
                    _2: inline592,
                }
                return inline597
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t344 bool = first__8 < 194
            if t344 {
                var inline599 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline599
            } else {
                var t348 bool = first__8 < 224
                if t348 {
                    var t361 int = length__7 - index__6
                    var t362 bool = t361 < 2
                    if t362 {
                        var inline601 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline601
                    } else {
                        var t350 int = index__6 + 1
                        var t351 uint8
                        var inline615 uint8 = _goml_runtime_core_string_byte_get(value__5, t350)
                        t351 = inline615
                        var second__9 uint32 = uint32(uint8(t351))
                        var t354 bool
                        var inline612 bool = second__9 < 128
                        if inline612 {
                            t354 = true
                        } else {
                            var inline613 bool = second__9 > 191
                            t354 = inline613
                        }
                        if t354 {
                            var inline603 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline603
                        } else {
                            var t356_rhs uint32 = 31
                            var t356 uint32 = first__8 & t356_rhs
                            var t357_rhs int = 6
                            var t357 uint32 = t356 << t357_rhs
                            var t358_rhs uint32 = 63
                            var t358 uint32 = second__9 & t358_rhs
                            var t359 uint32 = t357 | t358
                            var inline605 int = 2
                            var inline606 Option__char = __goml_builtin_char_from_uint32(t359)
                            switch inline606.(type) {
                            case Option__char_None:
                                var inline607 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline607
                            case Option__char_Some:
                                var inline608 rune = inline606.(Option__char_Some)._0
                                var inline610 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline608,
                                    _2: inline605,
                                }
                                return inline610
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t366 bool = first__8 < 240
                    if t366 {
                        var t399 int = length__7 - index__6
                        var t400 bool = t399 < 3
                        if t400 {
                            var inline617 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline617
                        } else {
                            var t368 int = index__6 + 1
                            var t369 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t368)
                            var second__10 uint32 = uint32(uint8(t369))
                            var t370 int = index__6 + 2
                            var t371 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t370)
                            var third__11 uint32 = uint32(uint8(t371))
                            var t397 bool = utf8_invalid_continuation(second__10)
                            var jp392 bool
                            if t397 {
                                jp392 = true
                            } else {
                                var inline619 bool = third__11 < 128
                                if inline619 {
                                    jp392 = true
                                } else {
                                    var inline620 bool = third__11 > 191
                                    jp392 = inline620
                                }
                            }
                            var jp386 bool
                            if jp392 {
                                jp386 = true
                            } else {
                                var t395 bool = first__8 == 224
                                if t395 {
                                    var t396 bool = second__10 < 160
                                    jp386 = t396
                                } else {
                                    jp386 = false
                                }
                            }
                            var jp375 bool
                            if jp386 {
                                jp375 = true
                            } else {
                                var t389 bool = first__8 == 237
                                if t389 {
                                    var t390 bool = second__10 >= 160
                                    jp375 = t390
                                } else {
                                    jp375 = false
                                }
                            }
                            if jp375 {
                                var inline622 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline622
                            } else {
                                var t377_rhs uint32 = 15
                                var t377 uint32 = first__8 & t377_rhs
                                var t378_rhs int = 12
                                var t378 uint32 = t377 << t378_rhs
                                var t379_rhs uint32 = 63
                                var t379 uint32 = second__10 & t379_rhs
                                var t380_rhs int = 6
                                var t380 uint32 = t379 << t380_rhs
                                var t381 uint32 = t378 | t380
                                var t382_rhs uint32 = 63
                                var t382 uint32 = third__11 & t382_rhs
                                var t383 uint32 = t381 | t382
                                var inline624 int = 3
                                var inline625 Option__char = __goml_builtin_char_from_uint32(t383)
                                switch inline625.(type) {
                                case Option__char_None:
                                    var inline626 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline626
                                case Option__char_Some:
                                    var inline627 rune = inline625.(Option__char_Some)._0
                                    var inline629 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline627,
                                        _2: inline624,
                                    }
                                    return inline629
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t404 bool = first__8 < 245
                        if t404 {
                            var t445 int = length__7 - index__6
                            var t446 bool = t445 < 4
                            if t446 {
                                var t447 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t447
                            } else {
                                var t406 int = index__6 + 1
                                var t407 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t406)
                                var second__12 uint32 = uint32(uint8(t407))
                                var t408 int = index__6 + 2
                                var t409 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t408)
                                var third__13 uint32 = uint32(uint8(t409))
                                var t410 int = index__6 + 3
                                var t411 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t410)
                                var fourth__14 uint32 = uint32(uint8(t411))
                                var t443 bool = utf8_invalid_continuation(second__12)
                                var jp441 bool
                                if t443 {
                                    jp441 = true
                                } else {
                                    var t444 bool = utf8_invalid_continuation(third__13)
                                    jp441 = t444
                                }
                                var jp435 bool
                                if jp441 {
                                    jp435 = true
                                } else {
                                    var t442 bool = utf8_invalid_continuation(fourth__14)
                                    jp435 = t442
                                }
                                var jp429 bool
                                if jp435 {
                                    jp429 = true
                                } else {
                                    var t438 bool = first__8 == 240
                                    if t438 {
                                        var t439 bool = second__12 < 144
                                        jp429 = t439
                                    } else {
                                        jp429 = false
                                    }
                                }
                                var jp415 bool
                                if jp429 {
                                    jp415 = true
                                } else {
                                    var t432 bool = first__8 == 244
                                    if t432 {
                                        var t433 bool = second__12 > 143
                                        jp415 = t433
                                    } else {
                                        jp415 = false
                                    }
                                }
                                if jp415 {
                                    var t416 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t416
                                } else {
                                    var t417_rhs uint32 = 7
                                    var t417 uint32 = first__8 & t417_rhs
                                    var t418_rhs int = 18
                                    var t418 uint32 = t417 << t418_rhs
                                    var t419_rhs uint32 = 63
                                    var t419 uint32 = second__12 & t419_rhs
                                    var t420_rhs int = 12
                                    var t420 uint32 = t419 << t420_rhs
                                    var t421 uint32 = t418 | t420
                                    var t422_rhs uint32 = 63
                                    var t422 uint32 = third__13 & t422_rhs
                                    var t423_rhs int = 6
                                    var t423 uint32 = t422 << t423_rhs
                                    var t424 uint32 = t421 | t423
                                    var t425_rhs uint32 = 63
                                    var t425 uint32 = fourth__14 & t425_rhs
                                    var t426 uint32 = t424 | t425
                                    var t427 Tuple3_4bool_4char_3int = utf8_valid_decode(t426, 4)
                                    return t427
                                }
                            }
                        } else {
                            var t448 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t448
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t456 string = _goml_runtime_core_bool_to_string(self__64)
    return t456
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t459 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t459
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t462 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t462
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field649 rune
    var inline633 bool = utf8_valid_scalar(value__0)
    if inline633 {
        var inline634 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline635 rune = inline634._1
        commute_field649 = inline635
        var t468 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field649,
            _2: width__1,
        }
        return t468
    } else {
        var inline631 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline631
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t473 bool = value__3 < 128
    if t473 {
        return true
    } else {
        var t474 bool = value__3 > 191
        return t474
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t479 bool
    var inline639 bool = value__30 <= 1114111
    if inline639 {
        var inline640 bool = value__30 >= 55296
        var inline642 bool
        if inline640 {
            var inline644 bool = value__30 <= 57343
            inline642 = inline644
        } else {
            inline642 = false
        }
        var inline643 bool = !inline642
        t479 = inline643
    } else {
        t479 = false
    }
    if t479 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t480 Option__char = Option__char_Some{
            _0: x24,
        }
        return t480
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t485 bool = value__4 <= 1114111
    if t485 {
        var t489 bool = value__4 >= 55296
        var jp487 bool
        if t489 {
            var t490 bool = value__4 <= 57343
            jp487 = t490
        } else {
            jp487 = false
        }
        var t488 bool = !jp487
        return t488
    } else {
        return false
    }
}

func main() {
    main0()
}
