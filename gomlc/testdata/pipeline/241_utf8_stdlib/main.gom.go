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
    var inline493 int = vec_len__Vec_5uint8(bytes__0)
    expected_length__2 = inline493
    var mtmp172 Tuple2_4bool_6string = string_from_utf8(bytes__0)
    var x173 bool = mtmp172._0
    var x174 string = mtmp172._1
    var t277 bool = x173 == expected__1
    var inline490 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t277)
    _goml_runtime_core_string_println(inline490)
    if x173 {
        var t279 int
        var inline485 int = _goml_runtime_core_string_len(x174)
        t279 = inline485
        var t280 bool = t279 == expected_length__2
        var inline482 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t280)
        _goml_runtime_core_string_println(inline482)
        return struct{}{}
    } else {
        var t282 bool = x174 == ""
        var inline487 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t282)
        _goml_runtime_core_string_println(inline487)
        return struct{}{}
    }
}

func check_scalar(bytes__5 *_goml_vec_uint8, expected__6 uint32, expected_width__7 int) struct{} {
    var mtmp176 Tuple2_4bool_6string = string_from_utf8(bytes__5)
    var x177 bool = mtmp176._0
    var x178 string = mtmp176._1
    var inline520 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x177)
    _goml_runtime_core_string_println(inline520)
    var commute_field637 Tuple2_4char_3int
    var inline509 int = 0
    var inline510 Tuple3_4bool_4char_3int = string_decode_utf8_at(x178, inline509)
    var inline511 bool = inline510._0
    var inline512 rune = inline510._1
    var inline513 int = inline510._2
    if inline511 {
        var inline517 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline512,
            _1: inline513,
        }
        commute_field637 = inline517
        var x184 rune = commute_field637._0
        var x185 int = commute_field637._1
        var t287 uint32 = uint32(rune(x184))
        var t288 bool = t287 == expected__6
        var inline506 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t288)
        _goml_runtime_core_string_println(inline506)
        var t289 bool = x185 == expected_width__7
        var inline503 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t289)
        _goml_runtime_core_string_println(inline503)
        return struct{}{}
    } else {
        var inline499 bool = false
        var inline500 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline499)
        _goml_runtime_core_string_println(inline500)
        var inline495 bool = false
        var inline496 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline495)
        _goml_runtime_core_string_println(inline496)
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
    var inline573 uint8 = 144
    vec_push__Vec_5uint8(vec_literal__1835, inline573)
    var inline570 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1835, inline570)
    var inline567 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1835, inline567)
    var inline552 bool = false
    var inline553 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1835)
    var inline554 Tuple2_4bool_6string = string_from_utf8(vec_literal__1835)
    var inline555 bool = inline554._0
    var inline556 string = inline554._1
    var inline559 bool = inline555 == inline552
    println__T_bool(inline559)
    if inline555 {
        var inline561 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline556)
        var inline562 bool = inline561 == inline553
        println__T_bool(inline562)
    } else {
        var inline564 bool = inline556 == ""
        println__T_bool(inline564)
    }
    var vec_literal__1885 *_goml_vec_uint8
    var inline550 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__1885 = inline550
    var inline547 uint8 = 245
    vec_push__Vec_5uint8(vec_literal__1885, inline547)
    var inline544 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline544)
    var inline541 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline541)
    var inline538 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1885, inline538)
    var inline523 bool = false
    var inline524 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1885)
    var inline525 Tuple2_4bool_6string = string_from_utf8(vec_literal__1885)
    var inline526 bool = inline525._0
    var inline527 string = inline525._1
    var inline530 bool = inline526 == inline523
    println__T_bool(inline530)
    if inline526 {
        var inline532 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline527)
        var inline533 bool = inline532 == inline524
        println__T_bool(inline533)
        return struct{}{}
    } else {
        var inline535 bool = inline527 == ""
        println__T_bool(inline535)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__187 *_goml_vec_uint8) int {
    var t295 int = vec_len__Vec_5uint8(self__187)
    return t295
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop300:
    for {
        var t301 int
        var inline576 int = _goml_runtime_core_string_len(x12)
        t301 = inline576
        var t302 bool = index__26 < t301
        if t302 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t304 int = compound_old17 + x16
                index__26 = t304
                continue
            } else {
                var t306 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t306
            }
        } else {
            break Loop_loop300
        }
    }
    var t299 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t299
}

func println__T_bool(value__31 bool) struct{} {
    var t308 string
    var inline578 string = _goml_runtime_core_bool_to_string(value__31)
    t308 = inline578
    _goml_runtime_core_string_println(t308)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t312 int = _goml_runtime_core_string_len(self__38)
    return t312
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t321 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t321
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__176 *_goml_vec_uint8, elem__177 uint8) struct{} {
    vec_push__Vec_5uint8(self__176, elem__177)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t442 bool = index__6 < 0
    var jp440 bool
    if t442 {
        jp440 = true
    } else {
        var t443 bool = index__6 >= length__7
        jp440 = t443
    }
    if jp440 {
        var inline580 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline580
    } else {
        var t327 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t327))
        var t330 bool = first__8 < 128
        if t330 {
            var inline582 int = 1
            var inline583 Option__char = char_from_uint32(first__8)
            switch inline583.(type) {
            case Option__char_None:
                var inline584 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline584
            case Option__char_Some:
                var inline585 rune = inline583.(Option__char_Some)._0
                var inline587 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline585,
                    _2: inline582,
                }
                return inline587
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t334 bool = first__8 < 194
            if t334 {
                var inline589 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline589
            } else {
                var t338 bool = first__8 < 224
                if t338 {
                    var t351 int = length__7 - index__6
                    var t352 bool = t351 < 2
                    if t352 {
                        var inline591 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline591
                    } else {
                        var t340 int = index__6 + 1
                        var t341 uint8
                        var inline605 uint8 = _goml_runtime_core_string_byte_get(value__5, t340)
                        t341 = inline605
                        var second__9 uint32 = uint32(uint8(t341))
                        var t344 bool
                        var inline602 bool = second__9 < 128
                        if inline602 {
                            t344 = true
                        } else {
                            var inline603 bool = second__9 > 191
                            t344 = inline603
                        }
                        if t344 {
                            var inline593 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline593
                        } else {
                            var t346_rhs uint32 = 31
                            var t346 uint32 = first__8 & t346_rhs
                            var t347_rhs int = 6
                            var t347 uint32 = t346 << t347_rhs
                            var t348_rhs uint32 = 63
                            var t348 uint32 = second__9 & t348_rhs
                            var t349 uint32 = t347 | t348
                            var inline595 int = 2
                            var inline596 Option__char = char_from_uint32(t349)
                            switch inline596.(type) {
                            case Option__char_None:
                                var inline597 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline597
                            case Option__char_Some:
                                var inline598 rune = inline596.(Option__char_Some)._0
                                var inline600 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline598,
                                    _2: inline595,
                                }
                                return inline600
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t356 bool = first__8 < 240
                    if t356 {
                        var t389 int = length__7 - index__6
                        var t390 bool = t389 < 3
                        if t390 {
                            var inline607 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline607
                        } else {
                            var t358 int = index__6 + 1
                            var t359 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t358)
                            var second__10 uint32 = uint32(uint8(t359))
                            var t360 int = index__6 + 2
                            var t361 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t360)
                            var third__11 uint32 = uint32(uint8(t361))
                            var t387 bool = utf8_invalid_continuation(second__10)
                            var jp382 bool
                            if t387 {
                                jp382 = true
                            } else {
                                var inline609 bool = third__11 < 128
                                if inline609 {
                                    jp382 = true
                                } else {
                                    var inline610 bool = third__11 > 191
                                    jp382 = inline610
                                }
                            }
                            var jp376 bool
                            if jp382 {
                                jp376 = true
                            } else {
                                var t385 bool = first__8 == 224
                                if t385 {
                                    var t386 bool = second__10 < 160
                                    jp376 = t386
                                } else {
                                    jp376 = false
                                }
                            }
                            var jp365 bool
                            if jp376 {
                                jp365 = true
                            } else {
                                var t379 bool = first__8 == 237
                                if t379 {
                                    var t380 bool = second__10 >= 160
                                    jp365 = t380
                                } else {
                                    jp365 = false
                                }
                            }
                            if jp365 {
                                var inline612 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline612
                            } else {
                                var t367_rhs uint32 = 15
                                var t367 uint32 = first__8 & t367_rhs
                                var t368_rhs int = 12
                                var t368 uint32 = t367 << t368_rhs
                                var t369_rhs uint32 = 63
                                var t369 uint32 = second__10 & t369_rhs
                                var t370_rhs int = 6
                                var t370 uint32 = t369 << t370_rhs
                                var t371 uint32 = t368 | t370
                                var t372_rhs uint32 = 63
                                var t372 uint32 = third__11 & t372_rhs
                                var t373 uint32 = t371 | t372
                                var inline614 int = 3
                                var inline615 Option__char = char_from_uint32(t373)
                                switch inline615.(type) {
                                case Option__char_None:
                                    var inline616 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline616
                                case Option__char_Some:
                                    var inline617 rune = inline615.(Option__char_Some)._0
                                    var inline619 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline617,
                                        _2: inline614,
                                    }
                                    return inline619
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t394 bool = first__8 < 245
                        if t394 {
                            var t435 int = length__7 - index__6
                            var t436 bool = t435 < 4
                            if t436 {
                                var t437 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t437
                            } else {
                                var t396 int = index__6 + 1
                                var t397 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t396)
                                var second__12 uint32 = uint32(uint8(t397))
                                var t398 int = index__6 + 2
                                var t399 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t398)
                                var third__13 uint32 = uint32(uint8(t399))
                                var t400 int = index__6 + 3
                                var t401 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t400)
                                var fourth__14 uint32 = uint32(uint8(t401))
                                var t433 bool = utf8_invalid_continuation(second__12)
                                var jp431 bool
                                if t433 {
                                    jp431 = true
                                } else {
                                    var t434 bool = utf8_invalid_continuation(third__13)
                                    jp431 = t434
                                }
                                var jp425 bool
                                if jp431 {
                                    jp425 = true
                                } else {
                                    var t432 bool = utf8_invalid_continuation(fourth__14)
                                    jp425 = t432
                                }
                                var jp419 bool
                                if jp425 {
                                    jp419 = true
                                } else {
                                    var t428 bool = first__8 == 240
                                    if t428 {
                                        var t429 bool = second__12 < 144
                                        jp419 = t429
                                    } else {
                                        jp419 = false
                                    }
                                }
                                var jp405 bool
                                if jp419 {
                                    jp405 = true
                                } else {
                                    var t422 bool = first__8 == 244
                                    if t422 {
                                        var t423 bool = second__12 > 143
                                        jp405 = t423
                                    } else {
                                        jp405 = false
                                    }
                                }
                                if jp405 {
                                    var t406 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t406
                                } else {
                                    var t407_rhs uint32 = 7
                                    var t407 uint32 = first__8 & t407_rhs
                                    var t408_rhs int = 18
                                    var t408 uint32 = t407 << t408_rhs
                                    var t409_rhs uint32 = 63
                                    var t409 uint32 = second__12 & t409_rhs
                                    var t410_rhs int = 12
                                    var t410 uint32 = t409 << t410_rhs
                                    var t411 uint32 = t408 | t410
                                    var t412_rhs uint32 = 63
                                    var t412 uint32 = third__13 & t412_rhs
                                    var t413_rhs int = 6
                                    var t413 uint32 = t412 << t413_rhs
                                    var t414 uint32 = t411 | t413
                                    var t415_rhs uint32 = 63
                                    var t415 uint32 = fourth__14 & t415_rhs
                                    var t416 uint32 = t414 | t415
                                    var t417 Tuple3_4bool_4char_3int = utf8_valid_decode(t416, 4)
                                    return t417
                                }
                            }
                        } else {
                            var t438 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t438
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t446 string = _goml_runtime_core_bool_to_string(self__66)
    return t446
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t449 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t449
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t452 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t452
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field640 rune
    var inline623 bool = utf8_valid_scalar(value__0)
    if inline623 {
        var inline624 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline626 rune = inline624._1
        commute_field640 = inline626
        var t458 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field640,
            _2: width__1,
        }
        return t458
    } else {
        var inline621 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline621
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t463 bool = value__3 < 128
    if t463 {
        return true
    } else {
        var t464 bool = value__3 > 191
        return t464
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t469 bool
    var inline630 bool = value__32 <= 1114111
    if inline630 {
        var inline631 bool = value__32 >= 55296
        var inline633 bool
        if inline631 {
            var inline635 bool = value__32 <= 57343
            inline633 = inline635
        } else {
            inline633 = false
        }
        var inline634 bool = !inline633
        t469 = inline634
    } else {
        t469 = false
    }
    if t469 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t470 Option__char = Option__char_Some{
            _0: x24,
        }
        return t470
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t475 bool = value__4 <= 1114111
    if t475 {
        var t479 bool = value__4 >= 55296
        var jp477 bool
        if t479 {
            var t480 bool = value__4 <= 57343
            jp477 = t480
        } else {
            jp477 = false
        }
        var t478 bool = !jp477
        return t478
    } else {
        return false
    }
}

func main() {
    main0()
}
