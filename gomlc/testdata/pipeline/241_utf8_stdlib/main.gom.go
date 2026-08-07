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
    var inline512 int = vec_len__Vec_5uint8(bytes__0)
    expected_length__2 = inline512
    var mtmp172 Tuple2_4bool_6string = string_from_utf8(bytes__0)
    var x173 bool = mtmp172._0
    var x174 string = mtmp172._1
    var t277 bool
    var inline510 bool = x173 == expected__1
    t277 = inline510
    var inline507 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t277)
    _goml_runtime_core_string_println(inline507)
    if x173 {
        var t279 int
        var inline499 int = _goml_runtime_core_string_len(x174)
        t279 = inline499
        var t280 bool
        var inline497 bool = t279 == expected_length__2
        t280 = inline497
        var inline494 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t280)
        _goml_runtime_core_string_println(inline494)
        return struct{}{}
    } else {
        var t282 bool
        var inline504 string = ""
        var inline505 bool = x174 == inline504
        t282 = inline505
        var inline501 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t282)
        _goml_runtime_core_string_println(inline501)
        return struct{}{}
    }
}

func check_scalar(bytes__5 *_goml_vec_uint8, expected__6 uint32, expected_width__7 int) struct{} {
    var mtmp176 Tuple2_4bool_6string = string_from_utf8(bytes__5)
    var x177 bool = mtmp176._0
    var x178 string = mtmp176._1
    var inline543 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x177)
    _goml_runtime_core_string_println(inline543)
    var commute_field663 Tuple2_4char_3int
    var inline532 int = 0
    var inline533 Tuple3_4bool_4char_3int = string_decode_utf8_at(x178, inline532)
    var inline534 bool = inline533._0
    var inline535 rune = inline533._1
    var inline536 int = inline533._2
    if inline534 {
        var inline540 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline535,
            _1: inline536,
        }
        commute_field663 = inline540
        var x184 rune = commute_field663._0
        var x185 int = commute_field663._1
        var t287 uint32 = uint32(rune(x184))
        var t288 bool
        var inline530 bool = t287 == expected__6
        t288 = inline530
        var inline527 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t288)
        _goml_runtime_core_string_println(inline527)
        var t289 bool
        var inline525 bool = x185 == expected_width__7
        t289 = inline525
        var inline522 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t289)
        _goml_runtime_core_string_println(inline522)
        return struct{}{}
    } else {
        var inline518 bool = false
        var inline519 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline518)
        _goml_runtime_core_string_println(inline519)
        var inline514 bool = false
        var inline515 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline514)
        _goml_runtime_core_string_println(inline515)
        return struct{}{}
    }
}

func main0() struct{} {
    var vec_literal__793 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    check_utf8(vec_literal__793, true)
    var vec_literal__824 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__824, 0)
    check_utf8(vec_literal__824, true)
    var vec_literal__856 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__856, 127)
    check_utf8(vec_literal__856, true)
    var vec_literal__892 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__892, 194)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__892, 128)
    check_scalar(vec_literal__892, 128, 2)
    var vec_literal__935 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__935, 223)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__935, 191)
    check_scalar(vec_literal__935, 2047, 2)
    var vec_literal__979 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__979, 224)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__979, 160)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__979, 128)
    check_scalar(vec_literal__979, 2048, 3)
    var vec_literal__1028 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1028, 237)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1028, 159)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1028, 191)
    check_scalar(vec_literal__1028, 55295, 3)
    var vec_literal__1078 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1078, 238)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1078, 128)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1078, 128)
    check_scalar(vec_literal__1078, 57344, 3)
    var vec_literal__1128 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1128, 239)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1128, 191)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1128, 189)
    check_scalar(vec_literal__1128, 65533, 3)
    var vec_literal__1178 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1178, 239)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1178, 191)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1178, 191)
    check_scalar(vec_literal__1178, 65535, 3)
    var vec_literal__1228 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1228, 240)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1228, 144)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1228, 128)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1228, 128)
    check_scalar(vec_literal__1228, 65536, 4)
    var vec_literal__1283 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1283, 244)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1283, 143)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1283, 191)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1283, 191)
    check_scalar(vec_literal__1283, 1114111, 4)
    var vec_literal__1338 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1338, 128)
    check_utf8(vec_literal__1338, false)
    var vec_literal__1373 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1373, 191)
    check_utf8(vec_literal__1373, false)
    var vec_literal__1408 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1408, 192)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1408, 128)
    check_utf8(vec_literal__1408, false)
    var vec_literal__1448 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1448, 193)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1448, 191)
    check_utf8(vec_literal__1448, false)
    var vec_literal__1488 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1488, 194)
    check_utf8(vec_literal__1488, false)
    var vec_literal__1523 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1523, 194)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1523, 127)
    check_utf8(vec_literal__1523, false)
    var vec_literal__1563 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1563, 224)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1563, 159)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1563, 191)
    check_utf8(vec_literal__1563, false)
    var vec_literal__1608 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1608, 225)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1608, 128)
    check_utf8(vec_literal__1608, false)
    var vec_literal__1648 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1648, 225)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1648, 128)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1648, 127)
    check_utf8(vec_literal__1648, false)
    var vec_literal__1693 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1693, 237)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1693, 160)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1693, 128)
    check_utf8(vec_literal__1693, false)
    var vec_literal__1738 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1738, 240)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1738, 143)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1738, 191)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1738, 191)
    check_utf8(vec_literal__1738, false)
    var vec_literal__1788 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1788, 240)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1788, 144)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1788, 128)
    check_utf8(vec_literal__1788, false)
    var vec_literal__1833 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(vec_literal__1833, 244)
    var inline596 uint8 = 144
    vec_push__Vec_5uint8(vec_literal__1833, inline596)
    var inline593 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1833, inline593)
    var inline590 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1833, inline590)
    var inline575 bool = false
    var inline576 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1833)
    var inline577 Tuple2_4bool_6string = string_from_utf8(vec_literal__1833)
    var inline578 bool = inline577._0
    var inline579 string = inline577._1
    var inline582 bool = _goml_m_trait__impl_i_PartialEq_i_bool_i_eq(inline578, inline575)
    println__T_bool(inline582)
    if inline578 {
        var inline584 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline579)
        var inline585 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline584, inline576)
        println__T_bool(inline585)
    } else {
        var inline587 bool = _goml_m_trait__impl_i_PartialEq_i_string_i_eq(inline579, "")
        println__T_bool(inline587)
    }
    var vec_literal__1883 *_goml_vec_uint8
    var inline573 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__1883 = inline573
    var inline570 uint8 = 245
    vec_push__Vec_5uint8(vec_literal__1883, inline570)
    var inline567 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1883, inline567)
    var inline564 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1883, inline564)
    var inline561 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1883, inline561)
    var inline546 bool = false
    var inline547 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1883)
    var inline548 Tuple2_4bool_6string = string_from_utf8(vec_literal__1883)
    var inline549 bool = inline548._0
    var inline550 string = inline548._1
    var inline553 bool = _goml_m_trait__impl_i_PartialEq_i_bool_i_eq(inline549, inline546)
    println__T_bool(inline553)
    if inline549 {
        var inline555 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline550)
        var inline556 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline555, inline547)
        println__T_bool(inline556)
        return struct{}{}
    } else {
        var inline558 bool = _goml_m_trait__impl_i_PartialEq_i_string_i_eq(inline550, "")
        println__T_bool(inline558)
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
        var inline599 int = _goml_runtime_core_string_len(x12)
        t301 = inline599
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
    var inline601 string = _goml_runtime_core_bool_to_string(value__31)
    t308 = inline601
    _goml_runtime_core_string_println(t308)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_bool_i_eq(self__97 bool, other__98 bool) bool {
    var t312 bool = self__97 == other__98
    return t312
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t315 int = _goml_runtime_core_string_len(self__38)
    return t315
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__103 int, other__104 int) bool {
    var t318 bool = self__103 == other__104
    return t318
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__99 string, other__100 string) bool {
    var t321 bool = self__99 == other__100
    return t321
}

func _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t330 bool = self__117 == other__118
    return t330
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t333 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t333
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__176 *_goml_vec_uint8, elem__177 uint8) struct{} {
    vec_push__Vec_5uint8(self__176, elem__177)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t454 bool = index__6 < 0
    var jp452 bool
    if t454 {
        jp452 = true
    } else {
        var t455 bool = index__6 >= length__7
        jp452 = t455
    }
    if jp452 {
        var inline603 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline603
    } else {
        var t339 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t339))
        var t342 bool = first__8 < 128
        if t342 {
            var inline605 int = 1
            var inline606 Option__char = char_from_uint32(first__8)
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
        } else {
            var t346 bool = first__8 < 194
            if t346 {
                var inline612 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline612
            } else {
                var t350 bool = first__8 < 224
                if t350 {
                    var t363 int = length__7 - index__6
                    var t364 bool = t363 < 2
                    if t364 {
                        var inline614 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline614
                    } else {
                        var t352 int = index__6 + 1
                        var t353 uint8
                        var inline628 uint8 = _goml_runtime_core_string_byte_get(value__5, t352)
                        t353 = inline628
                        var second__9 uint32 = uint32(uint8(t353))
                        var t356 bool
                        var inline625 bool = second__9 < 128
                        if inline625 {
                            t356 = true
                        } else {
                            var inline626 bool = second__9 > 191
                            t356 = inline626
                        }
                        if t356 {
                            var inline616 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline616
                        } else {
                            var t358_rhs uint32 = 31
                            var t358 uint32 = first__8 & t358_rhs
                            var t359_rhs int = 6
                            var t359 uint32 = t358 << t359_rhs
                            var t360_rhs uint32 = 63
                            var t360 uint32 = second__9 & t360_rhs
                            var t361 uint32 = t359 | t360
                            var inline618 int = 2
                            var inline619 Option__char = char_from_uint32(t361)
                            switch inline619.(type) {
                            case Option__char_None:
                                var inline620 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline620
                            case Option__char_Some:
                                var inline621 rune = inline619.(Option__char_Some)._0
                                var inline623 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline621,
                                    _2: inline618,
                                }
                                return inline623
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t368 bool = first__8 < 240
                    if t368 {
                        var t401 int = length__7 - index__6
                        var t402 bool = t401 < 3
                        if t402 {
                            var inline630 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline630
                        } else {
                            var t370 int = index__6 + 1
                            var t371 uint8
                            var inline645 uint8 = _goml_runtime_core_string_byte_get(value__5, t370)
                            t371 = inline645
                            var second__10 uint32 = uint32(uint8(t371))
                            var t372 int = index__6 + 2
                            var t373 uint8
                            var inline643 uint8 = _goml_runtime_core_string_byte_get(value__5, t372)
                            t373 = inline643
                            var third__11 uint32 = uint32(uint8(t373))
                            var t399 bool = utf8_invalid_continuation(second__10)
                            var jp394 bool
                            if t399 {
                                jp394 = true
                            } else {
                                var inline632 bool = third__11 < 128
                                if inline632 {
                                    jp394 = true
                                } else {
                                    var inline633 bool = third__11 > 191
                                    jp394 = inline633
                                }
                            }
                            var jp388 bool
                            if jp394 {
                                jp388 = true
                            } else {
                                var t397 bool
                                var inline635 uint32 = 224
                                var inline636 bool = first__8 == inline635
                                t397 = inline636
                                if t397 {
                                    var t398 bool = second__10 < 160
                                    jp388 = t398
                                } else {
                                    jp388 = false
                                }
                            }
                            var jp377 bool
                            if jp388 {
                                jp377 = true
                            } else {
                                var t391 bool
                                var inline638 uint32 = 237
                                var inline639 bool = first__8 == inline638
                                t391 = inline639
                                if t391 {
                                    var t392 bool = second__10 >= 160
                                    jp377 = t392
                                } else {
                                    jp377 = false
                                }
                            }
                            if jp377 {
                                var inline641 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline641
                            } else {
                                var t379_rhs uint32 = 15
                                var t379 uint32 = first__8 & t379_rhs
                                var t380_rhs int = 12
                                var t380 uint32 = t379 << t380_rhs
                                var t381_rhs uint32 = 63
                                var t381 uint32 = second__10 & t381_rhs
                                var t382_rhs int = 6
                                var t382 uint32 = t381 << t382_rhs
                                var t383 uint32 = t380 | t382
                                var t384_rhs uint32 = 63
                                var t384 uint32 = third__11 & t384_rhs
                                var t385 uint32 = t383 | t384
                                var t386 Tuple3_4bool_4char_3int = utf8_valid_decode(t385, 3)
                                return t386
                            }
                        }
                    } else {
                        var t406 bool = first__8 < 245
                        if t406 {
                            var t447 int = length__7 - index__6
                            var t448 bool = t447 < 4
                            if t448 {
                                var t449 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t449
                            } else {
                                var t408 int = index__6 + 1
                                var t409 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t408)
                                var second__12 uint32 = uint32(uint8(t409))
                                var t410 int = index__6 + 2
                                var t411 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t410)
                                var third__13 uint32 = uint32(uint8(t411))
                                var t412 int = index__6 + 3
                                var t413 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t412)
                                var fourth__14 uint32 = uint32(uint8(t413))
                                var t445 bool = utf8_invalid_continuation(second__12)
                                var jp443 bool
                                if t445 {
                                    jp443 = true
                                } else {
                                    var t446 bool = utf8_invalid_continuation(third__13)
                                    jp443 = t446
                                }
                                var jp437 bool
                                if jp443 {
                                    jp437 = true
                                } else {
                                    var t444 bool = utf8_invalid_continuation(fourth__14)
                                    jp437 = t444
                                }
                                var jp431 bool
                                if jp437 {
                                    jp431 = true
                                } else {
                                    var t440 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 240)
                                    if t440 {
                                        var t441 bool = second__12 < 144
                                        jp431 = t441
                                    } else {
                                        jp431 = false
                                    }
                                }
                                var jp417 bool
                                if jp431 {
                                    jp417 = true
                                } else {
                                    var t434 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 244)
                                    if t434 {
                                        var t435 bool = second__12 > 143
                                        jp417 = t435
                                    } else {
                                        jp417 = false
                                    }
                                }
                                if jp417 {
                                    var t418 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t418
                                } else {
                                    var t419_rhs uint32 = 7
                                    var t419 uint32 = first__8 & t419_rhs
                                    var t420_rhs int = 18
                                    var t420 uint32 = t419 << t420_rhs
                                    var t421_rhs uint32 = 63
                                    var t421 uint32 = second__12 & t421_rhs
                                    var t422_rhs int = 12
                                    var t422 uint32 = t421 << t422_rhs
                                    var t423 uint32 = t420 | t422
                                    var t424_rhs uint32 = 63
                                    var t424 uint32 = third__13 & t424_rhs
                                    var t425_rhs int = 6
                                    var t425 uint32 = t424 << t425_rhs
                                    var t426 uint32 = t423 | t425
                                    var t427_rhs uint32 = 63
                                    var t427 uint32 = fourth__14 & t427_rhs
                                    var t428 uint32 = t426 | t427
                                    var t429 Tuple3_4bool_4char_3int = utf8_valid_decode(t428, 4)
                                    return t429
                                }
                            }
                        } else {
                            var t450 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t450
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t458 string = _goml_runtime_core_bool_to_string(self__66)
    return t458
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t461 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t461
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t464 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t464
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field666 rune
    var inline649 bool = utf8_valid_scalar(value__0)
    if inline649 {
        var inline650 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline652 rune = inline650._1
        commute_field666 = inline652
        var t470 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field666,
            _2: width__1,
        }
        return t470
    } else {
        var inline647 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline647
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t475 bool = value__3 < 128
    if t475 {
        return true
    } else {
        var t476 bool = value__3 > 191
        return t476
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t481 bool
    var inline656 bool = value__32 <= 1114111
    if inline656 {
        var inline657 bool = value__32 >= 55296
        var inline659 bool
        if inline657 {
            var inline661 bool = value__32 <= 57343
            inline659 = inline661
        } else {
            inline659 = false
        }
        var inline660 bool = !inline659
        t481 = inline660
    } else {
        t481 = false
    }
    if t481 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t482 Option__char = Option__char_Some{
            _0: x24,
        }
        return t482
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t487 bool = value__4 <= 1114111
    if t487 {
        var t491 bool = value__4 >= 55296
        var jp489 bool
        if t491 {
            var t492 bool = value__4 <= 57343
            jp489 = t492
        } else {
            jp489 = false
        }
        var t490 bool = !jp489
        return t490
    } else {
        return false
    }
}

func main() {
    main0()
}
