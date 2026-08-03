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
    var inline517 int = vec_len__Vec_5uint8(bytes__0)
    expected_length__2 = inline517
    var mtmp177 Tuple2_4bool_6string = string_from_utf8(bytes__0)
    var x178 bool = mtmp177._0
    var x179 string = mtmp177._1
    var t282 bool
    var inline515 bool = x178 == expected__1
    t282 = inline515
    var inline512 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t282)
    _goml_runtime_core_string_println(inline512)
    if x178 {
        var t284 int
        var inline504 int = _goml_runtime_core_string_len(x179)
        t284 = inline504
        var t285 bool
        var inline502 bool = t284 == expected_length__2
        t285 = inline502
        var inline499 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t285)
        _goml_runtime_core_string_println(inline499)
        return struct{}{}
    } else {
        var t287 bool
        var inline509 string = ""
        var inline510 bool = x179 == inline509
        t287 = inline510
        var inline506 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t287)
        _goml_runtime_core_string_println(inline506)
        return struct{}{}
    }
}

func check_scalar(bytes__5 *_goml_vec_uint8, expected__6 uint32, expected_width__7 int) struct{} {
    var mtmp181 Tuple2_4bool_6string = string_from_utf8(bytes__5)
    var x182 bool = mtmp181._0
    var x183 string = mtmp181._1
    var inline548 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x182)
    _goml_runtime_core_string_println(inline548)
    var commute_field668 Tuple2_4char_3int
    var inline537 int = 0
    var inline538 Tuple3_4bool_4char_3int = string_decode_utf8_at(x183, inline537)
    var inline539 bool = inline538._0
    var inline540 rune = inline538._1
    var inline541 int = inline538._2
    if inline539 {
        var inline545 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline540,
            _1: inline541,
        }
        commute_field668 = inline545
        var x189 rune = commute_field668._0
        var x190 int = commute_field668._1
        var t292 uint32 = uint32(rune(x189))
        var t293 bool
        var inline535 bool = t292 == expected__6
        t293 = inline535
        var inline532 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t293)
        _goml_runtime_core_string_println(inline532)
        var t294 bool
        var inline530 bool = x190 == expected_width__7
        t294 = inline530
        var inline527 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t294)
        _goml_runtime_core_string_println(inline527)
        return struct{}{}
    } else {
        var inline523 bool = false
        var inline524 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline523)
        _goml_runtime_core_string_println(inline524)
        var inline519 bool = false
        var inline520 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline519)
        _goml_runtime_core_string_println(inline520)
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
    var inline601 uint8 = 144
    vec_push__Vec_5uint8(vec_literal__1833, inline601)
    var inline598 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1833, inline598)
    var inline595 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1833, inline595)
    var inline580 bool = false
    var inline581 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1833)
    var inline582 Tuple2_4bool_6string = string_from_utf8(vec_literal__1833)
    var inline583 bool = inline582._0
    var inline584 string = inline582._1
    var inline587 bool = _goml_m_trait__impl_i_Eq_i_bool_i_eq(inline583, inline580)
    println__T_bool(inline587)
    if inline583 {
        var inline589 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline584)
        var inline590 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(inline589, inline581)
        println__T_bool(inline590)
    } else {
        var inline592 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(inline584, "")
        println__T_bool(inline592)
    }
    var vec_literal__1883 *_goml_vec_uint8
    var inline578 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__1883 = inline578
    var inline575 uint8 = 245
    vec_push__Vec_5uint8(vec_literal__1883, inline575)
    var inline572 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1883, inline572)
    var inline569 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1883, inline569)
    var inline566 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1883, inline566)
    var inline551 bool = false
    var inline552 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1883)
    var inline553 Tuple2_4bool_6string = string_from_utf8(vec_literal__1883)
    var inline554 bool = inline553._0
    var inline555 string = inline553._1
    var inline558 bool = _goml_m_trait__impl_i_Eq_i_bool_i_eq(inline554, inline551)
    println__T_bool(inline558)
    if inline554 {
        var inline560 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline555)
        var inline561 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(inline560, inline552)
        println__T_bool(inline561)
        return struct{}{}
    } else {
        var inline563 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(inline555, "")
        println__T_bool(inline563)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__166 *_goml_vec_uint8) int {
    var t300 int = vec_len__Vec_5uint8(self__166)
    return t300
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop305:
    for {
        var t306 int
        var inline604 int = _goml_runtime_core_string_len(x12)
        t306 = inline604
        var t307 bool = index__26 < t306
        if t307 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t309 int = compound_old17 + x16
                index__26 = t309
                continue
            } else {
                var t311 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t311
            }
        } else {
            break Loop_loop305
        }
    }
    var t304 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t304
}

func println__T_bool(value__31 bool) struct{} {
    var t313 string
    var inline606 string = _goml_runtime_core_bool_to_string(value__31)
    t313 = inline606
    _goml_runtime_core_string_println(t313)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_bool_i_eq(self__82 bool, other__83 bool) bool {
    var t317 bool = self__82 == other__83
    return t317
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t320 int = _goml_runtime_core_string_len(self__38)
    return t320
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__88 int, other__89 int) bool {
    var t323 bool = self__88 == other__89
    return t323
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__84 string, other__85 string) bool {
    var t326 bool = self__84 == other__85
    return t326
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t335 bool = self__102 == other__103
    return t335
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t338 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t338
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__155 *_goml_vec_uint8, elem__156 uint8) struct{} {
    vec_push__Vec_5uint8(self__155, elem__156)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t459 bool = index__6 < 0
    var jp457 bool
    if t459 {
        jp457 = true
    } else {
        var t460 bool = index__6 >= length__7
        jp457 = t460
    }
    if jp457 {
        var inline608 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline608
    } else {
        var t344 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t344))
        var t347 bool = first__8 < 128
        if t347 {
            var inline610 int = 1
            var inline611 Option__char = char_from_uint32(first__8)
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
        } else {
            var t351 bool = first__8 < 194
            if t351 {
                var inline617 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline617
            } else {
                var t355 bool = first__8 < 224
                if t355 {
                    var t368 int = length__7 - index__6
                    var t369 bool = t368 < 2
                    if t369 {
                        var inline619 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline619
                    } else {
                        var t357 int = index__6 + 1
                        var t358 uint8
                        var inline633 uint8 = _goml_runtime_core_string_byte_get(value__5, t357)
                        t358 = inline633
                        var second__9 uint32 = uint32(uint8(t358))
                        var t361 bool
                        var inline630 bool = second__9 < 128
                        if inline630 {
                            t361 = true
                        } else {
                            var inline631 bool = second__9 > 191
                            t361 = inline631
                        }
                        if t361 {
                            var inline621 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline621
                        } else {
                            var t363_rhs uint32 = 31
                            var t363 uint32 = first__8 & t363_rhs
                            var t364_rhs int = 6
                            var t364 uint32 = t363 << t364_rhs
                            var t365_rhs uint32 = 63
                            var t365 uint32 = second__9 & t365_rhs
                            var t366 uint32 = t364 | t365
                            var inline623 int = 2
                            var inline624 Option__char = char_from_uint32(t366)
                            switch inline624.(type) {
                            case Option__char_None:
                                var inline625 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline625
                            case Option__char_Some:
                                var inline626 rune = inline624.(Option__char_Some)._0
                                var inline628 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline626,
                                    _2: inline623,
                                }
                                return inline628
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t373 bool = first__8 < 240
                    if t373 {
                        var t406 int = length__7 - index__6
                        var t407 bool = t406 < 3
                        if t407 {
                            var inline635 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline635
                        } else {
                            var t375 int = index__6 + 1
                            var t376 uint8
                            var inline650 uint8 = _goml_runtime_core_string_byte_get(value__5, t375)
                            t376 = inline650
                            var second__10 uint32 = uint32(uint8(t376))
                            var t377 int = index__6 + 2
                            var t378 uint8
                            var inline648 uint8 = _goml_runtime_core_string_byte_get(value__5, t377)
                            t378 = inline648
                            var third__11 uint32 = uint32(uint8(t378))
                            var t404 bool = utf8_invalid_continuation(second__10)
                            var jp399 bool
                            if t404 {
                                jp399 = true
                            } else {
                                var inline637 bool = third__11 < 128
                                if inline637 {
                                    jp399 = true
                                } else {
                                    var inline638 bool = third__11 > 191
                                    jp399 = inline638
                                }
                            }
                            var jp393 bool
                            if jp399 {
                                jp393 = true
                            } else {
                                var t402 bool
                                var inline640 uint32 = 224
                                var inline641 bool = first__8 == inline640
                                t402 = inline641
                                if t402 {
                                    var t403 bool = second__10 < 160
                                    jp393 = t403
                                } else {
                                    jp393 = false
                                }
                            }
                            var jp382 bool
                            if jp393 {
                                jp382 = true
                            } else {
                                var t396 bool
                                var inline643 uint32 = 237
                                var inline644 bool = first__8 == inline643
                                t396 = inline644
                                if t396 {
                                    var t397 bool = second__10 >= 160
                                    jp382 = t397
                                } else {
                                    jp382 = false
                                }
                            }
                            if jp382 {
                                var inline646 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline646
                            } else {
                                var t384_rhs uint32 = 15
                                var t384 uint32 = first__8 & t384_rhs
                                var t385_rhs int = 12
                                var t385 uint32 = t384 << t385_rhs
                                var t386_rhs uint32 = 63
                                var t386 uint32 = second__10 & t386_rhs
                                var t387_rhs int = 6
                                var t387 uint32 = t386 << t387_rhs
                                var t388 uint32 = t385 | t387
                                var t389_rhs uint32 = 63
                                var t389 uint32 = third__11 & t389_rhs
                                var t390 uint32 = t388 | t389
                                var t391 Tuple3_4bool_4char_3int = utf8_valid_decode(t390, 3)
                                return t391
                            }
                        }
                    } else {
                        var t411 bool = first__8 < 245
                        if t411 {
                            var t452 int = length__7 - index__6
                            var t453 bool = t452 < 4
                            if t453 {
                                var t454 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t454
                            } else {
                                var t413 int = index__6 + 1
                                var t414 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t413)
                                var second__12 uint32 = uint32(uint8(t414))
                                var t415 int = index__6 + 2
                                var t416 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t415)
                                var third__13 uint32 = uint32(uint8(t416))
                                var t417 int = index__6 + 3
                                var t418 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t417)
                                var fourth__14 uint32 = uint32(uint8(t418))
                                var t450 bool = utf8_invalid_continuation(second__12)
                                var jp448 bool
                                if t450 {
                                    jp448 = true
                                } else {
                                    var t451 bool = utf8_invalid_continuation(third__13)
                                    jp448 = t451
                                }
                                var jp442 bool
                                if jp448 {
                                    jp442 = true
                                } else {
                                    var t449 bool = utf8_invalid_continuation(fourth__14)
                                    jp442 = t449
                                }
                                var jp436 bool
                                if jp442 {
                                    jp436 = true
                                } else {
                                    var t445 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t445 {
                                        var t446 bool = second__12 < 144
                                        jp436 = t446
                                    } else {
                                        jp436 = false
                                    }
                                }
                                var jp422 bool
                                if jp436 {
                                    jp422 = true
                                } else {
                                    var t439 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t439 {
                                        var t440 bool = second__12 > 143
                                        jp422 = t440
                                    } else {
                                        jp422 = false
                                    }
                                }
                                if jp422 {
                                    var t423 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t423
                                } else {
                                    var t424_rhs uint32 = 7
                                    var t424 uint32 = first__8 & t424_rhs
                                    var t425_rhs int = 18
                                    var t425 uint32 = t424 << t425_rhs
                                    var t426_rhs uint32 = 63
                                    var t426 uint32 = second__12 & t426_rhs
                                    var t427_rhs int = 12
                                    var t427 uint32 = t426 << t427_rhs
                                    var t428 uint32 = t425 | t427
                                    var t429_rhs uint32 = 63
                                    var t429 uint32 = third__13 & t429_rhs
                                    var t430_rhs int = 6
                                    var t430 uint32 = t429 << t430_rhs
                                    var t431 uint32 = t428 | t430
                                    var t432_rhs uint32 = 63
                                    var t432 uint32 = fourth__14 & t432_rhs
                                    var t433 uint32 = t431 | t432
                                    var t434 Tuple3_4bool_4char_3int = utf8_valid_decode(t433, 4)
                                    return t434
                                }
                            }
                        } else {
                            var t455 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t455
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t463 string = _goml_runtime_core_bool_to_string(self__66)
    return t463
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t466 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t466
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t469 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t469
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field671 rune
    var inline654 bool = utf8_valid_scalar(value__0)
    if inline654 {
        var inline655 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline657 rune = inline655._1
        commute_field671 = inline657
        var t475 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field671,
            _2: width__1,
        }
        return t475
    } else {
        var inline652 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline652
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t480 bool = value__3 < 128
    if t480 {
        return true
    } else {
        var t481 bool = value__3 > 191
        return t481
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t486 bool
    var inline661 bool = value__32 <= 1114111
    if inline661 {
        var inline662 bool = value__32 >= 55296
        var inline664 bool
        if inline662 {
            var inline666 bool = value__32 <= 57343
            inline664 = inline666
        } else {
            inline664 = false
        }
        var inline665 bool = !inline664
        t486 = inline665
    } else {
        t486 = false
    }
    if t486 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t487 Option__char = Option__char_Some{
            _0: x24,
        }
        return t487
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t492 bool = value__4 <= 1114111
    if t492 {
        var t496 bool = value__4 >= 55296
        var jp494 bool
        if t496 {
            var t497 bool = value__4 <= 57343
            jp494 = t497
        } else {
            jp494 = false
        }
        var t495 bool = !jp494
        return t495
    } else {
        return false
    }
}

func main() {
    main0()
}
