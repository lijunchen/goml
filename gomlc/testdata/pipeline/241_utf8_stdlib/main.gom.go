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
    var inline476 int = vec_len__Vec_5uint8(bytes__0)
    expected_length__2 = inline476
    var mtmp136 Tuple2_4bool_6string = string_from_utf8(bytes__0)
    var x137 bool = mtmp136._0
    var x138 string = mtmp136._1
    var t241 bool
    var inline474 bool = x137 == expected__1
    t241 = inline474
    var inline471 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t241)
    _goml_runtime_core_string_println(inline471)
    if x137 {
        var t243 int
        var inline463 int = _goml_runtime_core_string_len(x138)
        t243 = inline463
        var t244 bool
        var inline461 bool = t243 == expected_length__2
        t244 = inline461
        var inline458 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t244)
        _goml_runtime_core_string_println(inline458)
        return struct{}{}
    } else {
        var t246 bool
        var inline468 string = ""
        var inline469 bool = x138 == inline468
        t246 = inline469
        var inline465 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t246)
        _goml_runtime_core_string_println(inline465)
        return struct{}{}
    }
}

func check_scalar(bytes__5 *_goml_vec_uint8, expected__6 uint32, expected_width__7 int) struct{} {
    var mtmp140 Tuple2_4bool_6string = string_from_utf8(bytes__5)
    var x141 bool = mtmp140._0
    var x142 string = mtmp140._1
    var inline507 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x141)
    _goml_runtime_core_string_println(inline507)
    var commute_field627 Tuple2_4char_3int
    var inline496 int = 0
    var inline497 Tuple3_4bool_4char_3int = string_decode_utf8_at(x142, inline496)
    var inline498 bool = inline497._0
    var inline499 rune = inline497._1
    var inline500 int = inline497._2
    if inline498 {
        var inline504 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline499,
            _1: inline500,
        }
        commute_field627 = inline504
        var x148 rune = commute_field627._0
        var x149 int = commute_field627._1
        var t251 uint32 = uint32(rune(x148))
        var t252 bool
        var inline494 bool = t251 == expected__6
        t252 = inline494
        var inline491 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t252)
        _goml_runtime_core_string_println(inline491)
        var t253 bool
        var inline489 bool = x149 == expected_width__7
        t253 = inline489
        var inline486 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t253)
        _goml_runtime_core_string_println(inline486)
        return struct{}{}
    } else {
        var inline482 bool = false
        var inline483 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline482)
        _goml_runtime_core_string_println(inline483)
        var inline478 bool = false
        var inline479 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline478)
        _goml_runtime_core_string_println(inline479)
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
    var inline560 uint8 = 144
    vec_push__Vec_5uint8(vec_literal__1833, inline560)
    var inline557 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1833, inline557)
    var inline554 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1833, inline554)
    var inline539 bool = false
    var inline540 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1833)
    var inline541 Tuple2_4bool_6string = string_from_utf8(vec_literal__1833)
    var inline542 bool = inline541._0
    var inline543 string = inline541._1
    var inline546 bool = _goml_m_trait__impl_i_Eq_i_bool_i_eq(inline542, inline539)
    println__T_bool(inline546)
    if inline542 {
        var inline548 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline543)
        var inline549 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(inline548, inline540)
        println__T_bool(inline549)
    } else {
        var inline551 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(inline543, "")
        println__T_bool(inline551)
    }
    var vec_literal__1883 *_goml_vec_uint8
    var inline537 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__1883 = inline537
    var inline534 uint8 = 245
    vec_push__Vec_5uint8(vec_literal__1883, inline534)
    var inline531 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1883, inline531)
    var inline528 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1883, inline528)
    var inline525 uint8 = 128
    vec_push__Vec_5uint8(vec_literal__1883, inline525)
    var inline510 bool = false
    var inline511 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(vec_literal__1883)
    var inline512 Tuple2_4bool_6string = string_from_utf8(vec_literal__1883)
    var inline513 bool = inline512._0
    var inline514 string = inline512._1
    var inline517 bool = _goml_m_trait__impl_i_Eq_i_bool_i_eq(inline513, inline510)
    println__T_bool(inline517)
    if inline513 {
        var inline519 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline514)
        var inline520 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(inline519, inline511)
        println__T_bool(inline520)
        return struct{}{}
    } else {
        var inline522 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(inline514, "")
        println__T_bool(inline522)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__145 *_goml_vec_uint8) int {
    var t259 int = vec_len__Vec_5uint8(self__145)
    return t259
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop264:
    for {
        var t265 int
        var inline563 int = _goml_runtime_core_string_len(x12)
        t265 = inline563
        var t266 bool = index__26 < t265
        if t266 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t268 int = compound_old17 + x16
                index__26 = t268
                continue
            } else {
                var t270 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t270
            }
        } else {
            break Loop_loop264
        }
    }
    var t263 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t263
}

func println__T_bool(value__31 bool) struct{} {
    var t272 string
    var inline565 string = _goml_runtime_core_bool_to_string(value__31)
    t272 = inline565
    _goml_runtime_core_string_println(t272)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_bool_i_eq(self__82 bool, other__83 bool) bool {
    var t276 bool = self__82 == other__83
    return t276
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t279 int = _goml_runtime_core_string_len(self__38)
    return t279
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__88 int, other__89 int) bool {
    var t282 bool = self__88 == other__89
    return t282
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__84 string, other__85 string) bool {
    var t285 bool = self__84 == other__85
    return t285
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t294 bool = self__102 == other__103
    return t294
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t297 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t297
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__134 *_goml_vec_uint8, elem__135 uint8) struct{} {
    vec_push__Vec_5uint8(self__134, elem__135)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t418 bool = index__6 < 0
    var jp416 bool
    if t418 {
        jp416 = true
    } else {
        var t419 bool = index__6 >= length__7
        jp416 = t419
    }
    if jp416 {
        var inline567 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline567
    } else {
        var t303 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t303))
        var t306 bool = first__8 < 128
        if t306 {
            var inline569 int = 1
            var inline570 Option__char = char_from_uint32(first__8)
            switch inline570.(type) {
            case Option__char_None:
                var inline571 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline571
            case Option__char_Some:
                var inline572 rune = inline570.(Option__char_Some)._0
                var inline574 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline572,
                    _2: inline569,
                }
                return inline574
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t310 bool = first__8 < 194
            if t310 {
                var inline576 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline576
            } else {
                var t314 bool = first__8 < 224
                if t314 {
                    var t327 int = length__7 - index__6
                    var t328 bool = t327 < 2
                    if t328 {
                        var inline578 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline578
                    } else {
                        var t316 int = index__6 + 1
                        var t317 uint8
                        var inline592 uint8 = _goml_runtime_core_string_byte_get(value__5, t316)
                        t317 = inline592
                        var second__9 uint32 = uint32(uint8(t317))
                        var t320 bool
                        var inline589 bool = second__9 < 128
                        if inline589 {
                            t320 = true
                        } else {
                            var inline590 bool = second__9 > 191
                            t320 = inline590
                        }
                        if t320 {
                            var inline580 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline580
                        } else {
                            var t322_rhs uint32 = 31
                            var t322 uint32 = first__8 & t322_rhs
                            var t323_rhs int = 6
                            var t323 uint32 = t322 << t323_rhs
                            var t324_rhs uint32 = 63
                            var t324 uint32 = second__9 & t324_rhs
                            var t325 uint32 = t323 | t324
                            var inline582 int = 2
                            var inline583 Option__char = char_from_uint32(t325)
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
                        }
                    }
                } else {
                    var t332 bool = first__8 < 240
                    if t332 {
                        var t365 int = length__7 - index__6
                        var t366 bool = t365 < 3
                        if t366 {
                            var inline594 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline594
                        } else {
                            var t334 int = index__6 + 1
                            var t335 uint8
                            var inline609 uint8 = _goml_runtime_core_string_byte_get(value__5, t334)
                            t335 = inline609
                            var second__10 uint32 = uint32(uint8(t335))
                            var t336 int = index__6 + 2
                            var t337 uint8
                            var inline607 uint8 = _goml_runtime_core_string_byte_get(value__5, t336)
                            t337 = inline607
                            var third__11 uint32 = uint32(uint8(t337))
                            var t363 bool = utf8_invalid_continuation(second__10)
                            var jp358 bool
                            if t363 {
                                jp358 = true
                            } else {
                                var inline596 bool = third__11 < 128
                                if inline596 {
                                    jp358 = true
                                } else {
                                    var inline597 bool = third__11 > 191
                                    jp358 = inline597
                                }
                            }
                            var jp352 bool
                            if jp358 {
                                jp352 = true
                            } else {
                                var t361 bool
                                var inline599 uint32 = 224
                                var inline600 bool = first__8 == inline599
                                t361 = inline600
                                if t361 {
                                    var t362 bool = second__10 < 160
                                    jp352 = t362
                                } else {
                                    jp352 = false
                                }
                            }
                            var jp341 bool
                            if jp352 {
                                jp341 = true
                            } else {
                                var t355 bool
                                var inline602 uint32 = 237
                                var inline603 bool = first__8 == inline602
                                t355 = inline603
                                if t355 {
                                    var t356 bool = second__10 >= 160
                                    jp341 = t356
                                } else {
                                    jp341 = false
                                }
                            }
                            if jp341 {
                                var inline605 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline605
                            } else {
                                var t343_rhs uint32 = 15
                                var t343 uint32 = first__8 & t343_rhs
                                var t344_rhs int = 12
                                var t344 uint32 = t343 << t344_rhs
                                var t345_rhs uint32 = 63
                                var t345 uint32 = second__10 & t345_rhs
                                var t346_rhs int = 6
                                var t346 uint32 = t345 << t346_rhs
                                var t347 uint32 = t344 | t346
                                var t348_rhs uint32 = 63
                                var t348 uint32 = third__11 & t348_rhs
                                var t349 uint32 = t347 | t348
                                var t350 Tuple3_4bool_4char_3int = utf8_valid_decode(t349, 3)
                                return t350
                            }
                        }
                    } else {
                        var t370 bool = first__8 < 245
                        if t370 {
                            var t411 int = length__7 - index__6
                            var t412 bool = t411 < 4
                            if t412 {
                                var t413 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t413
                            } else {
                                var t372 int = index__6 + 1
                                var t373 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t372)
                                var second__12 uint32 = uint32(uint8(t373))
                                var t374 int = index__6 + 2
                                var t375 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t374)
                                var third__13 uint32 = uint32(uint8(t375))
                                var t376 int = index__6 + 3
                                var t377 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t376)
                                var fourth__14 uint32 = uint32(uint8(t377))
                                var t409 bool = utf8_invalid_continuation(second__12)
                                var jp407 bool
                                if t409 {
                                    jp407 = true
                                } else {
                                    var t410 bool = utf8_invalid_continuation(third__13)
                                    jp407 = t410
                                }
                                var jp401 bool
                                if jp407 {
                                    jp401 = true
                                } else {
                                    var t408 bool = utf8_invalid_continuation(fourth__14)
                                    jp401 = t408
                                }
                                var jp395 bool
                                if jp401 {
                                    jp395 = true
                                } else {
                                    var t404 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t404 {
                                        var t405 bool = second__12 < 144
                                        jp395 = t405
                                    } else {
                                        jp395 = false
                                    }
                                }
                                var jp381 bool
                                if jp395 {
                                    jp381 = true
                                } else {
                                    var t398 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t398 {
                                        var t399 bool = second__12 > 143
                                        jp381 = t399
                                    } else {
                                        jp381 = false
                                    }
                                }
                                if jp381 {
                                    var t382 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t382
                                } else {
                                    var t383_rhs uint32 = 7
                                    var t383 uint32 = first__8 & t383_rhs
                                    var t384_rhs int = 18
                                    var t384 uint32 = t383 << t384_rhs
                                    var t385_rhs uint32 = 63
                                    var t385 uint32 = second__12 & t385_rhs
                                    var t386_rhs int = 12
                                    var t386 uint32 = t385 << t386_rhs
                                    var t387 uint32 = t384 | t386
                                    var t388_rhs uint32 = 63
                                    var t388 uint32 = third__13 & t388_rhs
                                    var t389_rhs int = 6
                                    var t389 uint32 = t388 << t389_rhs
                                    var t390 uint32 = t387 | t389
                                    var t391_rhs uint32 = 63
                                    var t391 uint32 = fourth__14 & t391_rhs
                                    var t392 uint32 = t390 | t391
                                    var t393 Tuple3_4bool_4char_3int = utf8_valid_decode(t392, 4)
                                    return t393
                                }
                            }
                        } else {
                            var t414 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t414
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t422 string = _goml_runtime_core_bool_to_string(self__66)
    return t422
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t425 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t425
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t428 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t428
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field630 rune
    var inline613 bool = utf8_valid_scalar(value__0)
    if inline613 {
        var inline614 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline616 rune = inline614._1
        commute_field630 = inline616
        var t434 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field630,
            _2: width__1,
        }
        return t434
    } else {
        var inline611 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline611
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t439 bool = value__3 < 128
    if t439 {
        return true
    } else {
        var t440 bool = value__3 > 191
        return t440
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t445 bool
    var inline620 bool = value__32 <= 1114111
    if inline620 {
        var inline621 bool = value__32 >= 55296
        var inline623 bool
        if inline621 {
            var inline625 bool = value__32 <= 57343
            inline623 = inline625
        } else {
            inline623 = false
        }
        var inline624 bool = !inline623
        t445 = inline624
    } else {
        t445 = false
    }
    if t445 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t446 Option__char = Option__char_Some{
            _0: x24,
        }
        return t446
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t451 bool = value__4 <= 1114111
    if t451 {
        var t455 bool = value__4 >= 55296
        var jp453 bool
        if t455 {
            var t456 bool = value__4 <= 57343
            jp453 = t456
        } else {
            jp453 = false
        }
        var t454 bool = !jp453
        return t454
    } else {
        return false
    }
}

func main() {
    main0()
}
