package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_3_5uint8(arr [3]uint8, index int) uint8 {
    return arr[index]
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type Ordering int32

func answer() int {
    var base__0 int = 40
    var t421 int = base__0 + 2
    return t421
}

func loop_answer() int {
    var jp425 int
    var base__1 int = 6
    var t427 int = base__1 * 7
    jp425 = t427
    return jp425
}

func main0() struct{} {
    var plain__2 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{65, 10, 66},
    }
    var empty__3 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{},
    }
    var raw__4 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{114, 97, 119, 32, 92, 110, 32, 98, 121, 116, 101, 115},
    }
    var quoted__5 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{113, 117, 111, 116, 101, 100, 32, 34, 116, 101, 120, 116, 34, 32, 97, 110, 100, 32, 35},
    }
    var fixed__6 [3]uint8 = [3]uint8{65, 66, 67}
    var value__7 int = answer()
    var t429 int = loop_answer()
    var t430 int = value__7 + t429
    var t431 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(plain__2)
    var t432 string = _goml_m_inherent_i_int_i_int_i_to__string(t431)
    println__T_string(t432)
    var t433 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(empty__3)
    var t434 string = _goml_m_inherent_i_int_i_int_i_to__string(t433)
    println__T_string(t434)
    var t435 uint8
    var inline518 int = 0
    var inline519 uint8 = vec_get__Vec_5uint8(plain__2, inline518)
    t435 = inline519
    var t436 int = int(uint8(t435))
    var t437 string
    var inline516 string = _goml_runtime_core_int_to_string(t436)
    t437 = inline516
    var inline513 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline513)
    var t438 uint8
    var inline510 int = 1
    var inline511 uint8 = vec_get__Vec_5uint8(plain__2, inline510)
    t438 = inline511
    var t439 int = int(uint8(t438))
    var t440 string
    var inline508 string = _goml_runtime_core_int_to_string(t439)
    t440 = inline508
    var inline505 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
    _goml_runtime_core_string_println(inline505)
    var t441 uint8
    var inline502 int = 2
    var inline503 uint8 = vec_get__Vec_5uint8(plain__2, inline502)
    t441 = inline503
    var t442 int = int(uint8(t441))
    var t443 string
    var inline500 string = _goml_runtime_core_int_to_string(t442)
    t443 = inline500
    var inline497 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline497)
    var t444 int
    var inline495 int = vec_len__Vec_5uint8(raw__4)
    t444 = inline495
    var t445 string
    var inline493 string = _goml_runtime_core_int_to_string(t444)
    t445 = inline493
    var inline490 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t445)
    _goml_runtime_core_string_println(inline490)
    var t446 int
    var inline488 int = vec_len__Vec_5uint8(quoted__5)
    t446 = inline488
    var t447 string
    var inline486 string = _goml_runtime_core_int_to_string(t446)
    t447 = inline486
    var inline483 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t447)
    _goml_runtime_core_string_println(inline483)
    var t448 uint8 = array_get__Array_3_5uint8(fixed__6, 2)
    var t449 int = int(uint8(t448))
    var t450 string
    var inline481 string = _goml_runtime_core_int_to_string(t449)
    t450 = inline481
    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t450)
    _goml_runtime_core_string_println(inline478)
    var t451 string
    var inline476 string = _goml_runtime_core_int_to_string(t430)
    t451 = inline476
    var inline473 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t451)
    _goml_runtime_core_string_println(inline473)
    var inline469 string = "block condition"
    var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline469)
    _goml_runtime_core_string_println(inline470)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t455 string
    t455 = value__1
    _goml_runtime_core_string_println(t455)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__273 *_goml_vec_uint8) int {
    var t459 int = vec_len__Vec_5uint8(self__273)
    return t459
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t462 string = _goml_runtime_core_int_to_string(self__32)
    return t462
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
