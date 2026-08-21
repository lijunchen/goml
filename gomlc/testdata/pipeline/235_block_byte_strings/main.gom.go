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
    var t424 int = base__0 + 2
    return t424
}

func loop_answer() int {
    var jp428 int
    var base__1 int = 6
    var t430 int = base__1 * 7
    jp428 = t430
    return jp428
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
    var t432 int = loop_answer()
    var t433 int = value__7 + t432
    var t434 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(plain__2)
    var t435 string = _goml_m_inherent_i_int_i_int_i_to__string(t434)
    println__T_string(t435)
    var t436 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(empty__3)
    var t437 string = _goml_m_inherent_i_int_i_int_i_to__string(t436)
    println__T_string(t437)
    var t438 uint8
    var inline521 int = 0
    var inline522 uint8 = vec_get__Vec_5uint8(plain__2, inline521)
    t438 = inline522
    var t439 int = int(uint8(t438))
    var t440 string
    var inline519 string = _goml_runtime_core_int_to_string(t439)
    t440 = inline519
    var inline516 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
    _goml_runtime_core_string_println(inline516)
    var t441 uint8
    var inline513 int = 1
    var inline514 uint8 = vec_get__Vec_5uint8(plain__2, inline513)
    t441 = inline514
    var t442 int = int(uint8(t441))
    var t443 string
    var inline511 string = _goml_runtime_core_int_to_string(t442)
    t443 = inline511
    var inline508 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline508)
    var t444 uint8
    var inline505 int = 2
    var inline506 uint8 = vec_get__Vec_5uint8(plain__2, inline505)
    t444 = inline506
    var t445 int = int(uint8(t444))
    var t446 string
    var inline503 string = _goml_runtime_core_int_to_string(t445)
    t446 = inline503
    var inline500 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t446)
    _goml_runtime_core_string_println(inline500)
    var t447 int
    var inline498 int = vec_len__Vec_5uint8(raw__4)
    t447 = inline498
    var t448 string
    var inline496 string = _goml_runtime_core_int_to_string(t447)
    t448 = inline496
    var inline493 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t448)
    _goml_runtime_core_string_println(inline493)
    var t449 int
    var inline491 int = vec_len__Vec_5uint8(quoted__5)
    t449 = inline491
    var t450 string
    var inline489 string = _goml_runtime_core_int_to_string(t449)
    t450 = inline489
    var inline486 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t450)
    _goml_runtime_core_string_println(inline486)
    var t451 uint8 = array_get__Array_3_5uint8(fixed__6, 2)
    var t452 int = int(uint8(t451))
    var t453 string
    var inline484 string = _goml_runtime_core_int_to_string(t452)
    t453 = inline484
    var inline481 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t453)
    _goml_runtime_core_string_println(inline481)
    var t454 string
    var inline479 string = _goml_runtime_core_int_to_string(t433)
    t454 = inline479
    var inline476 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t454)
    _goml_runtime_core_string_println(inline476)
    var inline472 string = "block condition"
    var inline473 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline472)
    _goml_runtime_core_string_println(inline473)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t458 string
    t458 = value__1
    _goml_runtime_core_string_println(t458)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__273 *_goml_vec_uint8) int {
    var t462 int = vec_len__Vec_5uint8(self__273)
    return t462
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t465 string = _goml_runtime_core_int_to_string(self__32)
    return t465
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
