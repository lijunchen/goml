package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var v__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var inline514 int32 = 10
    vec_push__Vec_5int32(v__0, inline514)
    var inline511 int32 = 20
    vec_push__Vec_5int32(v__0, inline511)
    var inline508 int32 = 30
    vec_push__Vec_5int32(v__0, inline508)
    var inline505 int32 = 40
    vec_push__Vec_5int32(v__0, inline505)
    var s__1 []int32
    var inline501 int = 1
    var inline502 int = 4
    var inline503 []int32 = v__0.items[inline501:inline502]
    s__1 = inline503
    var t420 int
    var inline499 int = len(s__1)
    t420 = inline499
    var inline496 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t420)
    _goml_runtime_core_string_println(inline496)
    var t421 int32
    var inline493 int = 0
    var inline494 int32 = s__1[inline493]
    t421 = inline494
    var inline490 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t421)
    _goml_runtime_core_string_println(inline490)
    var t422 int32
    var inline487 int = 1
    var inline488 int32 = s__1[inline487]
    t422 = inline488
    var inline484 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t422)
    _goml_runtime_core_string_println(inline484)
    var t423 int32
    var inline481 int = 2
    var inline482 int32 = s__1[inline481]
    t423 = inline482
    var inline478 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t423)
    _goml_runtime_core_string_println(inline478)
    var t__2 []int32
    var inline474 int = 1
    var inline475 int = 3
    var inline476 []int32 = s__1[inline474:inline475]
    t__2 = inline476
    var t424 int
    var inline472 int = len(t__2)
    t424 = inline472
    var inline469 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t424)
    _goml_runtime_core_string_println(inline469)
    var t425 int32
    var inline466 int = 0
    var inline467 int32 = t__2[inline466]
    t425 = inline467
    var inline463 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t425)
    _goml_runtime_core_string_println(inline463)
    var t426 int32
    var inline460 int = 1
    var inline461 int32 = t__2[inline460]
    t426 = inline461
    var inline457 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t426)
    _goml_runtime_core_string_println(inline457)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t429 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t429
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t452 string = _goml_runtime_core_int_to_string(self__151)
    return t452
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t455 string = _goml_runtime_core_int32_to_string(self__154)
    return t455
}

func main() {
    main0()
}
