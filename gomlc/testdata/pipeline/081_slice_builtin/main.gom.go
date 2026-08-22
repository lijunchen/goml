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
    var v__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32()
    var inline517 int32 = 10
    vec_push__Vec_5int32(v__0, inline517)
    var inline514 int32 = 20
    vec_push__Vec_5int32(v__0, inline514)
    var inline511 int32 = 30
    vec_push__Vec_5int32(v__0, inline511)
    var inline508 int32 = 40
    vec_push__Vec_5int32(v__0, inline508)
    var s__1 []int32
    var inline504 int = 1
    var inline505 int = 4
    var inline506 []int32 = v__0.items[inline504:inline505]
    s__1 = inline506
    var t423 int
    var inline502 int = len(s__1)
    t423 = inline502
    var inline499 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t423)
    _goml_runtime_core_string_println(inline499)
    var t424 int32
    var inline496 int = 0
    var inline497 int32 = s__1[inline496]
    t424 = inline497
    var inline493 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t424)
    _goml_runtime_core_string_println(inline493)
    var t425 int32
    var inline490 int = 1
    var inline491 int32 = s__1[inline490]
    t425 = inline491
    var inline487 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t425)
    _goml_runtime_core_string_println(inline487)
    var t426 int32
    var inline484 int = 2
    var inline485 int32 = s__1[inline484]
    t426 = inline485
    var inline481 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t426)
    _goml_runtime_core_string_println(inline481)
    var t__2 []int32
    var inline477 int = 1
    var inline478 int = 3
    var inline479 []int32 = s__1[inline477:inline478]
    t__2 = inline479
    var t427 int
    var inline475 int = len(t__2)
    t427 = inline475
    var inline472 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t427)
    _goml_runtime_core_string_println(inline472)
    var t428 int32
    var inline469 int = 0
    var inline470 int32 = t__2[inline469]
    t428 = inline470
    var inline466 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t428)
    _goml_runtime_core_string_println(inline466)
    var t429 int32
    var inline463 int = 1
    var inline464 int32 = t__2[inline463]
    t429 = inline464
    var inline460 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t429)
    _goml_runtime_core_string_println(inline460)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32() *_goml_vec_int32 {
    var t432 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t432
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t455 string = _goml_runtime_core_int_to_string(self__151)
    return t455
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t458 string = _goml_runtime_core_int32_to_string(self__154)
    return t458
}

func main() {
    main0()
}
