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

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var counter__8 *ref_int_x
    var inline450 int = 0
    var inline451 *ref_int_x = ref__Ref_3int(inline450)
    counter__8 = inline451
    var inline446 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(counter__8)
    var inline447 int = inline446 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(counter__8, inline447)
    var t414 int
    var inline444 int = ref_get__Ref_3int(counter__8)
    t414 = inline444
    var inline441 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t414)
    _goml_runtime_core_string_println(inline441)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__432 *ref_int_x) int {
    var t418 int = ref_get__Ref_3int(self__432)
    return t418
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__433 *ref_int_x, value__434 int) struct{} {
    ref_set__Ref_3int(self__433, value__434)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t429 string = _goml_runtime_core_int_to_string(self__151)
    return t429
}

func main() {
    main0()
}
