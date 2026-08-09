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

func main0() struct{} {
    var counter__8 *ref_int_x
    var inline214 int = 0
    var inline215 *ref_int_x = ref__Ref_3int(inline214)
    counter__8 = inline215
    var inline210 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(counter__8)
    var inline211 int = inline210 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(counter__8, inline211)
    var t178 int
    var inline208 int = ref_get__Ref_3int(counter__8)
    t178 = inline208
    var inline205 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t178)
    _goml_runtime_core_string_println(inline205)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__258 *ref_int_x) int {
    var t182 int = ref_get__Ref_3int(self__258)
    return t182
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__259 *ref_int_x, value__260 int) struct{} {
    ref_set__Ref_3int(self__259, value__260)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t193 string = _goml_runtime_core_int_to_string(self__69)
    return t193
}

func main() {
    main0()
}
