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
    var inline229 int = 0
    var inline230 *ref_int_x = ref__Ref_3int(inline229)
    counter__8 = inline230
    var inline225 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(counter__8)
    var inline226 int = inline225 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(counter__8, inline226)
    var t193 int
    var inline223 int = ref_get__Ref_3int(counter__8)
    t193 = inline223
    var inline220 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t193)
    _goml_runtime_core_string_println(inline220)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__274 *ref_int_x) int {
    var t197 int = ref_get__Ref_3int(self__274)
    return t197
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__275 *ref_int_x, value__276 int) struct{} {
    ref_set__Ref_3int(self__275, value__276)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t208 string = _goml_runtime_core_int_to_string(self__67)
    return t208
}

func main() {
    main0()
}
