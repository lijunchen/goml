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
    var inline224 int = 0
    var inline225 *ref_int_x = ref__Ref_3int(inline224)
    counter__8 = inline225
    var inline220 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(counter__8)
    var inline221 int = inline220 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(counter__8, inline221)
    var t188 int
    var inline218 int = ref_get__Ref_3int(counter__8)
    t188 = inline218
    var inline215 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t188)
    _goml_runtime_core_string_println(inline215)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__271 *ref_int_x) int {
    var t192 int = ref_get__Ref_3int(self__271)
    return t192
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__272 *ref_int_x, value__273 int) struct{} {
    ref_set__Ref_3int(self__272, value__273)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t203 string = _goml_runtime_core_int_to_string(self__67)
    return t203
}

func main() {
    main0()
}
