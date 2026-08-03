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

type closure_env_inc_0 struct {
    x_0 *ref_int_x
}

func main0() struct{} {
    var x__0 *ref_int_x = ref__Ref_3int(0)
    var inline210 int = ref_get__Ref_3int(x__0)
    var inline211 int = inline210 + 1
    ref_set__Ref_3int(x__0, inline211)
    ref_get__Ref_3int(x__0)
    var t182 int = ref_get__Ref_3int(x__0)
    var t183 string
    var inline207 string = _goml_runtime_core_int_to_string(t182)
    t183 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
