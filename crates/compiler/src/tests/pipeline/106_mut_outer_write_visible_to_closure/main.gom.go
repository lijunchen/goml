package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type closure_env_get_0 struct {
    x_0 *ref_int32_x
}

func main0() struct{} {
    var x__0 *ref_int32_x = ref__Ref_5int32(0)
    var get__1 closure_env_get_0 = closure_env_get_0{
        x_0: x__0,
    }
    ref_set__Ref_5int32(x__0, 41)
    var t26 int32 = _goml_m_inherent_i_closure__env__get__0_i_closure__env__get__0_i_apply(get__1)
    var t27 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t26)
    println__T_string(t27)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t29 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t29)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv32 string
    var t33 string = _goml_runtime_core_int32_to_string(self__2)
    retv32 = t33
    return retv32
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv35 string
    retv35 = self__9
    return retv35
}

func _goml_m_inherent_i_closure__env__get__0_i_closure__env__get__0_i_apply(env24 closure_env_get_0) int32 {
    var retv43 int32
    var x__0 *ref_int32_x = env24.x_0
    var t44 int32 = ref_get__Ref_5int32(x__0)
    retv43 = t44
    return retv43
}

func main() {
    main0()
}
