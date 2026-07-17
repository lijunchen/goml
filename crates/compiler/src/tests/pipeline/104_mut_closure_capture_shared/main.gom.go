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

type closure_env_inc_0 struct {
    x_0 *ref_int32_x
}

func main0() struct{} {
    var x__0 *ref_int32_x = ref__Ref_5int32(0)
    var inc__1 closure_env_inc_0 = closure_env_inc_0{
        x_0: x__0,
    }
    _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(inc__1)
    var t66 int32 = ref_get__Ref_5int32(x__0)
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t66)
    println__T_string(t67)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t69)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv72 string
    var t73 string = _goml_runtime_core_int32_to_string(self__5)
    retv72 = t73
    return retv72
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv75 string
    retv75 = self__37
    return retv75
}

func _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(env64 closure_env_inc_0) int32 {
    var retv83 int32
    var x__0 *ref_int32_x = env64.x_0
    var t84 int32 = ref_get__Ref_5int32(x__0)
    var t85 int32 = t84 + 1
    ref_set__Ref_5int32(x__0, t85)
    var t86 int32 = ref_get__Ref_5int32(x__0)
    retv83 = t86
    return retv83
}

func main() {
    main0()
}
