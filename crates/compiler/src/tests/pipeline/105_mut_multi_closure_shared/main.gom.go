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

type closure_env_get_1 struct {
    x_0 *ref_int32_x
}

func main0() struct{} {
    var x__0 *ref_int32_x = ref__Ref_5int32(0)
    var inc__1 closure_env_inc_0 = closure_env_inc_0{
        x_0: x__0,
    }
    var get__2 closure_env_get_1 = closure_env_get_1{
        x_0: x__0,
    }
    _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(inc__1)
    var t67 int32 = _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(get__2)
    var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t67)
    println__T_string(t68)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t70 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t70)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv73 string
    var t74 string = _goml_runtime_core_int32_to_string(self__5)
    retv73 = t74
    return retv73
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv76 string
    retv76 = self__37
    return retv76
}

func _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(env64 closure_env_inc_0) int32 {
    var retv84 int32
    var x__0 *ref_int32_x = env64.x_0
    var t85 int32 = ref_get__Ref_5int32(x__0)
    var t86 int32 = t85 + 1
    ref_set__Ref_5int32(x__0, t86)
    var t87 int32 = ref_get__Ref_5int32(x__0)
    retv84 = t87
    return retv84
}

func _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(env65 closure_env_get_1) int32 {
    var retv89 int32
    var x__0 *ref_int32_x = env65.x_0
    var t90 int32 = ref_get__Ref_5int32(x__0)
    retv89 = t90
    return retv89
}

func main() {
    main0()
}
