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
    var t28 int32 = _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(get__2)
    var t29 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t28)
    println__T_string(t29)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t31 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t31)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv34 string
    var t35 string = _goml_runtime_core_int32_to_string(self__2)
    retv34 = t35
    return retv34
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv37 string
    retv37 = self__9
    return retv37
}

func _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(env25 closure_env_inc_0) int32 {
    var retv45 int32
    var x__0 *ref_int32_x = env25.x_0
    var t46 int32 = ref_get__Ref_5int32(x__0)
    var t47 int32 = t46 + 1
    ref_set__Ref_5int32(x__0, t47)
    var t48 int32 = ref_get__Ref_5int32(x__0)
    retv45 = t48
    return retv45
}

func _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(env26 closure_env_get_1) int32 {
    var retv50 int32
    var x__0 *ref_int32_x = env26.x_0
    var t51 int32 = ref_get__Ref_5int32(x__0)
    retv50 = t51
    return retv50
}

func main() {
    main0()
}
