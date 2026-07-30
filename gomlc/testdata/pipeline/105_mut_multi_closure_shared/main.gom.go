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

type closure_env_get_1 struct {
    x_0 *ref_int_x
}

func main0() struct{} {
    var x__0 *ref_int_x = ref__Ref_3int(0)
    var inc__1 closure_env_inc_0 = closure_env_inc_0{
        x_0: x__0,
    }
    var get__2 closure_env_get_1 = closure_env_get_1{
        x_0: x__0,
    }
    _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(inc__1)
    var t74 int = _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(get__2)
    var t75 string = _goml_m_inherent_i_int_i_int_i_to__string(t74)
    println__T_string(t75)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t77)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv80 string
    var t81 string = _goml_runtime_core_int_to_string(self__5)
    retv80 = t81
    return retv80
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv83 string
    retv83 = self__38
    return retv83
}

func _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(env71 closure_env_inc_0) int {
    var retv91 int
    var x__0 *ref_int_x = env71.x_0
    var t92 int = ref_get__Ref_3int(x__0)
    var t93 int = t92 + 1
    ref_set__Ref_3int(x__0, t93)
    var t94 int = ref_get__Ref_3int(x__0)
    retv91 = t94
    return retv91
}

func _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(env72 closure_env_get_1) int {
    var retv96 int
    var x__0 *ref_int_x = env72.x_0
    var t97 int = ref_get__Ref_3int(x__0)
    retv96 = t97
    return retv96
}

func main() {
    main0()
}
