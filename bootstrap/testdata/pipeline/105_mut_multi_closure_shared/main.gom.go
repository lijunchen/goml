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
    var t70 int = _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(get__2)
    var t71 string = _goml_m_inherent_i_int_i_int_i_to__string(t70)
    println__T_string(t71)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t73 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t73)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int_to_string(self__5)
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv79 string
    retv79 = self__38
    return retv79
}

func _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(env67 closure_env_inc_0) int {
    var retv87 int
    var x__0 *ref_int_x = env67.x_0
    var t88 int = ref_get__Ref_3int(x__0)
    var t89 int = t88 + 1
    ref_set__Ref_3int(x__0, t89)
    var t90 int = ref_get__Ref_3int(x__0)
    retv87 = t90
    return retv87
}

func _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(env68 closure_env_get_1) int {
    var retv92 int
    var x__0 *ref_int_x = env68.x_0
    var t93 int = ref_get__Ref_3int(x__0)
    retv92 = t93
    return retv92
}

func main() {
    main0()
}
