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

type closure_env_get_0 struct {
    x_0 *ref_int_x
}

func main0() struct{} {
    var x__0 *ref_int_x = ref__Ref_3int(0)
    var get__1 closure_env_get_0 = closure_env_get_0{
        x_0: x__0,
    }
    ref_set__Ref_3int(x__0, 41)
    var t68 int = _goml_m_inherent_i_closure__env__get__0_i_closure__env__get__0_i_apply(get__1)
    var t69 string = _goml_m_inherent_i_int_i_int_i_to__string(t68)
    println__T_string(t69)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t71 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t71)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv74 string
    var t75 string = _goml_runtime_core_int_to_string(self__5)
    retv74 = t75
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv77 string
    retv77 = self__38
    return retv77
}

func _goml_m_inherent_i_closure__env__get__0_i_closure__env__get__0_i_apply(env66 closure_env_get_0) int {
    var retv85 int
    var x__0 *ref_int_x = env66.x_0
    var t86 int = ref_get__Ref_3int(x__0)
    retv85 = t86
    return retv85
}

func main() {
    main0()
}
