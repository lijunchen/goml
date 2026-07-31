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
    var inc__1 closure_env_inc_0 = closure_env_inc_0{
        x_0: x__0,
    }
    _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(inc__1)
    var t157 int = ref_get__Ref_3int(x__0)
    var t158 string = _goml_m_inherent_i_int_i_int_i_to__string(t157)
    println__T_string(t158)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t160 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t160)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv163 string
    var t164 string = _goml_runtime_core_int_to_string(self__5)
    retv163 = t164
    return retv163
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv166 string
    retv166 = self__38
    return retv166
}

func _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(env155 closure_env_inc_0) int {
    var retv174 int
    var x__0 *ref_int_x = env155.x_0
    var t175 int = ref_get__Ref_3int(x__0)
    var t176 int = t175 + 1
    ref_set__Ref_3int(x__0, t176)
    var t177 int = ref_get__Ref_3int(x__0)
    retv174 = t177
    return retv174
}

func main() {
    main0()
}
