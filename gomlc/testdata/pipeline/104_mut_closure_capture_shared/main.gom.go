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
    var t113 int = ref_get__Ref_3int(x__0)
    var t114 string = _goml_m_inherent_i_int_i_int_i_to__string(t113)
    println__T_string(t114)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t116 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t116)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv119 string
    var t120 string = _goml_runtime_core_int_to_string(self__5)
    retv119 = t120
    return retv119
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv122 string
    retv122 = self__38
    return retv122
}

func _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(env111 closure_env_inc_0) int {
    var retv130 int
    var x__0 *ref_int_x = env111.x_0
    var t131 int = ref_get__Ref_3int(x__0)
    var t132 int = t131 + 1
    ref_set__Ref_3int(x__0, t132)
    var t133 int = ref_get__Ref_3int(x__0)
    retv130 = t133
    return retv130
}

func main() {
    main0()
}
