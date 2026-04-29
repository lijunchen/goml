package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
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

type GoError = error

func main0() struct{} {
    var x__0 *ref_int32_x = ref__Ref_5int32(0)
    var inc__1 closure_env_inc_0 = closure_env_inc_0{
        x_0: x__0,
    }
    var get__2 closure_env_get_1 = closure_env_get_1{
        x_0: x__0,
    }
    _goml_inherent_x23_closure_x5f_env_x5f_inc_x5f_0_x23_closure_x5f_env_x5f_inc_x5f_0_x23_apply(inc__1)
    var t6 int32 = _goml_inherent_x23_closure_x5f_env_x5f_get_x5f_1_x23_closure_x5f_env_x5f_get_x5f_1_x23_apply(get__2)
    var t7 string = int32_to_string(t6)
    println__T_string(t7)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_inc_x5f_0_x23_closure_x5f_env_x5f_inc_x5f_0_x23_apply(env3 closure_env_inc_0) int32 {
    var retv11 int32
    var x__0 *ref_int32_x = env3.x_0
    var t12 int32 = ref_get__Ref_5int32(x__0)
    var t13 int32 = t12 + 1
    ref_set__Ref_5int32(x__0, t13)
    var t14 int32 = ref_get__Ref_5int32(x__0)
    retv11 = t14
    return retv11
}

func _goml_inherent_x23_closure_x5f_env_x5f_get_x5f_1_x23_closure_x5f_env_x5f_get_x5f_1_x23_apply(env4 closure_env_get_1) int32 {
    var retv16 int32
    var x__0 *ref_int32_x = env4.x_0
    var t17 int32 = ref_get__Ref_5int32(x__0)
    retv16 = t17
    return retv16
}

func main() {
    main0()
}
