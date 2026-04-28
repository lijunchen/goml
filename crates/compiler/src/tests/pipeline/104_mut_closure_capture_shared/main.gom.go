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

type GoError = error

func main0() struct{} {
    var x__0 *ref_int32_x = ref__Ref_5int32(0)
    var inc__1 closure_env_inc_0 = closure_env_inc_0{
        x_0: x__0,
    }
    _goml_inherent_closure_env_inc_0_closure_env_inc_0_apply(inc__1)
    var t5 int32 = ref_get__Ref_5int32(x__0)
    var t6 string = int32_to_string(t5)
    string_println(t6)
    return struct{}{}
}

func _goml_inherent_closure_env_inc_0_closure_env_inc_0_apply(env3 closure_env_inc_0) int32 {
    var retv8 int32
    var x__0 *ref_int32_x = env3.x_0
    var t9 int32 = ref_get__Ref_5int32(x__0)
    var t10 int32 = t9 + 1
    ref_set__Ref_5int32(x__0, t10)
    var t11 int32 = ref_get__Ref_5int32(x__0)
    retv8 = t11
    return retv8
}

func main() {
    main0()
}
