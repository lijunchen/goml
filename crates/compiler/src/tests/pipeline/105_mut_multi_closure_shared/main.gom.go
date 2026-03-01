package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_int32(reference *ref_int32_x, value int32) struct{} {
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
    var x__0 *ref_int32_x = ref__Ref_int32(0)
    var inc__1 closure_env_inc_0 = closure_env_inc_0{
        x_0: x__0,
    }
    var get__2 closure_env_get_1 = closure_env_get_1{
        x_0: x__0,
    }
    _goml_inherent_closure_env_inc_0_closure_env_inc_0_apply(inc__1)
    var t5 int32 = _goml_inherent_closure_env_get_1_closure_env_get_1_apply(get__2)
    var t6 string = int32_to_string(t5)
    string_println(t6)
    return struct{}{}
}

func _goml_inherent_closure_env_inc_0_closure_env_inc_0_apply(env3 closure_env_inc_0) int32 {
    var x__0 *ref_int32_x = env3.x_0
    var t7 int32 = ref_get__Ref_int32(x__0)
    var t8 int32 = t7 + 1
    ref_set__Ref_int32(x__0, t8)
    var t9 int32 = ref_get__Ref_int32(x__0)
    return t9
}

func _goml_inherent_closure_env_get_1_closure_env_get_1_apply(env4 closure_env_get_1) int32 {
    var x__0 *ref_int32_x = env4.x_0
    var t10 int32 = ref_get__Ref_int32(x__0)
    return t10
}

func main() {
    main0()
}
