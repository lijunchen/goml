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
    var t6 int32 = _goml_inherent_closure_env_get_1_closure_env_get_1_apply(get__2)
    var t7 string = int32_to_string(t6)
    string_println(t7)
    return struct{}{}
}

func _goml_inherent_closure_env_inc_0_closure_env_inc_0_apply(env3 closure_env_inc_0) int32 {
    var retv9 int32
    var x__0 *ref_int32_x = env3.x_0
    var t10 int32 = ref_get__Ref_int32(x__0)
    var t11 int32 = t10 + 1
    ref_set__Ref_int32(x__0, t11)
    var t12 int32 = ref_get__Ref_int32(x__0)
    retv9 = t12
    return retv9
}

func _goml_inherent_closure_env_get_1_closure_env_get_1_apply(env4 closure_env_get_1) int32 {
    var retv14 int32
    var x__0 *ref_int32_x = env4.x_0
    var t15 int32 = ref_get__Ref_int32(x__0)
    retv14 = t15
    return retv14
}

func main() {
    main0()
}
