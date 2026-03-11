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

type closure_env_get_0 struct {
    x_0 *ref_int32_x
}

type GoError = error

func main0() struct{} {
    var x__0 *ref_int32_x = ref__Ref_int32(0)
    var get__1 closure_env_get_0 = closure_env_get_0{
        x_0: x__0,
    }
    ref_set__Ref_int32(x__0, 41)
    var t4 int32 = _goml_inherent_closure_env_get_0_closure_env_get_0_apply(get__1)
    var t5 string = int32_to_string(t4)
    string_println(t5)
    return struct{}{}
}

func _goml_inherent_closure_env_get_0_closure_env_get_0_apply(env2 closure_env_get_0) int32 {
    var retv7 int32
    var x__0 *ref_int32_x = env2.x_0
    var t8 int32 = ref_get__Ref_int32(x__0)
    retv7 = t8
    return retv7
}

func main() {
    main0()
}
