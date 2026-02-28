package main

import (
    "fmt"
    "time"
)

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

type closure_env_main_0 struct {
    counter_0 *ref_int32_x
}

type Duration = time.Duration

func main0() struct{} {
    var counter__0 *ref_int32_x = ref__Ref_int32(0)
    var t5 closure_env_main_0 = closure_env_main_0{
        counter_0: counter__0,
    }
    go _goml_inherent_closure_env_main_0_closure_env_main_0_apply(t5)
    for {
        var t8 int32 = ref_get__Ref_int32(counter__0)
        var t9 bool = t8 < 10
        if !t9 {
            break
        }
        continue
    }
    return struct{}{}
}

func _goml_inherent_closure_env_main_0_closure_env_main_0_apply(env4 closure_env_main_0) struct{} {
    var counter__0 *ref_int32_x = env4.counter_0
    for {
        var t12 int32 = ref_get__Ref_int32(counter__0)
        var t13 bool = t12 < 10
        if !t13 {
            break
        }
        string_println("hello")
        var t14 Duration = time.Duration(1000)
        time.Sleep(t14)
        var t15 int32 = ref_get__Ref_int32(counter__0)
        var t16 int32 = t15 + 1
        ref_set__Ref_int32(counter__0, t16)
        continue
    }
    return struct{}{}
}

func main() {
    main0()
}
