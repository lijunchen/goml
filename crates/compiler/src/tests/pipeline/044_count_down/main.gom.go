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
    var ret11 struct{}
    var counter__0 *ref_int32_x = ref__Ref_int32(0)
    var t5 closure_env_main_0 = closure_env_main_0{
        counter_0: counter__0,
    }
    go impl_inherent_closure_env_main_0_apply(t5)
    var cond12 bool
    for {
        var t6 int32 = ref_get__Ref_int32(counter__0)
        cond12 = t6 < 10
        if !cond12 {
            break
        }
    }
    ret11 = struct{}{}
    return ret11
}

func impl_inherent_closure_env_main_0_apply(env4 closure_env_main_0) struct{} {
    var ret13 struct{}
    var counter__0 *ref_int32_x = env4.counter_0
    var cond14 bool
    for {
        var t7 int32 = ref_get__Ref_int32(counter__0)
        cond14 = t7 < 10
        if !cond14 {
            break
        }
        string_println("hello")
        var t8 Duration = time.Duration(1000)
        time.Sleep(t8)
        var t10 int32 = ref_get__Ref_int32(counter__0)
        var t9 int32 = t10 + 1
        ref_set__Ref_int32(counter__0, t9)
    }
    ret13 = struct{}{}
    return ret13
}

func main() {
    main0()
}
