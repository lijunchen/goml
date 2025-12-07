package main

import (
    "fmt"
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
    signal_0 *ref_int32_x
}

func child(signal__0 *ref_int32_x) struct{} {
    var ret6 struct{}
    ret6 = ref_set__Ref_int32(signal__0, 1)
    return ret6
}

func main0() struct{} {
    var ret7 struct{}
    var signal__1 *ref_int32_x = ref__Ref_int32(0)
    var t4 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    go impl_inherent_closure_env_main_0_apply(t4)
    var cond8 bool
    for {
        var t5 int32 = ref_get__Ref_int32(signal__1)
        cond8 = t5 < 1
        if !cond8 {
            break
        }
    }
    string_println("main")
    ret7 = struct{}{}
    return ret7
}

func impl_inherent_closure_env_main_0_apply(env3 closure_env_main_0) struct{} {
    var ret9 struct{}
    var signal__1 *ref_int32_x = env3.signal_0
    ret9 = child(signal__1)
    return ret9
}

func main() {
    main0()
}
