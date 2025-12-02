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
    var ret7 struct{}
    ret7 = ref_set__Ref_int32(signal__0, 1)
    return ret7
}

func main0() struct{} {
    var ret8 struct{}
    var signal__1 *ref_int32_x = ref__Ref_int32(0)
    var t5 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    go impl_inherent_closure_env_main_0_apply(t5)
    var cond9 bool
    for {
        var t6 int32 = ref_get__Ref_int32(signal__1)
        cond9 = t6 < 1
        if !cond9 {
            break
        }
    }
    string_println("main")
    ret8 = struct{}{}
    return ret8
}

func impl_inherent_closure_env_main_0_apply(env4 closure_env_main_0) struct{} {
    var ret10 struct{}
    var signal__1 *ref_int32_x = env4.signal_0
    child(signal__1)
    ret10 = struct{}{}
    return ret10
}

func main() {
    main0()
}
