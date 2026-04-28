package main

import (
    _goml_fmt "fmt"
)

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

type closure_env_main_0 struct {
    signal_0 *ref_int32_x
}

type GoError = error

func child(signal__0 *ref_int32_x) struct{} {
    ref_set__Ref_5int32(signal__0, 1)
    return struct{}{}
}

func main0() struct{} {
    var signal__1 *ref_int32_x = ref__Ref_5int32(0)
    var t7 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    go _goml_inherent_closure_env_main_0_closure_env_main_0_apply(t7)
    Loop_loop9:
    for {
        var t10 int32 = ref_get__Ref_5int32(signal__1)
        var t11 bool = t10 < 1
        if t11 {
            continue
        } else {
            break Loop_loop9
        }
    }
    string_println("main")
    return struct{}{}
}

func _goml_inherent_closure_env_main_0_closure_env_main_0_apply(env3 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x = env3.signal_0
    child(signal__1)
    return struct{}{}
}

func main() {
    main0()
}
