package main

import (
    _goml_fmt "fmt"
    "time"
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
    counter_0 *ref_int32_x
}

type GoError = error

type Duration = time.Duration

func main0() struct{} {
    var counter__0 *ref_int32_x = ref__Ref_5int32(0)
    var t6 closure_env_main_0 = closure_env_main_0{
        counter_0: counter__0,
    }
    go _goml_inherent_x23_closure_x5f_env_x5f_main_x5f_0_x23_closure_x5f_env_x5f_main_x5f_0_x23_apply(t6)
    Loop_loop8:
    for {
        var t9 int32 = ref_get__Ref_5int32(counter__0)
        var t10 bool = t9 < 10
        if t10 {
            continue
        } else {
            break Loop_loop8
        }
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_main_x5f_0_x23_closure_x5f_env_x5f_main_x5f_0_x23_apply(env4 closure_env_main_0) struct{} {
    var counter__0 *ref_int32_x = env4.counter_0
    Loop_loop15:
    for {
        var t16 int32 = ref_get__Ref_5int32(counter__0)
        var t17 bool = t16 < 10
        if t17 {
            println__T_string("hello")
            var t18 Duration = time.Duration(1000)
            time.Sleep(t18)
            var t19 int32 = ref_get__Ref_5int32(counter__0)
            var t20 int32 = t19 + 1
            ref_set__Ref_5int32(counter__0, t20)
            continue
        } else {
            break Loop_loop15
        }
    }
    return struct{}{}
}

func main() {
    main0()
}
