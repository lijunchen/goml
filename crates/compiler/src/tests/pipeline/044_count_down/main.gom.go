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
    var counter__0 *ref_int32_x
    var t5 closure_env_main_0
    var t8 int32
    var t9 bool
    counter__0 = ref__Ref_int32(0)
    t5 = closure_env_main_0{
        counter_0: counter__0,
    }
    go _goml_inherent_closure_env_main_0_closure_env_main_0_apply(t5)
    goto b2
    b1:
    return struct{}{}
    b2:
    t8 = ref_get__Ref_int32(counter__0)
    t9 = t8 < 10
    if t9 {
        goto b3
    } else {
        goto b4
    }
    b3:
    goto b2
    b4:
    goto b1
}

func _goml_inherent_closure_env_main_0_closure_env_main_0_apply(env4 closure_env_main_0) struct{} {
    var counter__0 *ref_int32_x
    var t12 int32
    var t13 bool
    var t14 Duration
    var t15 int32
    var t16 int32
    counter__0 = env4.counter_0
    goto b2
    b1:
    return struct{}{}
    b2:
    t12 = ref_get__Ref_int32(counter__0)
    t13 = t12 < 10
    if t13 {
        goto b3
    } else {
        goto b4
    }
    b3:
    string_println("hello")
    t14 = time.Duration(1000)
    time.Sleep(t14)
    t15 = ref_get__Ref_int32(counter__0)
    t16 = t15 + 1
    ref_set__Ref_int32(counter__0, t16)
    goto b2
    b4:
    goto b1
}

func main() {
    main0()
}
