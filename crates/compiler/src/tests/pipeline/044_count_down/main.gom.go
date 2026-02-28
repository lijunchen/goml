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
    var _wild3 struct{}
    var t8 int32
    var t9 bool
    _ = _wild3
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            counter__0 = ref__Ref_int32(0)
            t5 = closure_env_main_0{
                counter_0: counter__0,
            }
            go _goml_inherent_closure_env_main_0_closure_env_main_0_apply(t5)
            pc = 2
        case 1:
            return struct{}{}
        case 2:
            t8 = ref_get__Ref_int32(counter__0)
            t9 = t8 < 10
            if t9 {
                pc = 3
            } else {
                pc = 4
            }
        case 3:
            pc = 2
        case 4:
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_closure_env_main_0_closure_env_main_0_apply(env4 closure_env_main_0) struct{} {
    var counter__0 *ref_int32_x
    var t12 int32
    var t13 bool
    var _wild0 struct{}
    var t14 Duration
    var _wild1 struct{}
    var t15 int32
    var t16 int32
    var _wild2 struct{}
    _ = _wild0
    _ = _wild1
    _ = _wild2
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            counter__0 = env4.counter_0
            pc = 2
        case 1:
            return struct{}{}
        case 2:
            t12 = ref_get__Ref_int32(counter__0)
            t13 = t12 < 10
            if t13 {
                pc = 3
            } else {
                pc = 4
            }
        case 3:
            string_println("hello")
            t14 = time.Duration(1000)
            time.Sleep(t14)
            t15 = ref_get__Ref_int32(counter__0)
            t16 = t15 + 1
            ref_set__Ref_int32(counter__0, t16)
            pc = 2
        case 4:
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
