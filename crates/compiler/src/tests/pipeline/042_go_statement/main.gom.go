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
    var t4 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t4 = ref_set__Ref_int32(signal__0, 1)
            return t4
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var signal__1 *ref_int32_x
    var t5 closure_env_main_0
    var _wild0 struct{}
    var _wild1 struct{}
    var _wild2 struct{}
    var t8 int32
    var t9 bool
    _ = _wild0
    _ = _wild1
    _ = _wild2
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            signal__1 = ref__Ref_int32(0)
            t5 = closure_env_main_0{
                signal_0: signal__1,
            }
            go _goml_inherent_closure_env_main_0_closure_env_main_0_apply(t5)
            pc = 2
        case 1:
            string_println("main")
            return struct{}{}
        case 2:
            t8 = ref_get__Ref_int32(signal__1)
            t9 = t8 < 1
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

func _goml_inherent_closure_env_main_0_closure_env_main_0_apply(env3 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x
    var t10 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            signal__1 = env3.signal_0
            t10 = child(signal__1)
            return t10
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
