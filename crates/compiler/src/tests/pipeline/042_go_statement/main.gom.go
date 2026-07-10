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

func child(signal__0 *ref_int32_x) struct{} {
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(signal__0, 1)
    return struct{}{}
}

func main0() struct{} {
    var signal__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t7 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    go _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t7)
    Loop_loop9:
    for {
        var t10 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(signal__1)
        var t11 bool = t10 < 1
        if t11 {
            continue
        } else {
            break Loop_loop9
        }
    }
    println__T_string("main")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__95 *ref_int32_x, value__96 int32) struct{} {
    ref_set__Ref_5int32(self__95, value__96)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__93 int32) *ref_int32_x {
    var retv15 *ref_int32_x
    var t16 *ref_int32_x = ref__Ref_5int32(value__93)
    retv15 = t16
    return retv15
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__94 *ref_int32_x) int32 {
    var retv18 int32
    var t19 int32 = ref_get__Ref_5int32(self__94)
    retv18 = t19
    return retv18
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env3 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x = env3.signal_0
    child(signal__1)
    return struct{}{}
}

func main() {
    main0()
}
