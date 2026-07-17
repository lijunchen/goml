package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
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
    var t65 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    go _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t65)
    Loop_loop67:
    for {
        var t68 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(signal__1)
        var t69 bool = t68 < 1
        if t69 {
            continue
        } else {
            break Loop_loop67
        }
    }
    println__T_string("main")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv73 *ref_int32_x
    var t74 *ref_int32_x = ref__Ref_5int32(value__200)
    retv73 = t74
    return retv73
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv76 int32
    var t77 int32 = ref_get__Ref_5int32(self__201)
    retv76 = t77
    return retv76
}

func println__T_string(value__1 string) struct{} {
    var t79 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t79)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv82 string
    retv82 = self__34
    return retv82
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env61 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x = env61.signal_0
    child(signal__1)
    return struct{}{}
}

func main() {
    main0()
}
