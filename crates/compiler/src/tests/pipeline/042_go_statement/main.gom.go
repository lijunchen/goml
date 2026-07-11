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
    var t29 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    go _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t29)
    Loop_loop31:
    for {
        var t32 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(signal__1)
        var t33 bool = t32 < 1
        if t33 {
            continue
        } else {
            break Loop_loop31
        }
    }
    println__T_string("main")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv37 *ref_int32_x
    var t38 *ref_int32_x = ref__Ref_5int32(value__137)
    retv37 = t38
    return retv37
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv40 int32
    var t41 int32 = ref_get__Ref_5int32(self__138)
    retv40 = t41
    return retv40
}

func println__T_string(value__1 string) struct{} {
    var t43 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t43)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv46 string
    retv46 = self__9
    return retv46
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env25 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x = env25.signal_0
    child(signal__1)
    return struct{}{}
}

func main() {
    main0()
}
