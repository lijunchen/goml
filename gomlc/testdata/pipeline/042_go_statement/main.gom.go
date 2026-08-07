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

func main0() struct{} {
    var signal__1 *ref_int32_x
    var inline215 int32 = 0
    var inline216 *ref_int32_x = ref__Ref_5int32(inline215)
    signal__1 = inline216
    var t179 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    go _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t179)
    Loop_loop181:
    for {
        var t182 int32
        var inline209 int32 = ref_get__Ref_5int32(signal__1)
        t182 = inline209
        var t183 bool = t182 < 1
        if t183 {
            continue
        } else {
            break Loop_loop181
        }
    }
    var inline211 string = "main"
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline211)
    _goml_runtime_core_string_println(inline212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__259 *ref_int32_x, value__260 int32) struct{} {
    ref_set__Ref_5int32(self__259, value__260)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env175 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x = env175.signal_0
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(signal__1, 1)
    return struct{}{}
}

func main() {
    main0()
}
