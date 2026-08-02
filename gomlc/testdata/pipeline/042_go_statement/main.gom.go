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
    var inline198 int32 = 0
    var inline199 *ref_int32_x = ref__Ref_5int32(inline198)
    signal__1 = inline199
    var t162 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    go _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t162)
    Loop_loop164:
    for {
        var t165 int32
        var inline192 int32 = ref_get__Ref_5int32(signal__1)
        t165 = inline192
        var t166 bool = t165 < 1
        if t166 {
            continue
        } else {
            break Loop_loop164
        }
    }
    var inline194 string = "main"
    var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline194)
    _goml_runtime_core_string_println(inline195)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env158 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x = env158.signal_0
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(signal__1, 1)
    return struct{}{}
}

func main() {
    main0()
}
