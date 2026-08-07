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
    var inline179 int32 = 0
    var inline180 *ref_int32_x = ref__Ref_5int32(inline179)
    signal__1 = inline180
    var t143 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    go _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t143)
    Loop_loop145:
    for {
        var t146 int32
        var inline173 int32 = ref_get__Ref_5int32(signal__1)
        t146 = inline173
        var t147 bool = t146 < 1
        if t147 {
            continue
        } else {
            break Loop_loop145
        }
    }
    var inline175 string = "main"
    var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline175)
    _goml_runtime_core_string_println(inline176)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__234 *ref_int32_x, value__235 int32) struct{} {
    ref_set__Ref_5int32(self__234, value__235)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env139 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x = env139.signal_0
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(signal__1, 1)
    return struct{}{}
}

func main() {
    main0()
}
