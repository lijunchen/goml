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

type Ordering int32

func main0() struct{} {
    var signal__1 *ref_int32_x
    var inline452 int32 = 0
    var inline453 *ref_int32_x = ref__Ref_5int32(inline452)
    signal__1 = inline453
    var t415 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    var t416 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t415)
    }
    go t416()
    Loop_loop418:
    for {
        var t419 int32
        var inline446 int32 = ref_get__Ref_5int32(signal__1)
        t419 = inline446
        var t420 bool = t419 < 1
        if t420 {
            continue
        } else {
            break Loop_loop418
        }
    }
    var inline448 string = "main"
    var inline449 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline448)
    _goml_runtime_core_string_println(inline449)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env411 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x = env411.signal_0
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(signal__1, 1)
    return struct{}{}
}

func main() {
    main0()
}
