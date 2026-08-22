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
    var inline455 int32 = 0
    var inline456 *ref_int32_x = ref__Ref_5int32(inline455)
    signal__1 = inline456
    var t418 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    var t419 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t418)
    }
    go t419()
    Loop_loop421:
    for {
        var t422 int32
        var inline449 int32 = ref_get__Ref_5int32(signal__1)
        t422 = inline449
        var t423 bool = t422 < 1
        if t423 {
            continue
        } else {
            break Loop_loop421
        }
    }
    var inline451 string = "main"
    var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline451)
    _goml_runtime_core_string_println(inline452)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env414 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x = env414.signal_0
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(signal__1, 1)
    return struct{}{}
}

func main() {
    main0()
}
