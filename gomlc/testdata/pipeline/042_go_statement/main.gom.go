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
    var inline226 int32 = 0
    var inline227 *ref_int32_x = ref__Ref_5int32(inline226)
    signal__1 = inline227
    var t189 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    var t190 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t189)
    }
    go t190()
    Loop_loop192:
    for {
        var t193 int32
        var inline220 int32 = ref_get__Ref_5int32(signal__1)
        t193 = inline220
        var t194 bool = t193 < 1
        if t194 {
            continue
        } else {
            break Loop_loop192
        }
    }
    var inline222 string = "main"
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline222)
    _goml_runtime_core_string_println(inline223)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__272 *ref_int32_x, value__273 int32) struct{} {
    ref_set__Ref_5int32(self__272, value__273)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env185 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x = env185.signal_0
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(signal__1, 1)
    return struct{}{}
}

func main() {
    main0()
}
