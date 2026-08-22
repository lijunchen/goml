package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint32 struct {
    items []uint32
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

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type closure_env_main_0 struct {
    signal_0 *ref_int32_x
}

type Ordering int32

func main0() struct{} {
    var signal__1 *ref_int32_x
    var inline840 int32 = 0
    var inline841 *ref_int32_x = ref__Ref_5int32(inline840)
    signal__1 = inline841
    var t803 closure_env_main_0 = closure_env_main_0{
        signal_0: signal__1,
    }
    var t804 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t803)
    }
    go t804()
    Loop_loop806:
    for {
        var t807 int32
        var inline834 int32 = ref_get__Ref_5int32(signal__1)
        t807 = inline834
        var t808 bool = t807 < 1
        if t808 {
            continue
        } else {
            break Loop_loop806
        }
    }
    var inline836 string = "main"
    var inline837 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline836)
    _goml_runtime_core_string_println(inline837)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(self__686 *ref_int32_x, value__687 int32) struct{} {
    ref_set__Ref_5int32(self__686, value__687)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env799 closure_env_main_0) struct{} {
    var signal__1 *ref_int32_x = env799.signal_0
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(signal__1, 1)
    return struct{}{}
}

func main() {
    main0()
}
