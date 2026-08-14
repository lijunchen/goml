package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_closure_apply_0 struct {}

type closure_env_global_invoker_1 struct {}

type closure_env_composer_closure_2 struct {}

type Ordering int32

func double(x__0 int32) int32 {
    var t417 int32 = x__0 * 2
    return t417
}

func increment(x__1 int32) int32 {
    var t420 int32 = x__1 + 1
    return t420
}

func main0() struct{} {
    var first__8 int32
    var inline477 int32 = 4
    var inline478 int32 = double(inline477)
    first__8 = inline478
    var composed__9 int32
    var inline474 int32 = increment(first__8)
    var inline475 int32 = double(inline474)
    composed__9 = inline475
    var t429 closure_env_closure_apply_0 = closure_env_closure_apply_0{}
    var closure_apply__11 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(t429, p0)
    }
    var closure_result__12 int32 = closure_apply__11(composed__9)
    var t430 closure_env_global_invoker_1 = closure_env_global_invoker_1{}
    var global_invoker__15 func(func(int32) int32, int32) int32 = func(p0 func(int32) int32, p1 int32) int32 {
        return _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(t430, p0, p1)
    }
    var invoked_with_global__16 int32 = global_invoker__15(double, 3)
    var t431 closure_env_composer_closure_2 = closure_env_composer_closure_2{}
    var composer_closure__18 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(t431, p0)
    }
    var composed_by_closure__19 int32 = composer_closure__18(5)
    var t432 string
    var inline472 string = _goml_runtime_core_int32_to_string(composed__9)
    t432 = inline472
    var inline469 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline469)
    var t433 string
    var inline467 string = _goml_runtime_core_int32_to_string(closure_result__12)
    t433 = inline467
    var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
    _goml_runtime_core_string_println(inline464)
    var t434 string
    var inline462 string = _goml_runtime_core_int32_to_string(invoked_with_global__16)
    t434 = inline462
    var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline459)
    var t435 string
    var inline457 string = _goml_runtime_core_int32_to_string(composed_by_closure__19)
    t435 = inline457
    var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline454)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(env412 closure_env_closure_apply_0, value__10 int32) int32 {
    var inline481 int32 = increment(value__10)
    return inline481
}

func _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(env413 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var inline483 int32 = func_to_call__13(value__14)
    return inline483
}

func _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(env414 closure_env_composer_closure_2, value__17 int32) int32 {
    var inline485 int32 = increment(value__17)
    var inline486 int32 = double(inline485)
    return inline486
}

func main() {
    main0()
}
