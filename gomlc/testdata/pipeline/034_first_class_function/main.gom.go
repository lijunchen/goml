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
    var t420 int32 = x__0 * 2
    return t420
}

func increment(x__1 int32) int32 {
    var t423 int32 = x__1 + 1
    return t423
}

func main0() struct{} {
    var first__8 int32
    var inline480 int32 = 4
    var inline481 int32 = double(inline480)
    first__8 = inline481
    var composed__9 int32
    var inline477 int32 = increment(first__8)
    var inline478 int32 = double(inline477)
    composed__9 = inline478
    var t432 closure_env_closure_apply_0 = closure_env_closure_apply_0{}
    var closure_apply__11 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(t432, p0)
    }
    var closure_result__12 int32 = closure_apply__11(composed__9)
    var t433 closure_env_global_invoker_1 = closure_env_global_invoker_1{}
    var global_invoker__15 func(func(int32) int32, int32) int32 = func(p0 func(int32) int32, p1 int32) int32 {
        return _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(t433, p0, p1)
    }
    var invoked_with_global__16 int32 = global_invoker__15(double, 3)
    var t434 closure_env_composer_closure_2 = closure_env_composer_closure_2{}
    var composer_closure__18 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(t434, p0)
    }
    var composed_by_closure__19 int32 = composer_closure__18(5)
    var t435 string
    var inline475 string = _goml_runtime_core_int32_to_string(composed__9)
    t435 = inline475
    var inline472 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline472)
    var t436 string
    var inline470 string = _goml_runtime_core_int32_to_string(closure_result__12)
    t436 = inline470
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline467)
    var t437 string
    var inline465 string = _goml_runtime_core_int32_to_string(invoked_with_global__16)
    t437 = inline465
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline462)
    var t438 string
    var inline460 string = _goml_runtime_core_int32_to_string(composed_by_closure__19)
    t438 = inline460
    var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline457)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(env415 closure_env_closure_apply_0, value__10 int32) int32 {
    var inline484 int32 = increment(value__10)
    return inline484
}

func _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(env416 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var inline486 int32 = func_to_call__13(value__14)
    return inline486
}

func _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(env417 closure_env_composer_closure_2, value__17 int32) int32 {
    var inline488 int32 = increment(value__17)
    var inline489 int32 = double(inline488)
    return inline489
}

func main() {
    main0()
}
