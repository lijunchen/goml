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

func double(x__0 int32) int32 {
    var t191 int32 = x__0 * 2
    return t191
}

func increment(x__1 int32) int32 {
    var t194 int32 = x__1 + 1
    return t194
}

func main0() struct{} {
    var first__8 int32
    var inline251 int32 = 4
    var inline252 int32 = double(inline251)
    first__8 = inline252
    var composed__9 int32
    var inline248 int32 = increment(first__8)
    var inline249 int32 = double(inline248)
    composed__9 = inline249
    var t203 closure_env_closure_apply_0 = closure_env_closure_apply_0{}
    var closure_apply__11 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(t203, p0)
    }
    var closure_result__12 int32 = closure_apply__11(composed__9)
    var t204 closure_env_global_invoker_1 = closure_env_global_invoker_1{}
    var global_invoker__15 func(func(int32) int32, int32) int32 = func(p0 func(int32) int32, p1 int32) int32 {
        return _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(t204, p0, p1)
    }
    var invoked_with_global__16 int32 = global_invoker__15(double, 3)
    var t205 closure_env_composer_closure_2 = closure_env_composer_closure_2{}
    var composer_closure__18 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(t205, p0)
    }
    var composed_by_closure__19 int32 = composer_closure__18(5)
    var t206 string
    var inline246 string = _goml_runtime_core_int32_to_string(composed__9)
    t206 = inline246
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline243)
    var t207 string
    var inline241 string = _goml_runtime_core_int32_to_string(closure_result__12)
    t207 = inline241
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline238)
    var t208 string
    var inline236 string = _goml_runtime_core_int32_to_string(invoked_with_global__16)
    t208 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline233)
    var t209 string
    var inline231 string = _goml_runtime_core_int32_to_string(composed_by_closure__19)
    t209 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline228)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(env186 closure_env_closure_apply_0, value__10 int32) int32 {
    var inline255 int32 = increment(value__10)
    return inline255
}

func _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(env187 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var inline257 int32 = func_to_call__13(value__14)
    return inline257
}

func _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(env188 closure_env_composer_closure_2, value__17 int32) int32 {
    var inline259 int32 = increment(value__17)
    var inline260 int32 = double(inline259)
    return inline260
}

func main() {
    main0()
}
