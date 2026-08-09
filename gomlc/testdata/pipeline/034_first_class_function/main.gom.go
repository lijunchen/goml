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
    var t181 int32 = x__0 * 2
    return t181
}

func increment(x__1 int32) int32 {
    var t184 int32 = x__1 + 1
    return t184
}

func main0() struct{} {
    var first__8 int32
    var inline241 int32 = 4
    var inline242 int32 = double(inline241)
    first__8 = inline242
    var composed__9 int32
    var inline238 int32 = increment(first__8)
    var inline239 int32 = double(inline238)
    composed__9 = inline239
    var t193 closure_env_closure_apply_0 = closure_env_closure_apply_0{}
    var closure_apply__11 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(t193, p0)
    }
    var closure_result__12 int32 = closure_apply__11(composed__9)
    var t194 closure_env_global_invoker_1 = closure_env_global_invoker_1{}
    var global_invoker__15 func(func(int32) int32, int32) int32 = func(p0 func(int32) int32, p1 int32) int32 {
        return _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(t194, p0, p1)
    }
    var invoked_with_global__16 int32 = global_invoker__15(double, 3)
    var t195 closure_env_composer_closure_2 = closure_env_composer_closure_2{}
    var composer_closure__18 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(t195, p0)
    }
    var composed_by_closure__19 int32 = composer_closure__18(5)
    var t196 string
    var inline236 string = _goml_runtime_core_int32_to_string(composed__9)
    t196 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline233)
    var t197 string
    var inline231 string = _goml_runtime_core_int32_to_string(closure_result__12)
    t197 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline228)
    var t198 string
    var inline226 string = _goml_runtime_core_int32_to_string(invoked_with_global__16)
    t198 = inline226
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline223)
    var t199 string
    var inline221 string = _goml_runtime_core_int32_to_string(composed_by_closure__19)
    t199 = inline221
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline218)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(env176 closure_env_closure_apply_0, value__10 int32) int32 {
    var inline245 int32 = increment(value__10)
    return inline245
}

func _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(env177 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var inline247 int32 = func_to_call__13(value__14)
    return inline247
}

func _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(env178 closure_env_composer_closure_2, value__17 int32) int32 {
    var inline249 int32 = increment(value__17)
    var inline250 int32 = double(inline249)
    return inline250
}

func main() {
    main0()
}
