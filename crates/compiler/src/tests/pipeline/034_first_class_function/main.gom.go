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
    var retv15 int32
    var t16 int32 = x__0 * 2
    retv15 = t16
    return retv15
}

func increment(x__1 int32) int32 {
    var retv18 int32
    var t19 int32 = x__1 + 1
    retv18 = t19
    return retv18
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var retv21 int32
    var t22 int32 = f__2(value__3)
    retv21 = t22
    return retv21
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var retv24 int32
    var t25 int32 = g__5(value__6)
    var t26 int32 = f__4(t25)
    retv24 = t26
    return retv24
}

func main0() struct{} {
    var local__7 func(int32) int32 = double
    var first__8 int32 = apply_once(local__7, 4)
    var composed__9 int32 = compose(double, increment, first__8)
    var closure_apply__11 closure_env_closure_apply_0 = closure_env_closure_apply_0{}
    var closure_result__12 int32 = _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(closure_apply__11, composed__9)
    var global_invoker__15 closure_env_global_invoker_1 = closure_env_global_invoker_1{}
    var invoked_with_global__16 int32 = _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(global_invoker__15, double, 3)
    var composer_closure__18 closure_env_composer_closure_2 = closure_env_composer_closure_2{}
    var composed_by_closure__19 int32 = _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(composer_closure__18, 5)
    var printer__20 func(string) struct{} = println__T_string
    var t28 string = _goml_m_inherent_i_int32_i_int32_i_to__string(composed__9)
    printer__20(t28)
    var t29 string = _goml_m_inherent_i_int32_i_int32_i_to__string(closure_result__12)
    printer__20(t29)
    var t30 string = _goml_m_inherent_i_int32_i_int32_i_to__string(invoked_with_global__16)
    printer__20(t30)
    var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(composed_by_closure__19)
    printer__20(t31)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv36 string
    var t37 string = _goml_runtime_core_int32_to_string(self__2)
    retv36 = t37
    return retv36
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(env11 closure_env_closure_apply_0, value__10 int32) int32 {
    var retv41 int32
    var t42 int32 = apply_once(increment, value__10)
    retv41 = t42
    return retv41
}

func _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(env12 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var retv44 int32
    var t45 int32 = apply_once(func_to_call__13, value__14)
    retv44 = t45
    return retv44
}

func _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(env13 closure_env_composer_closure_2, value__17 int32) int32 {
    var retv47 int32
    var t48 int32 = compose(double, increment, value__17)
    retv47 = t48
    return retv47
}

func main() {
    main0()
}
