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
    var retv30 int32
    var t31 int32 = x__0 * 2
    retv30 = t31
    return retv30
}

func increment(x__1 int32) int32 {
    var retv33 int32
    var t34 int32 = x__1 + 1
    retv33 = t34
    return retv33
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var retv36 int32
    var t37 int32 = f__2(value__3)
    retv36 = t37
    return retv36
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var retv39 int32
    var t40 int32 = g__5(value__6)
    var t41 int32 = f__4(t40)
    retv39 = t41
    return retv39
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
    var t43 string = _goml_m_inherent_i_int32_i_int32_i_to__string(composed__9)
    printer__20(t43)
    var t44 string = _goml_m_inherent_i_int32_i_int32_i_to__string(closure_result__12)
    printer__20(t44)
    var t45 string = _goml_m_inherent_i_int32_i_int32_i_to__string(invoked_with_global__16)
    printer__20(t45)
    var t46 string = _goml_m_inherent_i_int32_i_int32_i_to__string(composed_by_closure__19)
    printer__20(t46)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t48 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t48)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv51 string
    var t52 string = _goml_runtime_core_int32_to_string(self__2)
    retv51 = t52
    return retv51
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv54 string
    retv54 = self__9
    return retv54
}

func _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(env26 closure_env_closure_apply_0, value__10 int32) int32 {
    var retv56 int32
    var t57 int32 = apply_once(increment, value__10)
    retv56 = t57
    return retv56
}

func _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(env27 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var retv59 int32
    var t60 int32 = apply_once(func_to_call__13, value__14)
    retv59 = t60
    return retv59
}

func _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(env28 closure_env_composer_closure_2, value__17 int32) int32 {
    var retv62 int32
    var t63 int32 = compose(double, increment, value__17)
    retv62 = t63
    return retv62
}

func main() {
    main0()
}
