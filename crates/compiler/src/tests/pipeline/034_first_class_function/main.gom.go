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
    var retv66 int32
    var t67 int32 = x__0 * 2
    retv66 = t67
    return retv66
}

func increment(x__1 int32) int32 {
    var retv69 int32
    var t70 int32 = x__1 + 1
    retv69 = t70
    return retv69
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var retv72 int32
    var t73 int32 = f__2(value__3)
    retv72 = t73
    return retv72
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var retv75 int32
    var t76 int32 = g__5(value__6)
    var t77 int32 = f__4(t76)
    retv75 = t77
    return retv75
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
    var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(composed__9)
    printer__20(t79)
    var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(closure_result__12)
    printer__20(t80)
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(invoked_with_global__16)
    printer__20(t81)
    var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(composed_by_closure__19)
    printer__20(t82)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int32_to_string(self__2)
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv90 string
    retv90 = self__34
    return retv90
}

func _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(env62 closure_env_closure_apply_0, value__10 int32) int32 {
    var retv92 int32
    var t93 int32 = apply_once(increment, value__10)
    retv92 = t93
    return retv92
}

func _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(env63 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var retv95 int32
    var t96 int32 = apply_once(func_to_call__13, value__14)
    retv95 = t96
    return retv95
}

func _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(env64 closure_env_composer_closure_2, value__17 int32) int32 {
    var retv98 int32
    var t99 int32 = compose(double, increment, value__17)
    retv98 = t99
    return retv98
}

func main() {
    main0()
}
