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
    var retv116 int32
    var t117 int32 = x__0 * 2
    retv116 = t117
    return retv116
}

func increment(x__1 int32) int32 {
    var retv119 int32
    var t120 int32 = x__1 + 1
    retv119 = t120
    return retv119
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var retv122 int32
    var t123 int32 = f__2(value__3)
    retv122 = t123
    return retv122
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var retv125 int32
    var t126 int32 = g__5(value__6)
    var t127 int32 = f__4(t126)
    retv125 = t127
    return retv125
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
    var t129 string = _goml_m_inherent_i_int32_i_int32_i_to__string(composed__9)
    printer__20(t129)
    var t130 string = _goml_m_inherent_i_int32_i_int32_i_to__string(closure_result__12)
    printer__20(t130)
    var t131 string = _goml_m_inherent_i_int32_i_int32_i_to__string(invoked_with_global__16)
    printer__20(t131)
    var t132 string = _goml_m_inherent_i_int32_i_int32_i_to__string(composed_by_closure__19)
    printer__20(t132)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t134 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t134)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv137 string
    var t138 string = _goml_runtime_core_int32_to_string(self__6)
    retv137 = t138
    return retv137
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv140 string
    retv140 = self__38
    return retv140
}

func _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(env112 closure_env_closure_apply_0, value__10 int32) int32 {
    var retv142 int32
    var t143 int32 = apply_once(increment, value__10)
    retv142 = t143
    return retv142
}

func _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(env113 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var retv145 int32
    var t146 int32 = apply_once(func_to_call__13, value__14)
    retv145 = t146
    return retv145
}

func _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(env114 closure_env_composer_closure_2, value__17 int32) int32 {
    var retv148 int32
    var t149 int32 = compose(double, increment, value__17)
    retv148 = t149
    return retv148
}

func main() {
    main0()
}
