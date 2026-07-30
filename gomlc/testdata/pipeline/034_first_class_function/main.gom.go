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
    var retv76 int32
    var t77 int32 = x__0 * 2
    retv76 = t77
    return retv76
}

func increment(x__1 int32) int32 {
    var retv79 int32
    var t80 int32 = x__1 + 1
    retv79 = t80
    return retv79
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var retv82 int32
    var t83 int32 = f__2(value__3)
    retv82 = t83
    return retv82
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var retv85 int32
    var t86 int32 = g__5(value__6)
    var t87 int32 = f__4(t86)
    retv85 = t87
    return retv85
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
    var t89 string = _goml_m_inherent_i_int32_i_int32_i_to__string(composed__9)
    printer__20(t89)
    var t90 string = _goml_m_inherent_i_int32_i_int32_i_to__string(closure_result__12)
    printer__20(t90)
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(invoked_with_global__16)
    printer__20(t91)
    var t92 string = _goml_m_inherent_i_int32_i_int32_i_to__string(composed_by_closure__19)
    printer__20(t92)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t94 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t94)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv97 string
    var t98 string = _goml_runtime_core_int32_to_string(self__6)
    retv97 = t98
    return retv97
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv100 string
    retv100 = self__38
    return retv100
}

func _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(env72 closure_env_closure_apply_0, value__10 int32) int32 {
    var retv102 int32
    var t103 int32 = apply_once(increment, value__10)
    retv102 = t103
    return retv102
}

func _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(env73 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var retv105 int32
    var t106 int32 = apply_once(func_to_call__13, value__14)
    retv105 = t106
    return retv105
}

func _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(env74 closure_env_composer_closure_2, value__17 int32) int32 {
    var retv108 int32
    var t109 int32 = compose(double, increment, value__17)
    retv108 = t109
    return retv108
}

func main() {
    main0()
}
