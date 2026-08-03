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
    var t145 int32 = x__0 * 2
    return t145
}

func increment(x__1 int32) int32 {
    var t148 int32 = x__1 + 1
    return t148
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var t151 int32 = f__2(value__3)
    return t151
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var t154 int32 = g__5(value__6)
    var t155 int32 = f__4(t154)
    return t155
}

func main0() struct{} {
    var first__8 int32
    var inline210 int32 = 4
    var inline211 int32 = double(inline210)
    first__8 = inline211
    var composed__9 int32
    var inline207 int32 = increment(first__8)
    var inline208 int32 = double(inline207)
    composed__9 = inline208
    var closure_result__12 int32
    var inline205 int32 = apply_once(increment, composed__9)
    closure_result__12 = inline205
    var invoked_with_global__16 int32
    var inline202 int32 = 3
    var inline203 int32 = apply_once(double, inline202)
    invoked_with_global__16 = inline203
    var composed_by_closure__19 int32
    var inline199 int32 = 5
    var inline200 int32 = compose(double, increment, inline199)
    composed_by_closure__19 = inline200
    var t157 string
    var inline197 string = _goml_runtime_core_int32_to_string(composed__9)
    t157 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t157)
    _goml_runtime_core_string_println(inline194)
    var t158 string
    var inline192 string = _goml_runtime_core_int32_to_string(closure_result__12)
    t158 = inline192
    var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t158)
    _goml_runtime_core_string_println(inline189)
    var t159 string
    var inline187 string = _goml_runtime_core_int32_to_string(invoked_with_global__16)
    t159 = inline187
    var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
    _goml_runtime_core_string_println(inline184)
    var t160 string
    var inline182 string = _goml_runtime_core_int32_to_string(composed_by_closure__19)
    t160 = inline182
    var inline179 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline179)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
