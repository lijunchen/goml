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

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var t187 int32 = f__2(value__3)
    return t187
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var t190 int32 = g__5(value__6)
    var t191 int32 = f__4(t190)
    return t191
}

func main0() struct{} {
    var first__8 int32
    var inline246 int32 = 4
    var inline247 int32 = double(inline246)
    first__8 = inline247
    var composed__9 int32
    var inline243 int32 = increment(first__8)
    var inline244 int32 = double(inline243)
    composed__9 = inline244
    var closure_result__12 int32
    var inline241 int32 = apply_once(increment, composed__9)
    closure_result__12 = inline241
    var invoked_with_global__16 int32
    var inline238 int32 = 3
    var inline239 int32 = apply_once(double, inline238)
    invoked_with_global__16 = inline239
    var composed_by_closure__19 int32
    var inline235 int32 = 5
    var inline236 int32 = compose(double, increment, inline235)
    composed_by_closure__19 = inline236
    var t193 string
    var inline233 string = _goml_runtime_core_int32_to_string(composed__9)
    t193 = inline233
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline230)
    var t194 string
    var inline228 string = _goml_runtime_core_int32_to_string(closure_result__12)
    t194 = inline228
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline225)
    var t195 string
    var inline223 string = _goml_runtime_core_int32_to_string(invoked_with_global__16)
    t195 = inline223
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline220)
    var t196 string
    var inline218 string = _goml_runtime_core_int32_to_string(composed_by_closure__19)
    t196 = inline218
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline215)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
