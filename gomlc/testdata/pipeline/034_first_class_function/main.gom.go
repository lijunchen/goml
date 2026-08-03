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
    var t186 int32 = x__0 * 2
    return t186
}

func increment(x__1 int32) int32 {
    var t189 int32 = x__1 + 1
    return t189
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var t192 int32 = f__2(value__3)
    return t192
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var t195 int32 = g__5(value__6)
    var t196 int32 = f__4(t195)
    return t196
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
    var closure_result__12 int32
    var inline246 int32 = apply_once(increment, composed__9)
    closure_result__12 = inline246
    var invoked_with_global__16 int32
    var inline243 int32 = 3
    var inline244 int32 = apply_once(double, inline243)
    invoked_with_global__16 = inline244
    var composed_by_closure__19 int32
    var inline240 int32 = 5
    var inline241 int32 = compose(double, increment, inline240)
    composed_by_closure__19 = inline241
    var t198 string
    var inline238 string = _goml_runtime_core_int32_to_string(composed__9)
    t198 = inline238
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline235)
    var t199 string
    var inline233 string = _goml_runtime_core_int32_to_string(closure_result__12)
    t199 = inline233
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline230)
    var t200 string
    var inline228 string = _goml_runtime_core_int32_to_string(invoked_with_global__16)
    t200 = inline228
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline225)
    var t201 string
    var inline223 string = _goml_runtime_core_int32_to_string(composed_by_closure__19)
    t201 = inline223
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline220)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
