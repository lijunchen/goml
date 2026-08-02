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
    var t164 int32 = x__0 * 2
    return t164
}

func increment(x__1 int32) int32 {
    var t167 int32 = x__1 + 1
    return t167
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var t170 int32 = f__2(value__3)
    return t170
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var t173 int32 = g__5(value__6)
    var t174 int32 = f__4(t173)
    return t174
}

func main0() struct{} {
    var first__8 int32
    var inline229 int32 = 4
    var inline230 int32 = double(inline229)
    first__8 = inline230
    var composed__9 int32
    var inline226 int32 = increment(first__8)
    var inline227 int32 = double(inline226)
    composed__9 = inline227
    var closure_result__12 int32
    var inline224 int32 = apply_once(increment, composed__9)
    closure_result__12 = inline224
    var invoked_with_global__16 int32
    var inline221 int32 = 3
    var inline222 int32 = apply_once(double, inline221)
    invoked_with_global__16 = inline222
    var composed_by_closure__19 int32
    var inline218 int32 = 5
    var inline219 int32 = compose(double, increment, inline218)
    composed_by_closure__19 = inline219
    var t176 string
    var inline216 string = _goml_runtime_core_int32_to_string(composed__9)
    t176 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
    _goml_runtime_core_string_println(inline213)
    var t177 string
    var inline211 string = _goml_runtime_core_int32_to_string(closure_result__12)
    t177 = inline211
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline208)
    var t178 string
    var inline206 string = _goml_runtime_core_int32_to_string(invoked_with_global__16)
    t178 = inline206
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
    _goml_runtime_core_string_println(inline203)
    var t179 string
    var inline201 string = _goml_runtime_core_int32_to_string(composed_by_closure__19)
    t179 = inline201
    var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
    _goml_runtime_core_string_println(inline198)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
