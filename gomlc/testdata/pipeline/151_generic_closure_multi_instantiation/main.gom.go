package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_make_pairer_T_int_0 struct {
    x_0 int
}

type closure_env_make_pairer_T_string_1 struct {
    x_0 string
}

func main0() struct{} {
    var int_pairer__2 func(string) string
    var inline215 int = 7
    var inline217 func(string) string
    var inline218 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: inline215,
    }
    inline217 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(inline218, p0)
    }
    int_pairer__2 = inline217
    var string_pairer__3 func(string) string
    var inline210 string = "ok"
    var inline212 func(string) string
    var inline213 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: inline210,
    }
    inline212 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(inline213, p0)
    }
    string_pairer__3 = inline212
    var t177 string = int_pairer__2("a")
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline207)
    var t178 string = string_pairer__3("b")
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env174 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var x__0 int = env174.x_0
    var t195 string = tag__1 + ":"
    var t196 string
    var inline221 string = _goml_runtime_core_int_to_string(x__0)
    t196 = inline221
    var t197 string = t195 + t196
    return t197
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env175 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var x__0 string = env175.x_0
    var t200 string = tag__1 + ":"
    var t201 string
    t201 = x__0
    var t202 string = t200 + t201
    return t202
}

func main() {
    main0()
}
