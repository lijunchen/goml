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
    var inline231 int = 7
    var inline232 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: inline231,
    }
    var inline233 func(string) string = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(inline232, p0)
    }
    int_pairer__2 = inline233
    var string_pairer__3 func(string) string
    var inline227 string = "ok"
    var inline228 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: inline227,
    }
    var inline229 func(string) string = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(inline228, p0)
    }
    string_pairer__3 = inline229
    var t192 string = int_pairer__2("a")
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline224)
    var t193 string = string_pairer__3("b")
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env189 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var x__0 int = env189.x_0
    var t212 string = tag__1 + ":"
    var t213 string
    var inline236 string = _goml_runtime_core_int_to_string(x__0)
    t213 = inline236
    var t214 string = t212 + t213
    return t214
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env190 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var x__0 string = env190.x_0
    var t217 string = tag__1 + ":"
    var t218 string
    t218 = x__0
    var t219 string = t217 + t218
    return t219
}

func main() {
    main0()
}
