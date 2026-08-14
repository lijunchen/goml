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
    var inline226 int = 7
    var inline227 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: inline226,
    }
    var inline228 func(string) string = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(inline227, p0)
    }
    int_pairer__2 = inline228
    var string_pairer__3 func(string) string
    var inline222 string = "ok"
    var inline223 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: inline222,
    }
    var inline224 func(string) string = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(inline223, p0)
    }
    string_pairer__3 = inline224
    var t187 string = int_pairer__2("a")
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline219)
    var t188 string = string_pairer__3("b")
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline216)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env184 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var x__0 int = env184.x_0
    var t207 string = tag__1 + ":"
    var t208 string
    var inline231 string = _goml_runtime_core_int_to_string(x__0)
    t208 = inline231
    var t209 string = t207 + t208
    return t209
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env185 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var x__0 string = env185.x_0
    var t212 string = tag__1 + ":"
    var t213 string
    t213 = x__0
    var t214 string = t212 + t213
    return t214
}

func main() {
    main0()
}
