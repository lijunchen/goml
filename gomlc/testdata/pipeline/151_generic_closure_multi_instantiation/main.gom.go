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
    var inline198 int = 7
    var inline200 func(string) string
    var inline201 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: inline198,
    }
    inline200 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(inline201, p0)
    }
    int_pairer__2 = inline200
    var string_pairer__3 func(string) string
    var inline193 string = "ok"
    var inline195 func(string) string
    var inline196 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: inline193,
    }
    inline195 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(inline196, p0)
    }
    string_pairer__3 = inline195
    var t160 string = int_pairer__2("a")
    var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline190)
    var t161 string = string_pairer__3("b")
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t161)
    _goml_runtime_core_string_println(inline187)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env157 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var x__0 int = env157.x_0
    var t178 string = tag__1 + ":"
    var t179 string
    var inline204 string = _goml_runtime_core_int_to_string(x__0)
    t179 = inline204
    var t180 string = t178 + t179
    return t180
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env158 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var x__0 string = env158.x_0
    var t183 string = tag__1 + ":"
    var t184 string
    t184 = x__0
    var t185 string = t183 + t184
    return t185
}

func main() {
    main0()
}
