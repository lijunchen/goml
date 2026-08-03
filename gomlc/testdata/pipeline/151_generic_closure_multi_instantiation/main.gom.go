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
    var inline179 int = 7
    var inline181 func(string) string
    var inline182 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: inline179,
    }
    inline181 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(inline182, p0)
    }
    int_pairer__2 = inline181
    var string_pairer__3 func(string) string
    var inline174 string = "ok"
    var inline176 func(string) string
    var inline177 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: inline174,
    }
    inline176 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(inline177, p0)
    }
    string_pairer__3 = inline176
    var t141 string = int_pairer__2("a")
    var inline171 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t141)
    _goml_runtime_core_string_println(inline171)
    var t142 string = string_pairer__3("b")
    var inline168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t142)
    _goml_runtime_core_string_println(inline168)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env138 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var x__0 int = env138.x_0
    var t159 string = tag__1 + ":"
    var t160 string
    var inline185 string = _goml_runtime_core_int_to_string(x__0)
    t160 = inline185
    var t161 string = t159 + t160
    return t161
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env139 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var x__0 string = env139.x_0
    var t164 string = tag__1 + ":"
    var t165 string
    t165 = x__0
    var t166 string = t164 + t165
    return t166
}

func main() {
    main0()
}
