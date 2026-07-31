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
    var int_pairer__2 func(string) string = make_pairer__T_int(7)
    var string_pairer__3 func(string) string = make_pairer__T_string("ok")
    var t157 string = int_pairer__2("a")
    println__T_string(t157)
    var t158 string = string_pairer__3("b")
    println__T_string(t158)
    return struct{}{}
}

func make_pairer__T_int(x__0 int) func(string) string {
    var retv160 func(string) string
    var t161 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: x__0,
    }
    retv160 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(t161, p0)
    }
    return retv160
}

func make_pairer__T_string(x__0 string) func(string) string {
    var retv163 func(string) string
    var t164 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: x__0,
    }
    retv163 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(t164, p0)
    }
    return retv163
}

func println__T_string(value__1 string) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv169 string
    var t170 string = _goml_runtime_core_int_to_string(self__40)
    retv169 = t170
    return retv169
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv172 string
    retv172 = self__38
    return retv172
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env154 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var retv174 string
    var x__0 int = env154.x_0
    var t175 string = tag__1 + ":"
    var t176 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x__0)
    var t177 string = t175 + t176
    retv174 = t177
    return retv174
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env155 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var retv179 string
    var x__0 string = env155.x_0
    var t180 string = tag__1 + ":"
    var t181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x__0)
    var t182 string = t180 + t181
    retv179 = t182
    return retv179
}

func main() {
    main0()
}
