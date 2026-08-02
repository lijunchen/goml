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
    var t160 string = int_pairer__2("a")
    println__T_string(t160)
    var t161 string = string_pairer__3("b")
    println__T_string(t161)
    return struct{}{}
}

func make_pairer__T_int(x__0 int) func(string) string {
    var retv163 func(string) string
    var t164 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: x__0,
    }
    retv163 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(t164, p0)
    }
    return retv163
}

func make_pairer__T_string(x__0 string) func(string) string {
    var retv166 func(string) string
    var t167 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: x__0,
    }
    retv166 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(t167, p0)
    }
    return retv166
}

func println__T_string(value__1 string) struct{} {
    var t169 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t169)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t173 string = _goml_runtime_core_int_to_string(self__40)
    return t173
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env157 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var x__0 int = env157.x_0
    var t178 string = tag__1 + ":"
    var t179 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x__0)
    var t180 string = t178 + t179
    return t180
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env158 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var x__0 string = env158.x_0
    var t183 string = tag__1 + ":"
    var t184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x__0)
    var t185 string = t183 + t184
    return t185
}

func main() {
    main0()
}
