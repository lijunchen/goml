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
    var t113 string = int_pairer__2("a")
    println__T_string(t113)
    var t114 string = string_pairer__3("b")
    println__T_string(t114)
    return struct{}{}
}

func make_pairer__T_int(x__0 int) func(string) string {
    var retv116 func(string) string
    var t117 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: x__0,
    }
    retv116 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(t117, p0)
    }
    return retv116
}

func make_pairer__T_string(x__0 string) func(string) string {
    var retv119 func(string) string
    var t120 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: x__0,
    }
    retv119 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(t120, p0)
    }
    return retv119
}

func println__T_string(value__1 string) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t122)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv125 string
    var t126 string = _goml_runtime_core_int_to_string(self__40)
    retv125 = t126
    return retv125
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv128 string
    retv128 = self__38
    return retv128
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env110 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var retv130 string
    var x__0 int = env110.x_0
    var t131 string = tag__1 + ":"
    var t132 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x__0)
    var t133 string = t131 + t132
    retv130 = t133
    return retv130
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env111 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var retv135 string
    var x__0 string = env111.x_0
    var t136 string = tag__1 + ":"
    var t137 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x__0)
    var t138 string = t136 + t137
    retv135 = t138
    return retv135
}

func main() {
    main0()
}
