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
    var t69 string = int_pairer__2("a")
    println__T_string(t69)
    var t70 string = string_pairer__3("b")
    println__T_string(t70)
    return struct{}{}
}

func make_pairer__T_int(x__0 int) func(string) string {
    var retv72 func(string) string
    var t73 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: x__0,
    }
    retv72 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(t73, p0)
    }
    return retv72
}

func make_pairer__T_string(x__0 string) func(string) string {
    var retv75 func(string) string
    var t76 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: x__0,
    }
    retv75 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(t76, p0)
    }
    return retv75
}

func println__T_string(value__1 string) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t78)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int_to_string(self__40)
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv84 string
    retv84 = self__38
    return retv84
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env66 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var retv86 string
    var x__0 int = env66.x_0
    var t87 string = tag__1 + ":"
    var t88 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x__0)
    var t89 string = t87 + t88
    retv86 = t89
    return retv86
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env67 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var retv91 string
    var x__0 string = env67.x_0
    var t92 string = tag__1 + ":"
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x__0)
    var t94 string = t92 + t93
    retv91 = t94
    return retv91
}

func main() {
    main0()
}
