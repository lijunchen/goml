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
    var t73 string = int_pairer__2("a")
    println__T_string(t73)
    var t74 string = string_pairer__3("b")
    println__T_string(t74)
    return struct{}{}
}

func make_pairer__T_int(x__0 int) func(string) string {
    var retv76 func(string) string
    var t77 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: x__0,
    }
    retv76 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(t77, p0)
    }
    return retv76
}

func make_pairer__T_string(x__0 string) func(string) string {
    var retv79 func(string) string
    var t80 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: x__0,
    }
    retv79 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(t80, p0)
    }
    return retv79
}

func println__T_string(value__1 string) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv85 string
    var t86 string = _goml_runtime_core_int_to_string(self__40)
    retv85 = t86
    return retv85
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv88 string
    retv88 = self__38
    return retv88
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env70 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var retv90 string
    var x__0 int = env70.x_0
    var t91 string = tag__1 + ":"
    var t92 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x__0)
    var t93 string = t91 + t92
    retv90 = t93
    return retv90
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env71 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var retv95 string
    var x__0 string = env71.x_0
    var t96 string = tag__1 + ":"
    var t97 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x__0)
    var t98 string = t96 + t97
    retv95 = t98
    return retv95
}

func main() {
    main0()
}
