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

type Ordering int32

func main0() struct{} {
    var int_pairer__2 func(string) string
    var inline452 int = 7
    var inline453 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: inline452,
    }
    var inline454 func(string) string = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(inline453, p0)
    }
    int_pairer__2 = inline454
    var string_pairer__3 func(string) string
    var inline448 string = "ok"
    var inline449 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: inline448,
    }
    var inline450 func(string) string = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(inline449, p0)
    }
    string_pairer__3 = inline450
    var t413 string = int_pairer__2("a")
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t413)
    _goml_runtime_core_string_println(inline445)
    var t414 string = string_pairer__3("b")
    var inline442 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t414)
    _goml_runtime_core_string_println(inline442)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env410 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var x__0 int = env410.x_0
    var t433 string = tag__1 + ":"
    var t434 string
    var inline457 string = _goml_runtime_core_int_to_string(x__0)
    t434 = inline457
    var t435 string = t433 + t434
    return t435
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env411 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var x__0 string = env411.x_0
    var t438 string = tag__1 + ":"
    var t439 string
    t439 = x__0
    var t440 string = t438 + t439
    return t440
}

func main() {
    main0()
}
