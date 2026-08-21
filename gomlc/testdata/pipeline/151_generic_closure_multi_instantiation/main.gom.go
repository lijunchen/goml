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
    var inline455 int = 7
    var inline456 closure_env_make_pairer_T_int_0 = closure_env_make_pairer_T_int_0{
        x_0: inline455,
    }
    var inline457 func(string) string = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(inline456, p0)
    }
    int_pairer__2 = inline457
    var string_pairer__3 func(string) string
    var inline451 string = "ok"
    var inline452 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: inline451,
    }
    var inline453 func(string) string = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(inline452, p0)
    }
    string_pairer__3 = inline453
    var t416 string = int_pairer__2("a")
    var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t416)
    _goml_runtime_core_string_println(inline448)
    var t417 string = string_pairer__3("b")
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t417)
    _goml_runtime_core_string_println(inline445)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__en_h7caa8560de0ac6116c19c6efbb6b4ada__int__0_i_apply(env413 closure_env_make_pairer_T_int_0, tag__1 string) string {
    var x__0 int = env413.x_0
    var t436 string = tag__1 + ":"
    var t437 string
    var inline460 string = _goml_runtime_core_int_to_string(x__0)
    t437 = inline460
    var t438 string = t436 + t437
    return t438
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env414 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var x__0 string = env414.x_0
    var t441 string = tag__1 + ":"
    var t442 string
    t442 = x__0
    var t443 string = t441 + t442
    return t443
}

func main() {
    main0()
}
