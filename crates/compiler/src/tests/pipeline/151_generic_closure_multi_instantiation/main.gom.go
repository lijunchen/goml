package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_make_pairer_T_int32_0 struct {
    x_0 int32
}

type closure_env_make_pairer_T_string_1 struct {
    x_0 string
}

func main0() struct{} {
    var int_pairer__2 func(string) string = make_pairer__T_int32(7)
    var string_pairer__3 func(string) string = make_pairer__T_string("ok")
    var t5 string = int_pairer__2("a")
    println__T_string(t5)
    var t6 string = string_pairer__3("b")
    println__T_string(t6)
    return struct{}{}
}

func make_pairer__T_int32(x__0 int32) func(string) string {
    var retv8 func(string) string
    var t9 closure_env_make_pairer_T_int32_0 = closure_env_make_pairer_T_int32_0{
        x_0: x__0,
    }
    retv8 = func(p0 string) string {
        return _goml_inherent_x23_closure_x5f_env_x5f_make_x5f_pairer_x5f_T_x5f_int32_x5f_0_x23_closure_x5f_env_x5f_make_x5f_pairer_x5f_T_x5f_int32_x5f_0_x23_apply(t9, p0)
    }
    return retv8
}

func make_pairer__T_string(x__0 string) func(string) string {
    var retv11 func(string) string
    var t12 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: x__0,
    }
    retv11 = func(p0 string) string {
        return _goml_inherent_x23_closure_x5f_env_x5f_make_x5f_pairer_x5f_T_x5f_string_x5f_1_x23_closure_x5f_env_x5f_make_x5f_pairer_x5f_T_x5f_string_x5f_1_x23_apply(t12, p0)
    }
    return retv11
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_make_x5f_pairer_x5f_T_x5f_int32_x5f_0_x23_closure_x5f_env_x5f_make_x5f_pairer_x5f_T_x5f_int32_x5f_0_x23_apply(env2 closure_env_make_pairer_T_int32_0, tag__1 string) string {
    var retv16 string
    var x__0 int32 = env2.x_0
    var t17 string = tag__1 + ":"
    var t18 string = int32_to_string(x__0)
    var t19 string = t17 + t18
    retv16 = t19
    return retv16
}

func _goml_inherent_x23_closure_x5f_env_x5f_make_x5f_pairer_x5f_T_x5f_string_x5f_1_x23_closure_x5f_env_x5f_make_x5f_pairer_x5f_T_x5f_string_x5f_1_x23_apply(env3 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var retv21 string
    var x__0 string = env3.x_0
    var t22 string = tag__1 + ":"
    var t23 string = t22 + x__0
    retv21 = t23
    return retv21
}

func main() {
    main0()
}
