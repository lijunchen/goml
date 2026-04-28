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

type closure_env_f_0 struct {}

type GoError = error

func add_after_match(flag__0 bool) int32 {
    var retv8 int32
    var jp10 int32
    switch flag__0 {
    case true:
        retv8 = 5
        return retv8
    case false:
        jp10 = 7
        var value__1 int32 = jp10
        var t11 int32 = value__1 + 1
        retv8 = t11
        return retv8
    default:
        panic("non-exhaustive match")
    }
}

func receiver_after_match(flag__2 bool) string {
    var retv13 string
    var jp15 int32
    switch flag__2 {
    case true:
        retv13 = "early"
        return retv13
    case false:
        jp15 = 7
        var t16 string = int32_to_string(jp15)
        retv13 = t16
        return retv13
    default:
        panic("non-exhaustive match")
    }
}

func closure_after_match(flag__3 bool) int32 {
    var retv18 int32
    var f__6 closure_env_f_0 = closure_env_f_0{}
    var t19 int32 = _goml_inherent_x23_closure_x5f_env_x5f_f_x5f_0_x23_closure_x5f_env_x5f_f_x5f_0_x23_apply(f__6, flag__3)
    retv18 = t19
    return retv18
}

func main0() struct{} {
    var t21 int32 = add_after_match(false)
    println__T_int32(t21)
    var t22 int32 = add_after_match(true)
    println__T_int32(t22)
    var t23 string = receiver_after_match(false)
    println__T_string(t23)
    var t24 string = receiver_after_match(true)
    println__T_string(t24)
    var t25 int32 = closure_after_match(false)
    println__T_int32(t25)
    var t26 int32 = closure_after_match(true)
    println__T_int32(t26)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t28 string = int32_to_string(value__1)
    string_println(t28)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_f_x5f_0_x23_closure_x5f_env_x5f_f_x5f_0_x23_apply(env6 closure_env_f_0, inner__4 bool) int32 {
    var retv33 int32
    var jp35 int32
    switch inner__4 {
    case true:
        retv33 = 2
        return retv33
    case false:
        jp35 = 4
        var value__5 int32 = jp35
        var t36 int32 = value__5 + 3
        retv33 = t36
        return retv33
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
