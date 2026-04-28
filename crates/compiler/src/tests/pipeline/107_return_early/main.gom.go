package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_f_0 struct {}

type GoError = error

func early(x__0 int32) int32 {
    var retv19 int32
    var t24 bool = x__0 < 0
    if t24 {
        retv19 = 0
        return retv19
    } else {
        var t23 bool = x__0 == 0
        if t23 {
            retv19 = 1
            return retv19
        } else {
            var t22 int32 = x__0 + 2
            retv19 = t22
            return retv19
        }
    }
}

func closure_early(x__1 int32) int32 {
    var retv26 int32
    var f__3 closure_env_f_0 = closure_env_f_0{}
    var t27 int32 = _goml_inherent_closure_env_f_0_closure_env_f_0_apply(f__3, x__1)
    retv26 = t27
    return retv26
}

func unit_ret(flag__4 bool) struct{} {
    if flag__4 {
        return struct{}{}
    } else {
        println__T_string("after")
        return struct{}{}
    }
}

func main0() struct{} {
    print__T_string("e-1: ")
    var t31 int32 = -1
    var t32 int32 = early(t31)
    println__T_int32(t32)
    print__T_string("e0: ")
    var t33 int32 = early(0)
    println__T_int32(t33)
    print__T_string("e3: ")
    var t34 int32 = early(3)
    println__T_int32(t34)
    print__T_string("c7: ")
    var t35 int32 = closure_early(7)
    println__T_int32(t35)
    print__T_string("c2: ")
    var t36 int32 = closure_early(2)
    println__T_int32(t36)
    unit_ret(true)
    unit_ret(false)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    string_print(value__0)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t42 string = int32_to_string(value__1)
    string_println(t42)
    return struct{}{}
}

func _goml_inherent_closure_env_f_0_closure_env_f_0_apply(env17 closure_env_f_0, y__2 int32) int32 {
    var retv45 int32
    var t48 bool = y__2 > 5
    if t48 {
        retv45 = y__2
        return retv45
    } else {
        var t47 int32 = y__2 + 10
        retv45 = t47
        return retv45
    }
}

func main() {
    main0()
}
