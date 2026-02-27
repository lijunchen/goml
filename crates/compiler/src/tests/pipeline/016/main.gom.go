package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type List__int32 interface {
    isList__int32()
}

type List__int32_Nil struct {}

func (_ List__int32_Nil) isList__int32() {}

type List__int32_Cons struct {
    _0 int32
    _1 List__int32
}

func (_ List__int32_Cons) isList__int32() {}

type List__unit interface {
    isList__unit()
}

type List__unit_Nil struct {}

func (_ List__unit_Nil) isList__unit() {}

type List__unit_Cons struct {
    _0 struct{}
    _1 List__unit
}

func (_ List__unit_Cons) isList__unit() {}

type List__bool interface {
    isList__bool()
}

type List__bool_Nil struct {}

func (_ List__bool_Nil) isList__bool() {}

type List__bool_Cons struct {
    _0 bool
    _1 List__bool
}

func (_ List__bool_Cons) isList__bool() {}

func int_list_length(xs__2 List__int32) int32 {
    var ret26 int32
    switch xs__2 := xs__2.(type) {
    case List__int32_Nil:
        ret26 = 0
    case List__int32_Cons:
        var x3 List__int32 = xs__2._1
        var tail__3 List__int32 = x3
        var t10 int32 = int_list_length(tail__3)
        ret26 = 1 + t10
    }
    return ret26
}

func main0() struct{} {
    var ret27 struct{}
    var t11 List__int32 = List__int32_Nil{}
    var x__4 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t11,
    }
    var length__5 int32 = list_length__T_int32(x__4)
    println__T_int32(length__5)
    var t13 List__int32 = List__int32_Nil{}
    var t12 List__int32 = List__int32_Cons{
        _0: 2,
        _1: t13,
    }
    var x__6 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t12,
    }
    var length__7 int32 = list_length__T_int32(x__6)
    println__T_int32(length__7)
    var t16 List__int32 = List__int32_Nil{}
    var t15 List__int32 = List__int32_Cons{
        _0: 2,
        _1: t16,
    }
    var t14 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t15,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t14,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var t17 List__unit = List__unit_Nil{}
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t17,
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t19 List__unit = List__unit_Nil{}
    var t18 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t19,
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t18,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t21 List__bool = List__bool_Nil{}
    var t20 List__bool = List__bool_Cons{
        _0: false,
        _1: t21,
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t20,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    ret27 = struct{}{}
    return ret27
}

func list_length__T_int32(xs__0 List__int32) int32 {
    var ret28 int32
    switch xs__0 := xs__0.(type) {
    case List__int32_Nil:
        ret28 = 0
    case List__int32_Cons:
        var x1 List__int32 = xs__0._1
        var tail__1 List__int32 = x1
        var t22 int32 = list_length__T_int32(tail__1)
        ret28 = 1 + t22
    }
    return ret28
}

func println__T_int32(value__1 int32) struct{} {
    var ret29 struct{}
    var t23 string = int32_to_string(value__1)
    ret29 = string_println(t23)
    return ret29
}

func list_length__T_unit(xs__0 List__unit) int32 {
    var ret30 int32
    switch xs__0 := xs__0.(type) {
    case List__unit_Nil:
        ret30 = 0
    case List__unit_Cons:
        var x1 List__unit = xs__0._1
        var tail__1 List__unit = x1
        var t24 int32 = list_length__T_unit(tail__1)
        ret30 = 1 + t24
    }
    return ret30
}

func list_length__T_bool(xs__0 List__bool) int32 {
    var ret31 int32
    switch xs__0 := xs__0.(type) {
    case List__bool_Nil:
        ret31 = 0
    case List__bool_Cons:
        var x1 List__bool = xs__0._1
        var tail__1 List__bool = x1
        var t25 int32 = list_length__T_bool(tail__1)
        ret31 = 1 + t25
    }
    return ret31
}

func main() {
    main0()
}
