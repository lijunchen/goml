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

type GoError = error

func int_list_length(xs__2 List__int32) int32 {
    var retv11 int32
    var jp13 int32
    switch xs__2.(type) {
    case List__int32_Nil:
        jp13 = 0
    case List__int32_Cons:
        var x3 List__int32 = xs__2.(List__int32_Cons)._1
        var tail__3 List__int32 = x3
        var t14 int32 = int_list_length(tail__3)
        var t15 int32 = 1 + t14
        jp13 = t15
    default:
        panic("non-exhaustive match")
    }
    retv11 = jp13
    return retv11
}

func main0() struct{} {
    var x__4 List__int32 = List__int32_Cons{
        _0: 1,
        _1: List__int32_Nil{},
    }
    var length__5 int32 = list_length__T_int32(x__4)
    println__T_int32(length__5)
    var t17 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var x__6 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t17,
    }
    var length__7 int32 = list_length__T_int32(x__6)
    println__T_int32(length__7)
    var t18 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t19 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t18,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t19,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t20 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t20,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t21 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t21,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    return struct{}{}
}

func list_length__T_int32(xs__0 List__int32) int32 {
    var retv23 int32
    var jp25 int32
    switch xs__0.(type) {
    case List__int32_Nil:
        jp25 = 0
    case List__int32_Cons:
        var x1 List__int32 = xs__0.(List__int32_Cons)._1
        var tail__1 List__int32 = x1
        var t26 int32 = list_length__T_int32(tail__1)
        var t27 int32 = 1 + t26
        jp25 = t27
    default:
        panic("non-exhaustive match")
    }
    retv23 = jp25
    return retv23
}

func println__T_int32(value__1 int32) struct{} {
    var t29 string = int32_to_string(value__1)
    string_println(t29)
    return struct{}{}
}

func list_length__T_unit(xs__0 List__unit) int32 {
    var retv32 int32
    var jp34 int32
    switch xs__0.(type) {
    case List__unit_Nil:
        jp34 = 0
    case List__unit_Cons:
        var x1 List__unit = xs__0.(List__unit_Cons)._1
        var tail__1 List__unit = x1
        var t35 int32 = list_length__T_unit(tail__1)
        var t36 int32 = 1 + t35
        jp34 = t36
    default:
        panic("non-exhaustive match")
    }
    retv32 = jp34
    return retv32
}

func list_length__T_bool(xs__0 List__bool) int32 {
    var retv38 int32
    var jp40 int32
    switch xs__0.(type) {
    case List__bool_Nil:
        jp40 = 0
    case List__bool_Cons:
        var x1 List__bool = xs__0.(List__bool_Cons)._1
        var tail__1 List__bool = x1
        var t41 int32 = list_length__T_bool(tail__1)
        var t42 int32 = 1 + t41
        jp40 = t42
    default:
        panic("non-exhaustive match")
    }
    retv38 = jp40
    return retv38
}

func main() {
    main0()
}
