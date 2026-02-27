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
    var ret83 int32
    switch xs__2 := xs__2.(type) {
    case List__int32_Nil:
        ret83 = 0
    case List__int32_Cons:
        var x3 List__int32 = xs__2._1
        var tail__3 List__int32 = x3
        var t67 int32 = int_list_length(tail__3)
        ret83 = 1 + t67
    }
    return ret83
}

func main0() struct{} {
    var ret84 struct{}
    var t68 List__int32 = List__int32_Nil{}
    var x__4 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t68,
    }
    var length__5 int32 = list_length__T_int32(x__4)
    println__T_int32(length__5)
    var t70 List__int32 = List__int32_Nil{}
    var t69 List__int32 = List__int32_Cons{
        _0: 2,
        _1: t70,
    }
    var x__6 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t69,
    }
    var length__7 int32 = list_length__T_int32(x__6)
    println__T_int32(length__7)
    var t73 List__int32 = List__int32_Nil{}
    var t72 List__int32 = List__int32_Cons{
        _0: 2,
        _1: t73,
    }
    var t71 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t72,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t71,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var t74 List__unit = List__unit_Nil{}
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t74,
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t76 List__unit = List__unit_Nil{}
    var t75 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t76,
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t75,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t78 List__bool = List__bool_Nil{}
    var t77 List__bool = List__bool_Cons{
        _0: false,
        _1: t78,
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t77,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    ret84 = struct{}{}
    return ret84
}

func list_length__T_int32(xs__0 List__int32) int32 {
    var ret85 int32
    switch xs__0 := xs__0.(type) {
    case List__int32_Nil:
        ret85 = 0
    case List__int32_Cons:
        var x1 List__int32 = xs__0._1
        var tail__1 List__int32 = x1
        var t79 int32 = list_length__T_int32(tail__1)
        ret85 = 1 + t79
    }
    return ret85
}

func println__T_int32(value__1 int32) struct{} {
    var ret86 struct{}
    var t80 string = int32_to_string(value__1)
    ret86 = string_println(t80)
    return ret86
}

func list_length__T_unit(xs__0 List__unit) int32 {
    var ret87 int32
    switch xs__0 := xs__0.(type) {
    case List__unit_Nil:
        ret87 = 0
    case List__unit_Cons:
        var x1 List__unit = xs__0._1
        var tail__1 List__unit = x1
        var t81 int32 = list_length__T_unit(tail__1)
        ret87 = 1 + t81
    }
    return ret87
}

func list_length__T_bool(xs__0 List__bool) int32 {
    var ret88 int32
    switch xs__0 := xs__0.(type) {
    case List__bool_Nil:
        ret88 = 0
    case List__bool_Cons:
        var x1 List__bool = xs__0._1
        var tail__1 List__bool = x1
        var t82 int32 = list_length__T_bool(tail__1)
        ret88 = 1 + t82
    }
    return ret88
}

func main() {
    main0()
}
