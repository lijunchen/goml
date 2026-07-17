package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
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

func int_list_length(xs__2 List__int32) int32 {
    var retv69 int32
    var jp71 int32
    switch xs__2.(type) {
    case List__int32_Nil:
        jp71 = 0
    case List__int32_Cons:
        var x61 List__int32 = xs__2.(List__int32_Cons)._1
        var tail__3 List__int32 = x61
        var t72 int32 = int_list_length(tail__3)
        var t73 int32 = 1 + t72
        jp71 = t73
    default:
        panic("non-exhaustive match")
    }
    retv69 = jp71
    return retv69
}

func main0() struct{} {
    var x__4 List__int32 = List__int32_Cons{
        _0: 1,
        _1: List__int32_Nil{},
    }
    var length__5 int32 = list_length__T_int32(x__4)
    println__T_int32(length__5)
    var t75 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var x__6 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t75,
    }
    var length__7 int32 = list_length__T_int32(x__6)
    println__T_int32(length__7)
    var t76 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t77 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t76,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t77,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t78 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t78,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t79 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t79,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    return struct{}{}
}

func list_length__T_int32(xs__0 List__int32) int32 {
    var retv81 int32
    var jp83 int32
    switch xs__0.(type) {
    case List__int32_Nil:
        jp83 = 0
    case List__int32_Cons:
        var x59 List__int32 = xs__0.(List__int32_Cons)._1
        var tail__1 List__int32 = x59
        var t84 int32 = list_length__T_int32(tail__1)
        var t85 int32 = 1 + t84
        jp83 = t85
    default:
        panic("non-exhaustive match")
    }
    retv81 = jp83
    return retv81
}

func println__T_int32(value__1 int32) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func list_length__T_unit(xs__0 List__unit) int32 {
    var retv90 int32
    var jp92 int32
    switch xs__0.(type) {
    case List__unit_Nil:
        jp92 = 0
    case List__unit_Cons:
        var x59 List__unit = xs__0.(List__unit_Cons)._1
        var tail__1 List__unit = x59
        var t93 int32 = list_length__T_unit(tail__1)
        var t94 int32 = 1 + t93
        jp92 = t94
    default:
        panic("non-exhaustive match")
    }
    retv90 = jp92
    return retv90
}

func list_length__T_bool(xs__0 List__bool) int32 {
    var retv96 int32
    var jp98 int32
    switch xs__0.(type) {
    case List__bool_Nil:
        jp98 = 0
    case List__bool_Cons:
        var x59 List__bool = xs__0.(List__bool_Cons)._1
        var tail__1 List__bool = x59
        var t99 int32 = list_length__T_bool(tail__1)
        var t100 int32 = 1 + t99
        jp98 = t100
    default:
        panic("non-exhaustive match")
    }
    retv96 = jp98
    return retv96
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv102 string
    var t103 string = _goml_runtime_core_int32_to_string(self__38)
    retv102 = t103
    return retv102
}

func main() {
    main0()
}
