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
    var retv72 int32
    var jp74 int32
    switch xs__2.(type) {
    case List__int32_Nil:
        jp74 = 0
    case List__int32_Cons:
        var x64 List__int32 = xs__2.(List__int32_Cons)._1
        var tail__3 List__int32 = x64
        var t75 int32 = int_list_length(tail__3)
        var t76 int32 = 1 + t75
        jp74 = t76
    default:
        panic("non-exhaustive match")
    }
    retv72 = jp74
    return retv72
}

func main0() struct{} {
    var x__4 List__int32 = List__int32_Cons{
        _0: 1,
        _1: List__int32_Nil{},
    }
    var length__5 int32 = list_length__T_int32(x__4)
    println__T_int32(length__5)
    var t78 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var x__6 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t78,
    }
    var length__7 int32 = list_length__T_int32(x__6)
    println__T_int32(length__7)
    var t79 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t80 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t79,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t80,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t81 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t81,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t82 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t82,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    return struct{}{}
}

func list_length__T_int32(xs__0 List__int32) int32 {
    var retv84 int32
    var jp86 int32
    switch xs__0.(type) {
    case List__int32_Nil:
        jp86 = 0
    case List__int32_Cons:
        var x62 List__int32 = xs__0.(List__int32_Cons)._1
        var tail__1 List__int32 = x62
        var t87 int32 = list_length__T_int32(tail__1)
        var t88 int32 = 1 + t87
        jp86 = t88
    default:
        panic("non-exhaustive match")
    }
    retv84 = jp86
    return retv84
}

func println__T_int32(value__1 int32) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func list_length__T_unit(xs__0 List__unit) int32 {
    var retv93 int32
    var jp95 int32
    switch xs__0.(type) {
    case List__unit_Nil:
        jp95 = 0
    case List__unit_Cons:
        var x62 List__unit = xs__0.(List__unit_Cons)._1
        var tail__1 List__unit = x62
        var t96 int32 = list_length__T_unit(tail__1)
        var t97 int32 = 1 + t96
        jp95 = t97
    default:
        panic("non-exhaustive match")
    }
    retv93 = jp95
    return retv93
}

func list_length__T_bool(xs__0 List__bool) int32 {
    var retv99 int32
    var jp101 int32
    switch xs__0.(type) {
    case List__bool_Nil:
        jp101 = 0
    case List__bool_Cons:
        var x62 List__bool = xs__0.(List__bool_Cons)._1
        var tail__1 List__bool = x62
        var t102 int32 = list_length__T_bool(tail__1)
        var t103 int32 = 1 + t102
        jp101 = t103
    default:
        panic("non-exhaustive match")
    }
    retv99 = jp101
    return retv99
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv105 string
    var t106 string = _goml_runtime_core_int32_to_string(self__41)
    retv105 = t106
    return retv105
}

func main() {
    main0()
}
