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

type List__int interface {
    isList__int()
}

type List__int_Nil struct {}

func (_ List__int_Nil) isList__int() {}

type List__int_Cons struct {
    _0 int
    _1 List__int
}

func (_ List__int_Cons) isList__int() {}

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
    var retv79 int32
    var jp81 int32
    switch xs__2.(type) {
    case List__int32_Nil:
        jp81 = 0
    case List__int32_Cons:
        var x71 List__int32 = xs__2.(List__int32_Cons)._1
        var tail__3 List__int32 = x71
        var t82 int32 = int_list_length(tail__3)
        var t83 int32 = 1 + t82
        jp81 = t83
    default:
        panic("non-exhaustive match")
    }
    retv79 = jp81
    return retv79
}

func main0() struct{} {
    var x__4 List__int = List__int_Cons{
        _0: 1,
        _1: List__int_Nil{},
    }
    var length__5 int32 = list_length__T_int(x__4)
    println__T_int32(length__5)
    var t85 List__int = List__int_Cons{
        _0: 2,
        _1: List__int_Nil{},
    }
    var x__6 List__int = List__int_Cons{
        _0: 1,
        _1: t85,
    }
    var length__7 int32 = list_length__T_int(x__6)
    println__T_int32(length__7)
    var t86 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t87 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t86,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t87,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t88 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t88,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t89 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t89,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    return struct{}{}
}

func list_length__T_int(xs__0 List__int) int32 {
    var retv91 int32
    var jp93 int32
    switch xs__0.(type) {
    case List__int_Nil:
        jp93 = 0
    case List__int_Cons:
        var x69 List__int = xs__0.(List__int_Cons)._1
        var tail__1 List__int = x69
        var t94 int32 = list_length__T_int(tail__1)
        var t95 int32 = 1 + t94
        jp93 = t95
    default:
        panic("non-exhaustive match")
    }
    retv91 = jp93
    return retv91
}

func println__T_int32(value__1 int32) struct{} {
    var t97 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t97)
    return struct{}{}
}

func list_length__T_unit(xs__0 List__unit) int32 {
    var retv100 int32
    var jp102 int32
    switch xs__0.(type) {
    case List__unit_Nil:
        jp102 = 0
    case List__unit_Cons:
        var x69 List__unit = xs__0.(List__unit_Cons)._1
        var tail__1 List__unit = x69
        var t103 int32 = list_length__T_unit(tail__1)
        var t104 int32 = 1 + t103
        jp102 = t104
    default:
        panic("non-exhaustive match")
    }
    retv100 = jp102
    return retv100
}

func list_length__T_bool(xs__0 List__bool) int32 {
    var retv106 int32
    var jp108 int32
    switch xs__0.(type) {
    case List__bool_Nil:
        jp108 = 0
    case List__bool_Cons:
        var x69 List__bool = xs__0.(List__bool_Cons)._1
        var tail__1 List__bool = x69
        var t109 int32 = list_length__T_bool(tail__1)
        var t110 int32 = 1 + t109
        jp108 = t110
    default:
        panic("non-exhaustive match")
    }
    retv106 = jp108
    return retv106
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv112 string
    var t113 string = _goml_runtime_core_int32_to_string(self__43)
    retv112 = t113
    return retv112
}

func main() {
    main0()
}
