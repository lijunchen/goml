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
    var retv15 int32
    var jp17 int32
    switch xs__2.(type) {
    case List__int32_Nil:
        jp17 = 0
    case List__int32_Cons:
        var x7 List__int32 = xs__2.(List__int32_Cons)._1
        var tail__3 List__int32 = x7
        var t18 int32 = int_list_length(tail__3)
        var t19 int32 = 1 + t18
        jp17 = t19
    default:
        panic("non-exhaustive match")
    }
    retv15 = jp17
    return retv15
}

func main0() struct{} {
    var x__4 List__int32 = List__int32_Cons{
        _0: 1,
        _1: List__int32_Nil{},
    }
    var length__5 int32 = list_length__T_int32(x__4)
    println__T_int32(length__5)
    var t21 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var x__6 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t21,
    }
    var length__7 int32 = list_length__T_int32(x__6)
    println__T_int32(length__7)
    var t22 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t23 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t22,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t23,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t24 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t24,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t25 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t25,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    return struct{}{}
}

func list_length__T_int32(xs__0 List__int32) int32 {
    var retv27 int32
    var jp29 int32
    switch xs__0.(type) {
    case List__int32_Nil:
        jp29 = 0
    case List__int32_Cons:
        var x5 List__int32 = xs__0.(List__int32_Cons)._1
        var tail__1 List__int32 = x5
        var t30 int32 = list_length__T_int32(tail__1)
        var t31 int32 = 1 + t30
        jp29 = t31
    default:
        panic("non-exhaustive match")
    }
    retv27 = jp29
    return retv27
}

func println__T_int32(value__1 int32) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func list_length__T_unit(xs__0 List__unit) int32 {
    var retv36 int32
    var jp38 int32
    switch xs__0.(type) {
    case List__unit_Nil:
        jp38 = 0
    case List__unit_Cons:
        var x5 List__unit = xs__0.(List__unit_Cons)._1
        var tail__1 List__unit = x5
        var t39 int32 = list_length__T_unit(tail__1)
        var t40 int32 = 1 + t39
        jp38 = t40
    default:
        panic("non-exhaustive match")
    }
    retv36 = jp38
    return retv36
}

func list_length__T_bool(xs__0 List__bool) int32 {
    var retv42 int32
    var jp44 int32
    switch xs__0.(type) {
    case List__bool_Nil:
        jp44 = 0
    case List__bool_Cons:
        var x5 List__bool = xs__0.(List__bool_Cons)._1
        var tail__1 List__bool = x5
        var t45 int32 = list_length__T_bool(tail__1)
        var t46 int32 = 1 + t45
        jp44 = t46
    default:
        panic("non-exhaustive match")
    }
    retv42 = jp44
    return retv42
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv48 string
    var t49 string = _goml_runtime_core_int32_to_string(self__13)
    retv48 = t49
    return retv48
}

func main() {
    main0()
}
