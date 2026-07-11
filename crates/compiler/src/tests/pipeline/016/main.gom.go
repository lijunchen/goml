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
    var retv18 int32
    var jp20 int32
    switch xs__2.(type) {
    case List__int32_Nil:
        jp20 = 0
    case List__int32_Cons:
        var x10 List__int32 = xs__2.(List__int32_Cons)._1
        var tail__3 List__int32 = x10
        var t21 int32 = int_list_length(tail__3)
        var t22 int32 = 1 + t21
        jp20 = t22
    default:
        panic("non-exhaustive match")
    }
    retv18 = jp20
    return retv18
}

func main0() struct{} {
    var x__4 List__int32 = List__int32_Cons{
        _0: 1,
        _1: List__int32_Nil{},
    }
    var length__5 int32 = list_length__T_int32(x__4)
    println__T_int32(length__5)
    var t24 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var x__6 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t24,
    }
    var length__7 int32 = list_length__T_int32(x__6)
    println__T_int32(length__7)
    var t25 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t26 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t25,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t26,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t27 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t27,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t28 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t28,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    return struct{}{}
}

func list_length__T_int32(xs__0 List__int32) int32 {
    var retv30 int32
    var jp32 int32
    switch xs__0.(type) {
    case List__int32_Nil:
        jp32 = 0
    case List__int32_Cons:
        var x8 List__int32 = xs__0.(List__int32_Cons)._1
        var tail__1 List__int32 = x8
        var t33 int32 = list_length__T_int32(tail__1)
        var t34 int32 = 1 + t33
        jp32 = t34
    default:
        panic("non-exhaustive match")
    }
    retv30 = jp32
    return retv30
}

func println__T_int32(value__1 int32) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func list_length__T_unit(xs__0 List__unit) int32 {
    var retv39 int32
    var jp41 int32
    switch xs__0.(type) {
    case List__unit_Nil:
        jp41 = 0
    case List__unit_Cons:
        var x8 List__unit = xs__0.(List__unit_Cons)._1
        var tail__1 List__unit = x8
        var t42 int32 = list_length__T_unit(tail__1)
        var t43 int32 = 1 + t42
        jp41 = t43
    default:
        panic("non-exhaustive match")
    }
    retv39 = jp41
    return retv39
}

func list_length__T_bool(xs__0 List__bool) int32 {
    var retv45 int32
    var jp47 int32
    switch xs__0.(type) {
    case List__bool_Nil:
        jp47 = 0
    case List__bool_Cons:
        var x8 List__bool = xs__0.(List__bool_Cons)._1
        var tail__1 List__bool = x8
        var t48 int32 = list_length__T_bool(tail__1)
        var t49 int32 = 1 + t48
        jp47 = t49
    default:
        panic("non-exhaustive match")
    }
    retv45 = jp47
    return retv45
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv51 string
    var t52 string = _goml_runtime_core_int32_to_string(self__13)
    retv51 = t52
    return retv51
}

func main() {
    main0()
}
