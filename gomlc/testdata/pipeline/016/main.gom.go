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
    switch xs__2.(type) {
    case List__int32_Nil:
        return 0
    case List__int32_Cons:
        var x158 List__int32 = xs__2.(List__int32_Cons)._1
        var t169 int32 = int_list_length(x158)
        var t170 int32 = 1 + t169
        return t170
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__4 List__int = List__int_Cons{
        _0: 1,
        _1: List__int_Nil{},
    }
    var length__5 int32 = list_length__T_int(x__4)
    println__T_int32(length__5)
    var t172 List__int = List__int_Cons{
        _0: 2,
        _1: List__int_Nil{},
    }
    var x__6 List__int = List__int_Cons{
        _0: 1,
        _1: t172,
    }
    var length__7 int32 = list_length__T_int(x__6)
    println__T_int32(length__7)
    var t173 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t174 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t173,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t174,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t175 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t175,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t176 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t176,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    return struct{}{}
}

func list_length__T_int(xs__0 List__int) int32 {
    switch xs__0.(type) {
    case List__int_Nil:
        return 0
    case List__int_Cons:
        var x156 List__int = xs__0.(List__int_Cons)._1
        var t181 int32 = list_length__T_int(x156)
        var t182 int32 = 1 + t181
        return t182
    default:
        panic("non-exhaustive match")
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t184 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t184)
    return struct{}{}
}

func list_length__T_unit(xs__0 List__unit) int32 {
    switch xs__0.(type) {
    case List__unit_Nil:
        return 0
    case List__unit_Cons:
        var x156 List__unit = xs__0.(List__unit_Cons)._1
        var t190 int32 = list_length__T_unit(x156)
        var t191 int32 = 1 + t190
        return t191
    default:
        panic("non-exhaustive match")
    }
}

func list_length__T_bool(xs__0 List__bool) int32 {
    switch xs__0.(type) {
    case List__bool_Nil:
        return 0
    case List__bool_Cons:
        var x156 List__bool = xs__0.(List__bool_Cons)._1
        var t196 int32 = list_length__T_bool(x156)
        var t197 int32 = 1 + t196
        return t197
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t200 string = _goml_runtime_core_int32_to_string(self__43)
    return t200
}

func main() {
    main0()
}
