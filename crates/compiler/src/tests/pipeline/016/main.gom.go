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
    var retv33 int32
    var jp35 int32
    switch xs__2.(type) {
    case List__int32_Nil:
        jp35 = 0
    case List__int32_Cons:
        var x25 List__int32 = xs__2.(List__int32_Cons)._1
        var tail__3 List__int32 = x25
        var t36 int32 = int_list_length(tail__3)
        var t37 int32 = 1 + t36
        jp35 = t37
    default:
        panic("non-exhaustive match")
    }
    retv33 = jp35
    return retv33
}

func main0() struct{} {
    var x__4 List__int32 = List__int32_Cons{
        _0: 1,
        _1: List__int32_Nil{},
    }
    var length__5 int32 = list_length__T_int32(x__4)
    println__T_int32(length__5)
    var t39 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var x__6 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t39,
    }
    var length__7 int32 = list_length__T_int32(x__6)
    println__T_int32(length__7)
    var t40 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t41 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t40,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t41,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t42 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t42,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t43 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t43,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    return struct{}{}
}

func list_length__T_int32(xs__0 List__int32) int32 {
    var retv45 int32
    var jp47 int32
    switch xs__0.(type) {
    case List__int32_Nil:
        jp47 = 0
    case List__int32_Cons:
        var x23 List__int32 = xs__0.(List__int32_Cons)._1
        var tail__1 List__int32 = x23
        var t48 int32 = list_length__T_int32(tail__1)
        var t49 int32 = 1 + t48
        jp47 = t49
    default:
        panic("non-exhaustive match")
    }
    retv45 = jp47
    return retv45
}

func println__T_int32(value__1 int32) struct{} {
    var t51 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t51)
    return struct{}{}
}

func list_length__T_unit(xs__0 List__unit) int32 {
    var retv54 int32
    var jp56 int32
    switch xs__0.(type) {
    case List__unit_Nil:
        jp56 = 0
    case List__unit_Cons:
        var x23 List__unit = xs__0.(List__unit_Cons)._1
        var tail__1 List__unit = x23
        var t57 int32 = list_length__T_unit(tail__1)
        var t58 int32 = 1 + t57
        jp56 = t58
    default:
        panic("non-exhaustive match")
    }
    retv54 = jp56
    return retv54
}

func list_length__T_bool(xs__0 List__bool) int32 {
    var retv60 int32
    var jp62 int32
    switch xs__0.(type) {
    case List__bool_Nil:
        jp62 = 0
    case List__bool_Cons:
        var x23 List__bool = xs__0.(List__bool_Cons)._1
        var tail__1 List__bool = x23
        var t63 int32 = list_length__T_bool(tail__1)
        var t64 int32 = 1 + t63
        jp62 = t64
    default:
        panic("non-exhaustive match")
    }
    retv60 = jp62
    return retv60
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv66 string
    var t67 string = _goml_runtime_core_int32_to_string(self__13)
    retv66 = t67
    return retv66
}

func main() {
    main0()
}
