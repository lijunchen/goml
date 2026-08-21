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

type Ordering int32

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
        var x414 List__int32 = xs__2.(List__int32_Cons)._1
        var t425 int32 = int_list_length(x414)
        var t426 int32 = 1 + t425
        return t426
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
    var inline473 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__5)
    _goml_runtime_core_string_println(inline473)
    var t428 List__int = List__int_Cons{
        _0: 2,
        _1: List__int_Nil{},
    }
    var x__6 List__int = List__int_Cons{
        _0: 1,
        _1: t428,
    }
    var length__7 int32 = list_length__T_int(x__6)
    var inline470 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__7)
    _goml_runtime_core_string_println(inline470)
    var t429 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t430 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t429,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t430,
    }
    var length__9 int32 = int_list_length(x__8)
    var inline467 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__9)
    _goml_runtime_core_string_println(inline467)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    var inline464 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__11)
    _goml_runtime_core_string_println(inline464)
    var t431 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t431,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    var inline461 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__13)
    _goml_runtime_core_string_println(inline461)
    var t432 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t432,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    var inline458 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__15)
    _goml_runtime_core_string_println(inline458)
    return struct{}{}
}

func list_length__T_int(xs__0 List__int) int32 {
    switch xs__0.(type) {
    case List__int_Nil:
        return 0
    case List__int_Cons:
        var x412 List__int = xs__0.(List__int_Cons)._1
        var t437 int32 = list_length__T_int(x412)
        var t438 int32 = 1 + t437
        return t438
    default:
        panic("non-exhaustive match")
    }
}

func list_length__T_unit(xs__0 List__unit) int32 {
    switch xs__0.(type) {
    case List__unit_Nil:
        return 0
    case List__unit_Cons:
        var x412 List__unit = xs__0.(List__unit_Cons)._1
        var t446 int32 = list_length__T_unit(x412)
        var t447 int32 = 1 + t446
        return t447
    default:
        panic("non-exhaustive match")
    }
}

func list_length__T_bool(xs__0 List__bool) int32 {
    switch xs__0.(type) {
    case List__bool_Nil:
        return 0
    case List__bool_Cons:
        var x412 List__bool = xs__0.(List__bool_Cons)._1
        var t452 int32 = list_length__T_bool(x412)
        var t453 int32 = 1 + t452
        return t453
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t456 string = _goml_runtime_core_int32_to_string(self__154)
    return t456
}

func main() {
    main0()
}
