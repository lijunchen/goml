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
    var retv119 int32
    var jp121 int32
    switch xs__2.(type) {
    case List__int32_Nil:
        jp121 = 0
    case List__int32_Cons:
        var x111 List__int32 = xs__2.(List__int32_Cons)._1
        var tail__3 List__int32 = x111
        var t122 int32 = int_list_length(tail__3)
        var t123 int32 = 1 + t122
        jp121 = t123
    default:
        panic("non-exhaustive match")
    }
    retv119 = jp121
    return retv119
}

func main0() struct{} {
    var x__4 List__int = List__int_Cons{
        _0: 1,
        _1: List__int_Nil{},
    }
    var length__5 int32 = list_length__T_int(x__4)
    println__T_int32(length__5)
    var t125 List__int = List__int_Cons{
        _0: 2,
        _1: List__int_Nil{},
    }
    var x__6 List__int = List__int_Cons{
        _0: 1,
        _1: t125,
    }
    var length__7 int32 = list_length__T_int(x__6)
    println__T_int32(length__7)
    var t126 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t127 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t126,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t127,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t128 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t128,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t129 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t129,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    return struct{}{}
}

func list_length__T_int(xs__0 List__int) int32 {
    var retv131 int32
    var jp133 int32
    switch xs__0.(type) {
    case List__int_Nil:
        jp133 = 0
    case List__int_Cons:
        var x109 List__int = xs__0.(List__int_Cons)._1
        var tail__1 List__int = x109
        var t134 int32 = list_length__T_int(tail__1)
        var t135 int32 = 1 + t134
        jp133 = t135
    default:
        panic("non-exhaustive match")
    }
    retv131 = jp133
    return retv131
}

func println__T_int32(value__1 int32) struct{} {
    var t137 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t137)
    return struct{}{}
}

func list_length__T_unit(xs__0 List__unit) int32 {
    var retv140 int32
    var jp142 int32
    switch xs__0.(type) {
    case List__unit_Nil:
        jp142 = 0
    case List__unit_Cons:
        var x109 List__unit = xs__0.(List__unit_Cons)._1
        var tail__1 List__unit = x109
        var t143 int32 = list_length__T_unit(tail__1)
        var t144 int32 = 1 + t143
        jp142 = t144
    default:
        panic("non-exhaustive match")
    }
    retv140 = jp142
    return retv140
}

func list_length__T_bool(xs__0 List__bool) int32 {
    var retv146 int32
    var jp148 int32
    switch xs__0.(type) {
    case List__bool_Nil:
        jp148 = 0
    case List__bool_Cons:
        var x109 List__bool = xs__0.(List__bool_Cons)._1
        var tail__1 List__bool = x109
        var t149 int32 = list_length__T_bool(tail__1)
        var t150 int32 = 1 + t149
        jp148 = t150
    default:
        panic("non-exhaustive match")
    }
    retv146 = jp148
    return retv146
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv152 string
    var t153 string = _goml_runtime_core_int32_to_string(self__43)
    retv152 = t153
    return retv152
}

func main() {
    main0()
}
