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
    var retv163 int32
    var jp165 int32
    switch xs__2.(type) {
    case List__int32_Nil:
        jp165 = 0
    case List__int32_Cons:
        var x155 List__int32 = xs__2.(List__int32_Cons)._1
        var tail__3 List__int32 = x155
        var t166 int32 = int_list_length(tail__3)
        var t167 int32 = 1 + t166
        jp165 = t167
    default:
        panic("non-exhaustive match")
    }
    retv163 = jp165
    return retv163
}

func main0() struct{} {
    var x__4 List__int = List__int_Cons{
        _0: 1,
        _1: List__int_Nil{},
    }
    var length__5 int32 = list_length__T_int(x__4)
    println__T_int32(length__5)
    var t169 List__int = List__int_Cons{
        _0: 2,
        _1: List__int_Nil{},
    }
    var x__6 List__int = List__int_Cons{
        _0: 1,
        _1: t169,
    }
    var length__7 int32 = list_length__T_int(x__6)
    println__T_int32(length__7)
    var t170 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t171 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t170,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t171,
    }
    var length__9 int32 = int_list_length(x__8)
    println__T_int32(length__9)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    println__T_int32(length__11)
    var t172 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t172,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    println__T_int32(length__13)
    var t173 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t173,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    println__T_int32(length__15)
    return struct{}{}
}

func list_length__T_int(xs__0 List__int) int32 {
    var retv175 int32
    var jp177 int32
    switch xs__0.(type) {
    case List__int_Nil:
        jp177 = 0
    case List__int_Cons:
        var x153 List__int = xs__0.(List__int_Cons)._1
        var tail__1 List__int = x153
        var t178 int32 = list_length__T_int(tail__1)
        var t179 int32 = 1 + t178
        jp177 = t179
    default:
        panic("non-exhaustive match")
    }
    retv175 = jp177
    return retv175
}

func println__T_int32(value__1 int32) struct{} {
    var t181 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t181)
    return struct{}{}
}

func list_length__T_unit(xs__0 List__unit) int32 {
    var retv184 int32
    var jp186 int32
    switch xs__0.(type) {
    case List__unit_Nil:
        jp186 = 0
    case List__unit_Cons:
        var x153 List__unit = xs__0.(List__unit_Cons)._1
        var tail__1 List__unit = x153
        var t187 int32 = list_length__T_unit(tail__1)
        var t188 int32 = 1 + t187
        jp186 = t188
    default:
        panic("non-exhaustive match")
    }
    retv184 = jp186
    return retv184
}

func list_length__T_bool(xs__0 List__bool) int32 {
    var retv190 int32
    var jp192 int32
    switch xs__0.(type) {
    case List__bool_Nil:
        jp192 = 0
    case List__bool_Cons:
        var x153 List__bool = xs__0.(List__bool_Cons)._1
        var tail__1 List__bool = x153
        var t193 int32 = list_length__T_bool(tail__1)
        var t194 int32 = 1 + t193
        jp192 = t194
    default:
        panic("non-exhaustive match")
    }
    retv190 = jp192
    return retv190
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv196 string
    var t197 string = _goml_runtime_core_int32_to_string(self__43)
    retv196 = t197
    return retv196
}

func main() {
    main0()
}
