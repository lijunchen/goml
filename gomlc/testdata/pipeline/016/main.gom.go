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
        var x185 List__int32 = xs__2.(List__int32_Cons)._1
        var t196 int32 = int_list_length(x185)
        var t197 int32 = 1 + t196
        return t197
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
    var inline244 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__5)
    _goml_runtime_core_string_println(inline244)
    var t199 List__int = List__int_Cons{
        _0: 2,
        _1: List__int_Nil{},
    }
    var x__6 List__int = List__int_Cons{
        _0: 1,
        _1: t199,
    }
    var length__7 int32 = list_length__T_int(x__6)
    var inline241 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__7)
    _goml_runtime_core_string_println(inline241)
    var t200 List__int32 = List__int32_Cons{
        _0: 2,
        _1: List__int32_Nil{},
    }
    var t201 List__int32 = List__int32_Cons{
        _0: 1,
        _1: t200,
    }
    var x__8 List__int32 = List__int32_Cons{
        _0: 0,
        _1: t201,
    }
    var length__9 int32 = int_list_length(x__8)
    var inline238 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__9)
    _goml_runtime_core_string_println(inline238)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    var inline235 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__11)
    _goml_runtime_core_string_println(inline235)
    var t202 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t202,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    var inline232 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__13)
    _goml_runtime_core_string_println(inline232)
    var t203 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t203,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    var inline229 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(length__15)
    _goml_runtime_core_string_println(inline229)
    return struct{}{}
}

func list_length__T_int(xs__0 List__int) int32 {
    switch xs__0.(type) {
    case List__int_Nil:
        return 0
    case List__int_Cons:
        var x183 List__int = xs__0.(List__int_Cons)._1
        var t208 int32 = list_length__T_int(x183)
        var t209 int32 = 1 + t208
        return t209
    default:
        panic("non-exhaustive match")
    }
}

func list_length__T_unit(xs__0 List__unit) int32 {
    switch xs__0.(type) {
    case List__unit_Nil:
        return 0
    case List__unit_Cons:
        var x183 List__unit = xs__0.(List__unit_Cons)._1
        var t217 int32 = list_length__T_unit(x183)
        var t218 int32 = 1 + t217
        return t218
    default:
        panic("non-exhaustive match")
    }
}

func list_length__T_bool(xs__0 List__bool) int32 {
    switch xs__0.(type) {
    case List__bool_Nil:
        return 0
    case List__bool_Cons:
        var x183 List__bool = xs__0.(List__bool_Cons)._1
        var t223 int32 = list_length__T_bool(x183)
        var t224 int32 = 1 + t223
        return t224
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t227 string = _goml_runtime_core_int32_to_string(self__70)
    return t227
}

func main() {
    main0()
}
