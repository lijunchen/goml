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

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func maybe_value(flag__0 bool) Option__int32 {
    var retv13 Option__int32
    var jp15 Option__int32
    if flag__0 {
        var t16 Option__int32 = Some{
            _0: 4,
        }
        jp15 = t16
    } else {
        jp15 = None{}
    }
    retv13 = jp15
    return retv13
}

func add(a__1 int32, b__2 int32) int32 {
    var retv18 int32
    var t19 int32 = a__1 + b__2
    retv18 = t19
    return retv18
}

func plus_two(flag__3 bool) Option__int32 {
    var retv21 Option__int32
    var mtmp7 Option__int32 = maybe_value(flag__3)
    var jp23 int32
    switch mtmp7.(type) {
    case None:
        retv21 = None{}
        return retv21
    case Some:
        var x8 int32 = mtmp7.(Some)._0
        var try_value__15 int32 = x8
        jp23 = try_value__15
        var t24 int32 = add(jp23, 2)
        var t25 Option__int32 = Some{
            _0: t24,
        }
        retv21 = t25
        return retv21
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__int32) string {
    var retv27 string
    var jp29 string
    switch opt__4.(type) {
    case None:
        jp29 = "none"
    case Some:
        var x9 int32 = opt__4.(Some)._0
        var value__5 int32 = x9
        var t30 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t31 string = "some=" + t30
        jp29 = t31
    default:
        panic("non-exhaustive match")
    }
    retv27 = jp29
    return retv27
}

func main0() struct{} {
    var t33 Option__int32 = plus_two(true)
    var t34 string = show(t33)
    println__T_string(t34)
    var t35 Option__int32 = plus_two(false)
    var t36 string = show(t35)
    println__T_string(t36)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv38 string
    var t39 string = _goml_runtime_core_int32_to_string(self__2)
    retv38 = t39
    return retv38
}

func println__T_string(value__1 string) struct{} {
    var t41 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t41)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv44 string
    retv44 = self__9
    return retv44
}

func main() {
    main0()
}
