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

func maybe_seed(flag__0 bool) Option__int32 {
    var retv12 Option__int32
    var jp14 Option__int32
    if flag__0 {
        var t15 Option__int32 = Some{
            _0: 3,
        }
        jp14 = t15
    } else {
        jp14 = None{}
    }
    retv12 = jp14
    return retv12
}

func maybe_double(value__1 int32) Option__int32 {
    var retv17 Option__int32
    var t20 bool = value__1 > 0
    var jp19 Option__int32
    if t20 {
        var t21 int32 = value__1 * 2
        var t22 Option__int32 = Some{
            _0: t21,
        }
        jp19 = t22
    } else {
        jp19 = None{}
    }
    retv17 = jp19
    return retv17
}

func maybe_total(flag__2 bool) Option__int32 {
    var retv24 Option__int32
    var mtmp4 Option__int32 = maybe_seed(flag__2)
    var jp26 int32
    switch mtmp4.(type) {
    case None:
        retv24 = None{}
        return retv24
    case Some:
        var x5 int32 = mtmp4.(Some)._0
        var try_value__22 int32 = x5
        jp26 = try_value__22
        var a__3 int32 = jp26
        var mtmp6 Option__int32 = maybe_double(a__3)
        var jp28 int32
        switch mtmp6.(type) {
        case None:
            retv24 = None{}
            return retv24
        case Some:
            var x7 int32 = mtmp6.(Some)._0
            var try_value__26 int32 = x7
            jp28 = try_value__26
            var b__4 int32 = jp28
            var t29 int32 = a__3 + b__4
            var t30 Option__int32 = Some{
                _0: t29,
            }
            retv24 = t30
            return retv24
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    var retv32 string
    var jp34 string
    switch opt__5.(type) {
    case None:
        jp34 = "none"
    case Some:
        var x8 int32 = opt__5.(Some)._0
        var value__6 int32 = x8
        var t35 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t36 string = "some=" + t35
        jp34 = t36
    default:
        panic("non-exhaustive match")
    }
    retv32 = jp34
    return retv32
}

func main0() struct{} {
    var t38 Option__int32 = maybe_total(true)
    var t39 string = show(t38)
    println__T_string(t39)
    var t40 Option__int32 = maybe_total(false)
    var t41 string = show(t40)
    println__T_string(t41)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv43 string
    var t44 string = _goml_runtime_core_int32_to_string(self__2)
    retv43 = t44
    return retv43
}

func println__T_string(value__1 string) struct{} {
    var t46 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t46)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv49 string
    retv49 = self__9
    return retv49
}

func main() {
    main0()
}
