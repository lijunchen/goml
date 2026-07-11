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
    var retv15 Option__int32
    var jp17 Option__int32
    if flag__0 {
        var t18 Option__int32 = Some{
            _0: 3,
        }
        jp17 = t18
    } else {
        jp17 = None{}
    }
    retv15 = jp17
    return retv15
}

func maybe_double(value__1 int32) Option__int32 {
    var retv20 Option__int32
    var t23 bool = value__1 > 0
    var jp22 Option__int32
    if t23 {
        var t24 int32 = value__1 * 2
        var t25 Option__int32 = Some{
            _0: t24,
        }
        jp22 = t25
    } else {
        jp22 = None{}
    }
    retv20 = jp22
    return retv20
}

func maybe_total(flag__2 bool) Option__int32 {
    var retv27 Option__int32
    var mtmp7 Option__int32 = maybe_seed(flag__2)
    var jp29 int32
    switch mtmp7.(type) {
    case None:
        retv27 = None{}
        return retv27
    case Some:
        var x8 int32 = mtmp7.(Some)._0
        var try_value__22 int32 = x8
        jp29 = try_value__22
        var a__3 int32 = jp29
        var mtmp9 Option__int32 = maybe_double(a__3)
        var jp31 int32
        switch mtmp9.(type) {
        case None:
            retv27 = None{}
            return retv27
        case Some:
            var x10 int32 = mtmp9.(Some)._0
            var try_value__26 int32 = x10
            jp31 = try_value__26
            var b__4 int32 = jp31
            var t32 int32 = a__3 + b__4
            var t33 Option__int32 = Some{
                _0: t32,
            }
            retv27 = t33
            return retv27
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    var retv35 string
    var jp37 string
    switch opt__5.(type) {
    case None:
        jp37 = "none"
    case Some:
        var x11 int32 = opt__5.(Some)._0
        var value__6 int32 = x11
        var t38 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t39 string = "some=" + t38
        jp37 = t39
    default:
        panic("non-exhaustive match")
    }
    retv35 = jp37
    return retv35
}

func main0() struct{} {
    var t41 Option__int32 = maybe_total(true)
    var t42 string = show(t41)
    println__T_string(t42)
    var t43 Option__int32 = maybe_total(false)
    var t44 string = show(t43)
    println__T_string(t44)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv46 string
    var t47 string = _goml_runtime_core_int32_to_string(self__2)
    retv46 = t47
    return retv46
}

func println__T_string(value__1 string) struct{} {
    var t49 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t49)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv52 string
    retv52 = self__9
    return retv52
}

func main() {
    main0()
}
