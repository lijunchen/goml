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

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func maybe_primary(flag__0 bool) Option__int32 {
    var retv13 Option__int32
    var jp15 Option__int32
    if flag__0 {
        var t16 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        jp15 = t16
    } else {
        jp15 = Option__int32_None{}
    }
    retv13 = jp15
    return retv13
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var retv18 Option__int32
    var jp20 Option__int32
    if flag__1 {
        var t21 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        jp20 = t21
    } else {
        jp20 = Option__int32_None{}
    }
    retv18 = jp20
    return retv18
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var retv23 Option__string
    var mtmp4 Option__int32 = maybe_primary(primary__2)
    var jp25 int32
    switch mtmp4.(type) {
    case Option__int32_None:
        retv23 = Option__string_None{}
        return retv23
    case Option__int32_Some:
        var x5 int32 = mtmp4.(Option__int32_Some)._0
        var try_value__18 int32 = x5
        jp25 = try_value__18
        var value__4 int32 = jp25
        var mtmp6 Option__int32 = maybe_secondary(secondary__3)
        var jp27 string
        switch mtmp6.(type) {
        case Option__int32_None:
            jp27 = "extra=none"
        case Option__int32_Some:
            var x7 int32 = mtmp6.(Option__int32_Some)._0
            var extra__5 int32 = x7
            var t33 string = _goml_m_inherent_i_int32_i_int32_i_to__string(extra__5)
            var t34 string = "extra=" + t33
            jp27 = t34
        default:
            panic("non-exhaustive match")
        }
        var label__6 string = jp27
        var t28 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t29 string = "value=" + t28
        var t30 string = t29 + ","
        var t31 string = t30 + label__6
        var t32 Option__string = Option__string_Some{
            _0: t31,
        }
        retv23 = t32
        return retv23
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__7 Option__string) string {
    var retv36 string
    var jp38 string
    switch opt__7.(type) {
    case Option__string_None:
        jp38 = "none"
    case Option__string_Some:
        var x8 string = opt__7.(Option__string_Some)._0
        var value__8 string = x8
        var t39 string = "some=" + value__8
        jp38 = t39
    default:
        panic("non-exhaustive match")
    }
    retv36 = jp38
    return retv36
}

func main0() struct{} {
    var t41 Option__string = mixed(true, true)
    var t42 string = show(t41)
    println__T_string(t42)
    var t43 Option__string = mixed(true, false)
    var t44 string = show(t43)
    println__T_string(t44)
    var t45 Option__string = mixed(false, true)
    var t46 string = show(t45)
    println__T_string(t46)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv48 string
    var t49 string = _goml_runtime_core_int32_to_string(self__2)
    retv48 = t49
    return retv48
}

func println__T_string(value__1 string) struct{} {
    var t51 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t51)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv54 string
    retv54 = self__9
    return retv54
}

func main() {
    main0()
}
