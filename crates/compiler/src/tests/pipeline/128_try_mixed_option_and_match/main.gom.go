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
    var retv16 Option__int32
    var jp18 Option__int32
    if flag__0 {
        var t19 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        jp18 = t19
    } else {
        jp18 = Option__int32_None{}
    }
    retv16 = jp18
    return retv16
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var retv21 Option__int32
    var jp23 Option__int32
    if flag__1 {
        var t24 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        jp23 = t24
    } else {
        jp23 = Option__int32_None{}
    }
    retv21 = jp23
    return retv21
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var retv26 Option__string
    var mtmp7 Option__int32 = maybe_primary(primary__2)
    var jp28 int32
    switch mtmp7.(type) {
    case Option__int32_None:
        retv26 = Option__string_None{}
        return retv26
    case Option__int32_Some:
        var x8 int32 = mtmp7.(Option__int32_Some)._0
        var try_value__18 int32 = x8
        jp28 = try_value__18
        var value__4 int32 = jp28
        var mtmp9 Option__int32 = maybe_secondary(secondary__3)
        var jp30 string
        switch mtmp9.(type) {
        case Option__int32_None:
            jp30 = "extra=none"
        case Option__int32_Some:
            var x10 int32 = mtmp9.(Option__int32_Some)._0
            var extra__5 int32 = x10
            var t36 string = _goml_m_inherent_i_int32_i_int32_i_to__string(extra__5)
            var t37 string = "extra=" + t36
            jp30 = t37
        default:
            panic("non-exhaustive match")
        }
        var label__6 string = jp30
        var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t32 string = "value=" + t31
        var t33 string = t32 + ","
        var t34 string = t33 + label__6
        var t35 Option__string = Option__string_Some{
            _0: t34,
        }
        retv26 = t35
        return retv26
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__7 Option__string) string {
    var retv39 string
    var jp41 string
    switch opt__7.(type) {
    case Option__string_None:
        jp41 = "none"
    case Option__string_Some:
        var x11 string = opt__7.(Option__string_Some)._0
        var value__8 string = x11
        var t42 string = "some=" + value__8
        jp41 = t42
    default:
        panic("non-exhaustive match")
    }
    retv39 = jp41
    return retv39
}

func main0() struct{} {
    var t44 Option__string = mixed(true, true)
    var t45 string = show(t44)
    println__T_string(t45)
    var t46 Option__string = mixed(true, false)
    var t47 string = show(t46)
    println__T_string(t47)
    var t48 Option__string = mixed(false, true)
    var t49 string = show(t48)
    println__T_string(t49)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv51 string
    var t52 string = _goml_runtime_core_int32_to_string(self__2)
    retv51 = t52
    return retv51
}

func println__T_string(value__1 string) struct{} {
    var t54 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t54)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv57 string
    retv57 = self__9
    return retv57
}

func main() {
    main0()
}
