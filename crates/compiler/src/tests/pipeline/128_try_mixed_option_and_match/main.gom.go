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
    var retv31 Option__int32
    var jp33 Option__int32
    if flag__0 {
        var t34 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        jp33 = t34
    } else {
        jp33 = Option__int32_None{}
    }
    retv31 = jp33
    return retv31
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var retv36 Option__int32
    var jp38 Option__int32
    if flag__1 {
        var t39 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        jp38 = t39
    } else {
        jp38 = Option__int32_None{}
    }
    retv36 = jp38
    return retv36
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var retv41 Option__string
    var mtmp22 Option__int32 = maybe_primary(primary__2)
    var jp43 int32
    switch mtmp22.(type) {
    case Option__int32_None:
        retv41 = Option__string_None{}
        return retv41
    case Option__int32_Some:
        var x23 int32 = mtmp22.(Option__int32_Some)._0
        var try_value__18 int32 = x23
        jp43 = try_value__18
        var value__4 int32 = jp43
        var mtmp24 Option__int32 = maybe_secondary(secondary__3)
        var jp45 string
        switch mtmp24.(type) {
        case Option__int32_None:
            jp45 = "extra=none"
        case Option__int32_Some:
            var x25 int32 = mtmp24.(Option__int32_Some)._0
            var extra__5 int32 = x25
            var t51 string = _goml_m_inherent_i_int32_i_int32_i_to__string(extra__5)
            var t52 string = "extra=" + t51
            jp45 = t52
        default:
            panic("non-exhaustive match")
        }
        var label__6 string = jp45
        var t46 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t47 string = "value=" + t46
        var t48 string = t47 + ","
        var t49 string = t48 + label__6
        var t50 Option__string = Option__string_Some{
            _0: t49,
        }
        retv41 = t50
        return retv41
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__7 Option__string) string {
    var retv54 string
    var jp56 string
    switch opt__7.(type) {
    case Option__string_None:
        jp56 = "none"
    case Option__string_Some:
        var x26 string = opt__7.(Option__string_Some)._0
        var value__8 string = x26
        var t57 string = "some=" + value__8
        jp56 = t57
    default:
        panic("non-exhaustive match")
    }
    retv54 = jp56
    return retv54
}

func main0() struct{} {
    var t59 Option__string = mixed(true, true)
    var t60 string = show(t59)
    println__T_string(t60)
    var t61 Option__string = mixed(true, false)
    var t62 string = show(t61)
    println__T_string(t62)
    var t63 Option__string = mixed(false, true)
    var t64 string = show(t63)
    println__T_string(t64)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv66 string
    var t67 string = _goml_runtime_core_int32_to_string(self__2)
    retv66 = t67
    return retv66
}

func println__T_string(value__1 string) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t69)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv72 string
    retv72 = self__9
    return retv72
}

func main() {
    main0()
}
