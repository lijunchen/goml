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
    var retv70 Option__int32
    var jp72 Option__int32
    if flag__0 {
        var t73 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        jp72 = t73
    } else {
        jp72 = Option__int32_None{}
    }
    retv70 = jp72
    return retv70
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var retv75 Option__int32
    var jp77 Option__int32
    if flag__1 {
        var t78 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        jp77 = t78
    } else {
        jp77 = Option__int32_None{}
    }
    retv75 = jp77
    return retv75
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var retv80 Option__string
    var mtmp61 Option__int32 = maybe_primary(primary__2)
    var jp82 int32
    switch mtmp61.(type) {
    case Option__int32_None:
        retv80 = Option__string_None{}
        return retv80
    case Option__int32_Some:
        var x62 int32 = mtmp61.(Option__int32_Some)._0
        var try_value__18 int32 = x62
        jp82 = try_value__18
        var value__4 int32 = jp82
        var mtmp63 Option__int32 = maybe_secondary(secondary__3)
        var jp84 string
        switch mtmp63.(type) {
        case Option__int32_None:
            jp84 = "extra=none"
        case Option__int32_Some:
            var x64 int32 = mtmp63.(Option__int32_Some)._0
            var extra__5 int32 = x64
            var t90 string = _goml_m_inherent_i_int32_i_int32_i_to__string(extra__5)
            var t91 string = "extra=" + t90
            jp84 = t91
        default:
            panic("non-exhaustive match")
        }
        var label__6 string = jp84
        var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t86 string = "value=" + t85
        var t87 string = t86 + ","
        var t88 string = t87 + label__6
        var t89 Option__string = Option__string_Some{
            _0: t88,
        }
        retv80 = t89
        return retv80
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__7 Option__string) string {
    var retv93 string
    var jp95 string
    switch opt__7.(type) {
    case Option__string_None:
        jp95 = "none"
    case Option__string_Some:
        var x65 string = opt__7.(Option__string_Some)._0
        var value__8 string = x65
        var t96 string = "some=" + value__8
        jp95 = t96
    default:
        panic("non-exhaustive match")
    }
    retv93 = jp95
    return retv93
}

func main0() struct{} {
    var t98 Option__string = mixed(true, true)
    var t99 string = show(t98)
    println__T_string(t99)
    var t100 Option__string = mixed(true, false)
    var t101 string = show(t100)
    println__T_string(t101)
    var t102 Option__string = mixed(false, true)
    var t103 string = show(t102)
    println__T_string(t103)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv105 string
    var t106 string = _goml_runtime_core_int32_to_string(self__5)
    retv105 = t106
    return retv105
}

func println__T_string(value__1 string) struct{} {
    var t108 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t108)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv111 string
    retv111 = self__37
    return retv111
}

func main() {
    main0()
}
