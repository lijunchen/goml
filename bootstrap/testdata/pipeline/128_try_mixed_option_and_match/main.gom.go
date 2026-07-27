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
    var retv73 Option__int32
    var jp75 Option__int32
    if flag__0 {
        var t76 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        jp75 = t76
    } else {
        jp75 = Option__int32_None{}
    }
    retv73 = jp75
    return retv73
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var retv78 Option__int32
    var jp80 Option__int32
    if flag__1 {
        var t81 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        jp80 = t81
    } else {
        jp80 = Option__int32_None{}
    }
    retv78 = jp80
    return retv78
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var retv83 Option__string
    var mtmp64 Option__int32 = maybe_primary(primary__2)
    var jp85 int32
    switch mtmp64.(type) {
    case Option__int32_None:
        retv83 = Option__string_None{}
        return retv83
    case Option__int32_Some:
        var x65 int32 = mtmp64.(Option__int32_Some)._0
        var try_value__18 int32 = x65
        jp85 = try_value__18
        var value__4 int32 = jp85
        var mtmp66 Option__int32 = maybe_secondary(secondary__3)
        var jp87 string
        switch mtmp66.(type) {
        case Option__int32_None:
            jp87 = "extra=none"
        case Option__int32_Some:
            var x67 int32 = mtmp66.(Option__int32_Some)._0
            var extra__5 int32 = x67
            var t93 string = _goml_m_inherent_i_int32_i_int32_i_to__string(extra__5)
            var t94 string = "extra=" + t93
            jp87 = t94
        default:
            panic("non-exhaustive match")
        }
        var label__6 string = jp87
        var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t89 string = "value=" + t88
        var t90 string = t89 + ","
        var t91 string = t90 + label__6
        var t92 Option__string = Option__string_Some{
            _0: t91,
        }
        retv83 = t92
        return retv83
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__7 Option__string) string {
    var retv96 string
    var jp98 string
    switch opt__7.(type) {
    case Option__string_None:
        jp98 = "none"
    case Option__string_Some:
        var x68 string = opt__7.(Option__string_Some)._0
        var value__8 string = x68
        var t99 string = "some=" + value__8
        jp98 = t99
    default:
        panic("non-exhaustive match")
    }
    retv96 = jp98
    return retv96
}

func main0() struct{} {
    var t101 Option__string = mixed(true, true)
    var t102 string = show(t101)
    println__T_string(t102)
    var t103 Option__string = mixed(true, false)
    var t104 string = show(t103)
    println__T_string(t104)
    var t105 Option__string = mixed(false, true)
    var t106 string = show(t105)
    println__T_string(t106)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv108 string
    var t109 string = _goml_runtime_core_int32_to_string(self__6)
    retv108 = t109
    return retv108
}

func println__T_string(value__1 string) struct{} {
    var t111 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t111)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv114 string
    retv114 = self__38
    return retv114
}

func main() {
    main0()
}
