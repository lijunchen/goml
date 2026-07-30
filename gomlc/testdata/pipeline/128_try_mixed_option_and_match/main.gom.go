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
    var retv77 Option__int32
    var jp79 Option__int32
    if flag__0 {
        var t80 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        jp79 = t80
    } else {
        jp79 = Option__int32_None{}
    }
    retv77 = jp79
    return retv77
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var retv82 Option__int32
    var jp84 Option__int32
    if flag__1 {
        var t85 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        jp84 = t85
    } else {
        jp84 = Option__int32_None{}
    }
    retv82 = jp84
    return retv82
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var retv87 Option__string
    var mtmp68 Option__int32 = maybe_primary(primary__2)
    var jp89 int32
    switch mtmp68.(type) {
    case Option__int32_None:
        retv87 = Option__string_None{}
        return retv87
    case Option__int32_Some:
        var x69 int32 = mtmp68.(Option__int32_Some)._0
        var try_value__18 int32 = x69
        jp89 = try_value__18
        var value__4 int32 = jp89
        var mtmp70 Option__int32 = maybe_secondary(secondary__3)
        var jp91 string
        switch mtmp70.(type) {
        case Option__int32_None:
            jp91 = "extra=none"
        case Option__int32_Some:
            var x71 int32 = mtmp70.(Option__int32_Some)._0
            var extra__5 int32 = x71
            var t97 string = _goml_m_inherent_i_int32_i_int32_i_to__string(extra__5)
            var t98 string = "extra=" + t97
            jp91 = t98
        default:
            panic("non-exhaustive match")
        }
        var label__6 string = jp91
        var t92 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t93 string = "value=" + t92
        var t94 string = t93 + ","
        var t95 string = t94 + label__6
        var t96 Option__string = Option__string_Some{
            _0: t95,
        }
        retv87 = t96
        return retv87
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__7 Option__string) string {
    var retv100 string
    var jp102 string
    switch opt__7.(type) {
    case Option__string_None:
        jp102 = "none"
    case Option__string_Some:
        var x72 string = opt__7.(Option__string_Some)._0
        var value__8 string = x72
        var t103 string = "some=" + value__8
        jp102 = t103
    default:
        panic("non-exhaustive match")
    }
    retv100 = jp102
    return retv100
}

func main0() struct{} {
    var t105 Option__string = mixed(true, true)
    var t106 string = show(t105)
    println__T_string(t106)
    var t107 Option__string = mixed(true, false)
    var t108 string = show(t107)
    println__T_string(t108)
    var t109 Option__string = mixed(false, true)
    var t110 string = show(t109)
    println__T_string(t110)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv112 string
    var t113 string = _goml_runtime_core_int32_to_string(self__6)
    retv112 = t113
    return retv112
}

func println__T_string(value__1 string) struct{} {
    var t115 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t115)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv118 string
    retv118 = self__38
    return retv118
}

func main() {
    main0()
}
