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
    var retv67 Option__int32
    var jp69 Option__int32
    if flag__0 {
        var t70 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        jp69 = t70
    } else {
        jp69 = Option__int32_None{}
    }
    retv67 = jp69
    return retv67
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var retv72 Option__int32
    var jp74 Option__int32
    if flag__1 {
        var t75 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        jp74 = t75
    } else {
        jp74 = Option__int32_None{}
    }
    retv72 = jp74
    return retv72
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var retv77 Option__string
    var mtmp58 Option__int32 = maybe_primary(primary__2)
    var jp79 int32
    switch mtmp58.(type) {
    case Option__int32_None:
        retv77 = Option__string_None{}
        return retv77
    case Option__int32_Some:
        var x59 int32 = mtmp58.(Option__int32_Some)._0
        var try_value__18 int32 = x59
        jp79 = try_value__18
        var value__4 int32 = jp79
        var mtmp60 Option__int32 = maybe_secondary(secondary__3)
        var jp81 string
        switch mtmp60.(type) {
        case Option__int32_None:
            jp81 = "extra=none"
        case Option__int32_Some:
            var x61 int32 = mtmp60.(Option__int32_Some)._0
            var extra__5 int32 = x61
            var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(extra__5)
            var t88 string = "extra=" + t87
            jp81 = t88
        default:
            panic("non-exhaustive match")
        }
        var label__6 string = jp81
        var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t83 string = "value=" + t82
        var t84 string = t83 + ","
        var t85 string = t84 + label__6
        var t86 Option__string = Option__string_Some{
            _0: t85,
        }
        retv77 = t86
        return retv77
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__7 Option__string) string {
    var retv90 string
    var jp92 string
    switch opt__7.(type) {
    case Option__string_None:
        jp92 = "none"
    case Option__string_Some:
        var x62 string = opt__7.(Option__string_Some)._0
        var value__8 string = x62
        var t93 string = "some=" + value__8
        jp92 = t93
    default:
        panic("non-exhaustive match")
    }
    retv90 = jp92
    return retv90
}

func main0() struct{} {
    var t95 Option__string = mixed(true, true)
    var t96 string = show(t95)
    println__T_string(t96)
    var t97 Option__string = mixed(true, false)
    var t98 string = show(t97)
    println__T_string(t98)
    var t99 Option__string = mixed(false, true)
    var t100 string = show(t99)
    println__T_string(t100)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv102 string
    var t103 string = _goml_runtime_core_int32_to_string(self__2)
    retv102 = t103
    return retv102
}

func println__T_string(value__1 string) struct{} {
    var t105 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t105)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv108 string
    retv108 = self__34
    return retv108
}

func main() {
    main0()
}
