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

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

type None struct {}

func (_ None) isOption__int32() {}

func maybe_value(flag__0 bool) Option__int32 {
    var retv73 Option__int32
    var jp75 Option__int32
    if flag__0 {
        var t76 Option__int32 = Some{
            _0: 41,
        }
        jp75 = t76
    } else {
        jp75 = None{}
    }
    retv73 = jp75
    return retv73
}

func compute(flag__1 bool) Option__int32 {
    var retv78 Option__int32
    var mtmp68 Option__int32 = maybe_value(flag__1)
    var jp80 int32
    switch mtmp68.(type) {
    case Some:
        var x69 int32 = mtmp68.(Some)._0
        var try_value__11 int32 = x69
        jp80 = try_value__11
        var value__2 int32 = jp80
        var t81 int32 = value__2 + 1
        var t82 Option__int32 = Some{
            _0: t81,
        }
        retv78 = t82
        return retv78
    case None:
        retv78 = None{}
        return retv78
    default:
        panic("non-exhaustive match")
    }
}

func show(value__3 Option__int32) string {
    var retv84 string
    var jp86 string
    switch value__3.(type) {
    case Some:
        var x70 int32 = value__3.(Some)._0
        var value__4 int32 = x70
        var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        jp86 = t87
    case None:
        jp86 = "none"
    default:
        panic("non-exhaustive match")
    }
    retv84 = jp86
    return retv84
}

func main0() struct{} {
    var t89 Option__int32 = compute(true)
    var t90 string = show(t89)
    println__T_string(t90)
    var t91 Option__int32 = compute(false)
    var t92 string = show(t91)
    println__T_string(t92)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv95 string
    var t96 string = _goml_runtime_core_int32_to_string(self__6)
    retv95 = t96
    return retv95
}

func println__T_string(value__1 string) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv101 string
    retv101 = self__38
    return retv101
}

func main() {
    main0()
}
