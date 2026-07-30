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
    var retv113 Option__int32
    var jp115 Option__int32
    if flag__0 {
        var t116 Option__int32 = Some{
            _0: 41,
        }
        jp115 = t116
    } else {
        jp115 = None{}
    }
    retv113 = jp115
    return retv113
}

func compute(flag__1 bool) Option__int32 {
    var retv118 Option__int32
    var mtmp108 Option__int32 = maybe_value(flag__1)
    var jp120 int32
    switch mtmp108.(type) {
    case Some:
        var x109 int32 = mtmp108.(Some)._0
        var try_value__11 int32 = x109
        jp120 = try_value__11
        var value__2 int32 = jp120
        var t121 int32 = value__2 + 1
        var t122 Option__int32 = Some{
            _0: t121,
        }
        retv118 = t122
        return retv118
    case None:
        retv118 = None{}
        return retv118
    default:
        panic("non-exhaustive match")
    }
}

func show(value__3 Option__int32) string {
    var retv124 string
    var jp126 string
    switch value__3.(type) {
    case Some:
        var x110 int32 = value__3.(Some)._0
        var value__4 int32 = x110
        var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        jp126 = t127
    case None:
        jp126 = "none"
    default:
        panic("non-exhaustive match")
    }
    retv124 = jp126
    return retv124
}

func main0() struct{} {
    var t129 Option__int32 = compute(true)
    var t130 string = show(t129)
    println__T_string(t130)
    var t131 Option__int32 = compute(false)
    var t132 string = show(t131)
    println__T_string(t132)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv135 string
    var t136 string = _goml_runtime_core_int32_to_string(self__6)
    retv135 = t136
    return retv135
}

func println__T_string(value__1 string) struct{} {
    var t138 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t138)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv141 string
    retv141 = self__38
    return retv141
}

func main() {
    main0()
}
