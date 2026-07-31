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
    var retv157 Option__int32
    var jp159 Option__int32
    if flag__0 {
        var t160 Option__int32 = Some{
            _0: 41,
        }
        jp159 = t160
    } else {
        jp159 = None{}
    }
    retv157 = jp159
    return retv157
}

func compute(flag__1 bool) Option__int32 {
    var retv162 Option__int32
    var mtmp152 Option__int32 = maybe_value(flag__1)
    var jp164 int32
    switch mtmp152.(type) {
    case Some:
        var x153 int32 = mtmp152.(Some)._0
        var try_value__11 int32 = x153
        jp164 = try_value__11
        var value__2 int32 = jp164
        var t165 int32 = value__2 + 1
        var t166 Option__int32 = Some{
            _0: t165,
        }
        retv162 = t166
        return retv162
    case None:
        retv162 = None{}
        return retv162
    default:
        panic("non-exhaustive match")
    }
}

func show(value__3 Option__int32) string {
    var retv168 string
    var jp170 string
    switch value__3.(type) {
    case Some:
        var x154 int32 = value__3.(Some)._0
        var value__4 int32 = x154
        var t171 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        jp170 = t171
    case None:
        jp170 = "none"
    default:
        panic("non-exhaustive match")
    }
    retv168 = jp170
    return retv168
}

func main0() struct{} {
    var t173 Option__int32 = compute(true)
    var t174 string = show(t173)
    println__T_string(t174)
    var t175 Option__int32 = compute(false)
    var t176 string = show(t175)
    println__T_string(t176)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv179 string
    var t180 string = _goml_runtime_core_int32_to_string(self__6)
    retv179 = t180
    return retv179
}

func println__T_string(value__1 string) struct{} {
    var t182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t182)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv185 string
    retv185 = self__38
    return retv185
}

func main() {
    main0()
}
