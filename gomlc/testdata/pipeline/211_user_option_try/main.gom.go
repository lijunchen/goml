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
    var retv69 Option__int32
    var jp71 Option__int32
    if flag__0 {
        var t72 Option__int32 = Some{
            _0: 41,
        }
        jp71 = t72
    } else {
        jp71 = None{}
    }
    retv69 = jp71
    return retv69
}

func compute(flag__1 bool) Option__int32 {
    var retv74 Option__int32
    var mtmp64 Option__int32 = maybe_value(flag__1)
    var jp76 int32
    switch mtmp64.(type) {
    case Some:
        var x65 int32 = mtmp64.(Some)._0
        var try_value__11 int32 = x65
        jp76 = try_value__11
        var value__2 int32 = jp76
        var t77 int32 = value__2 + 1
        var t78 Option__int32 = Some{
            _0: t77,
        }
        retv74 = t78
        return retv74
    case None:
        retv74 = None{}
        return retv74
    default:
        panic("non-exhaustive match")
    }
}

func show(value__3 Option__int32) string {
    var retv80 string
    var jp82 string
    switch value__3.(type) {
    case Some:
        var x66 int32 = value__3.(Some)._0
        var value__4 int32 = x66
        var t83 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        jp82 = t83
    case None:
        jp82 = "none"
    default:
        panic("non-exhaustive match")
    }
    retv80 = jp82
    return retv80
}

func main0() struct{} {
    var t85 Option__int32 = compute(true)
    var t86 string = show(t85)
    println__T_string(t86)
    var t87 Option__int32 = compute(false)
    var t88 string = show(t87)
    println__T_string(t88)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv91 string
    var t92 string = _goml_runtime_core_int32_to_string(self__6)
    retv91 = t92
    return retv91
}

func println__T_string(value__1 string) struct{} {
    var t94 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t94)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv97 string
    retv97 = self__38
    return retv97
}

func main() {
    main0()
}
