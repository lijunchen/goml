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

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func maybe_value(flag__0 bool) Option__int32 {
    var retv70 Option__int32
    var jp72 Option__int32
    if flag__0 {
        var t73 Option__int32 = Some{
            _0: 4,
        }
        jp72 = t73
    } else {
        jp72 = None{}
    }
    retv70 = jp72
    return retv70
}

func add(a__1 int32, b__2 int32) int32 {
    var retv75 int32
    var t76 int32 = a__1 + b__2
    retv75 = t76
    return retv75
}

func plus_two(flag__3 bool) Option__int32 {
    var retv78 Option__int32
    var mtmp64 Option__int32 = maybe_value(flag__3)
    var jp80 int32
    switch mtmp64.(type) {
    case None:
        retv78 = None{}
        return retv78
    case Some:
        var x65 int32 = mtmp64.(Some)._0
        var try_value__15 int32 = x65
        jp80 = try_value__15
        var t81 int32 = add(jp80, 2)
        var t82 Option__int32 = Some{
            _0: t81,
        }
        retv78 = t82
        return retv78
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__int32) string {
    var retv84 string
    var jp86 string
    switch opt__4.(type) {
    case None:
        jp86 = "none"
    case Some:
        var x66 int32 = opt__4.(Some)._0
        var value__5 int32 = x66
        var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t88 string = "some=" + t87
        jp86 = t88
    default:
        panic("non-exhaustive match")
    }
    retv84 = jp86
    return retv84
}

func main0() struct{} {
    var t90 Option__int32 = plus_two(true)
    var t91 string = show(t90)
    println__T_string(t91)
    var t92 Option__int32 = plus_two(false)
    var t93 string = show(t92)
    println__T_string(t93)
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
