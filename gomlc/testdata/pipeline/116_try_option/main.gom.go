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
    var retv74 Option__int32
    var jp76 Option__int32
    if flag__0 {
        var t77 Option__int32 = Some{
            _0: 4,
        }
        jp76 = t77
    } else {
        jp76 = None{}
    }
    retv74 = jp76
    return retv74
}

func add(a__1 int32, b__2 int32) int32 {
    var retv79 int32
    var t80 int32 = a__1 + b__2
    retv79 = t80
    return retv79
}

func plus_two(flag__3 bool) Option__int32 {
    var retv82 Option__int32
    var mtmp68 Option__int32 = maybe_value(flag__3)
    var jp84 int32
    switch mtmp68.(type) {
    case None:
        retv82 = None{}
        return retv82
    case Some:
        var x69 int32 = mtmp68.(Some)._0
        var try_value__15 int32 = x69
        jp84 = try_value__15
        var t85 int32 = add(jp84, 2)
        var t86 Option__int32 = Some{
            _0: t85,
        }
        retv82 = t86
        return retv82
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__int32) string {
    var retv88 string
    var jp90 string
    switch opt__4.(type) {
    case None:
        jp90 = "none"
    case Some:
        var x70 int32 = opt__4.(Some)._0
        var value__5 int32 = x70
        var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t92 string = "some=" + t91
        jp90 = t92
    default:
        panic("non-exhaustive match")
    }
    retv88 = jp90
    return retv88
}

func main0() struct{} {
    var t94 Option__int32 = plus_two(true)
    var t95 string = show(t94)
    println__T_string(t95)
    var t96 Option__int32 = plus_two(false)
    var t97 string = show(t96)
    println__T_string(t97)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv99 string
    var t100 string = _goml_runtime_core_int32_to_string(self__6)
    retv99 = t100
    return retv99
}

func println__T_string(value__1 string) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t102)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv105 string
    retv105 = self__38
    return retv105
}

func main() {
    main0()
}
