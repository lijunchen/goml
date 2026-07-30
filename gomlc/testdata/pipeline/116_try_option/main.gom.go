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
    var retv114 Option__int32
    var jp116 Option__int32
    if flag__0 {
        var t117 Option__int32 = Some{
            _0: 4,
        }
        jp116 = t117
    } else {
        jp116 = None{}
    }
    retv114 = jp116
    return retv114
}

func add(a__1 int32, b__2 int32) int32 {
    var retv119 int32
    var t120 int32 = a__1 + b__2
    retv119 = t120
    return retv119
}

func plus_two(flag__3 bool) Option__int32 {
    var retv122 Option__int32
    var mtmp108 Option__int32 = maybe_value(flag__3)
    var jp124 int32
    switch mtmp108.(type) {
    case None:
        retv122 = None{}
        return retv122
    case Some:
        var x109 int32 = mtmp108.(Some)._0
        var try_value__15 int32 = x109
        jp124 = try_value__15
        var t125 int32 = add(jp124, 2)
        var t126 Option__int32 = Some{
            _0: t125,
        }
        retv122 = t126
        return retv122
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__int32) string {
    var retv128 string
    var jp130 string
    switch opt__4.(type) {
    case None:
        jp130 = "none"
    case Some:
        var x110 int32 = opt__4.(Some)._0
        var value__5 int32 = x110
        var t131 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t132 string = "some=" + t131
        jp130 = t132
    default:
        panic("non-exhaustive match")
    }
    retv128 = jp130
    return retv128
}

func main0() struct{} {
    var t134 Option__int32 = plus_two(true)
    var t135 string = show(t134)
    println__T_string(t135)
    var t136 Option__int32 = plus_two(false)
    var t137 string = show(t136)
    println__T_string(t137)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv139 string
    var t140 string = _goml_runtime_core_int32_to_string(self__6)
    retv139 = t140
    return retv139
}

func println__T_string(value__1 string) struct{} {
    var t142 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t142)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv145 string
    retv145 = self__38
    return retv145
}

func main() {
    main0()
}
