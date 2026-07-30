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

func maybe_seed(flag__0 bool) Option__int32 {
    var retv116 Option__int32
    var jp118 Option__int32
    if flag__0 {
        var t119 Option__int32 = Some{
            _0: 3,
        }
        jp118 = t119
    } else {
        jp118 = None{}
    }
    retv116 = jp118
    return retv116
}

func maybe_double(value__1 int32) Option__int32 {
    var retv121 Option__int32
    var t124 bool = value__1 > 0
    var jp123 Option__int32
    if t124 {
        var t125 int32 = value__1 * 2
        var t126 Option__int32 = Some{
            _0: t125,
        }
        jp123 = t126
    } else {
        jp123 = None{}
    }
    retv121 = jp123
    return retv121
}

func maybe_total(flag__2 bool) Option__int32 {
    var retv128 Option__int32
    var mtmp108 Option__int32 = maybe_seed(flag__2)
    var jp130 int32
    switch mtmp108.(type) {
    case None:
        retv128 = None{}
        return retv128
    case Some:
        var x109 int32 = mtmp108.(Some)._0
        var try_value__22 int32 = x109
        jp130 = try_value__22
        var a__3 int32 = jp130
        var mtmp110 Option__int32 = maybe_double(a__3)
        var jp132 int32
        switch mtmp110.(type) {
        case None:
            retv128 = None{}
            return retv128
        case Some:
            var x111 int32 = mtmp110.(Some)._0
            var try_value__26 int32 = x111
            jp132 = try_value__26
            var b__4 int32 = jp132
            var t133 int32 = a__3 + b__4
            var t134 Option__int32 = Some{
                _0: t133,
            }
            retv128 = t134
            return retv128
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    var retv136 string
    var jp138 string
    switch opt__5.(type) {
    case None:
        jp138 = "none"
    case Some:
        var x112 int32 = opt__5.(Some)._0
        var value__6 int32 = x112
        var t139 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t140 string = "some=" + t139
        jp138 = t140
    default:
        panic("non-exhaustive match")
    }
    retv136 = jp138
    return retv136
}

func main0() struct{} {
    var t142 Option__int32 = maybe_total(true)
    var t143 string = show(t142)
    println__T_string(t143)
    var t144 Option__int32 = maybe_total(false)
    var t145 string = show(t144)
    println__T_string(t145)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv147 string
    var t148 string = _goml_runtime_core_int32_to_string(self__6)
    retv147 = t148
    return retv147
}

func println__T_string(value__1 string) struct{} {
    var t150 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t150)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv153 string
    retv153 = self__38
    return retv153
}

func main() {
    main0()
}
