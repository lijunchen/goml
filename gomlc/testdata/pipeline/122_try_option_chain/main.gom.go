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
    if flag__0 {
        var t166 Option__int32 = Some{
            _0: 3,
        }
        return t166
    } else {
        return None{}
    }
}

func maybe_double(value__1 int32) Option__int32 {
    var t171 bool = value__1 > 0
    if t171 {
        var t172 int32 = value__1 * 2
        var t173 Option__int32 = Some{
            _0: t172,
        }
        return t173
    } else {
        return None{}
    }
}

func maybe_total(flag__2 bool) Option__int32 {
    var mtmp155 Option__int32 = maybe_seed(flag__2)
    var jp177 int32
    switch mtmp155.(type) {
    case None:
        return None{}
    case Some:
        var x156 int32 = mtmp155.(Some)._0
        jp177 = x156
        var mtmp157 Option__int32 = maybe_double(jp177)
        var jp179 int32
        switch mtmp157.(type) {
        case None:
            return None{}
        case Some:
            var x158 int32 = mtmp157.(Some)._0
            jp179 = x158
            var t180 int32 = jp177 + jp179
            var t181 Option__int32 = Some{
                _0: t180,
            }
            return t181
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    switch opt__5.(type) {
    case None:
        return "none"
    case Some:
        var x159 int32 = opt__5.(Some)._0
        var t186 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x159)
        var t187 string = "some=" + t186
        return t187
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t189 Option__int32 = maybe_total(true)
    var t190 string = show(t189)
    println__T_string(t190)
    var t191 Option__int32 = maybe_total(false)
    var t192 string = show(t191)
    println__T_string(t192)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t195 string = _goml_runtime_core_int32_to_string(self__6)
    return t195
}

func println__T_string(value__1 string) struct{} {
    var t197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t197)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
