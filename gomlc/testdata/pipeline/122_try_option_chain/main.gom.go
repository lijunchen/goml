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
    var retv163 Option__int32
    var jp165 Option__int32
    if flag__0 {
        var t166 Option__int32 = Some{
            _0: 3,
        }
        jp165 = t166
    } else {
        jp165 = None{}
    }
    retv163 = jp165
    return retv163
}

func maybe_double(value__1 int32) Option__int32 {
    var retv168 Option__int32
    var t171 bool = value__1 > 0
    var jp170 Option__int32
    if t171 {
        var t172 int32 = value__1 * 2
        var t173 Option__int32 = Some{
            _0: t172,
        }
        jp170 = t173
    } else {
        jp170 = None{}
    }
    retv168 = jp170
    return retv168
}

func maybe_total(flag__2 bool) Option__int32 {
    var retv175 Option__int32
    var mtmp155 Option__int32 = maybe_seed(flag__2)
    var jp177 int32
    switch mtmp155.(type) {
    case None:
        retv175 = None{}
        return retv175
    case Some:
        var x156 int32 = mtmp155.(Some)._0
        var try_value__22 int32 = x156
        jp177 = try_value__22
        var a__3 int32 = jp177
        var mtmp157 Option__int32 = maybe_double(a__3)
        var jp179 int32
        switch mtmp157.(type) {
        case None:
            retv175 = None{}
            return retv175
        case Some:
            var x158 int32 = mtmp157.(Some)._0
            var try_value__26 int32 = x158
            jp179 = try_value__26
            var b__4 int32 = jp179
            var t180 int32 = a__3 + b__4
            var t181 Option__int32 = Some{
                _0: t180,
            }
            retv175 = t181
            return retv175
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    var retv183 string
    var jp185 string
    switch opt__5.(type) {
    case None:
        jp185 = "none"
    case Some:
        var x159 int32 = opt__5.(Some)._0
        var value__6 int32 = x159
        var t186 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t187 string = "some=" + t186
        jp185 = t187
    default:
        panic("non-exhaustive match")
    }
    retv183 = jp185
    return retv183
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
    var retv194 string
    var t195 string = _goml_runtime_core_int32_to_string(self__6)
    retv194 = t195
    return retv194
}

func println__T_string(value__1 string) struct{} {
    var t197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t197)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv200 string
    retv200 = self__38
    return retv200
}

func main() {
    main0()
}
