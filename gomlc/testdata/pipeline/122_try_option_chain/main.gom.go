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

func maybe_total(flag__2 bool) Option__int32 {
    var mtmp155 Option__int32
    if flag__2 {
        var inline206 Option__int32 = Some{
            _0: 3,
        }
        mtmp155 = inline206
    } else {
        mtmp155 = None{}
    }
    var jp177 int32
    switch mtmp155.(type) {
    case None:
        return None{}
    case Some:
        var x156 int32 = mtmp155.(Some)._0
        jp177 = x156
        var mtmp157 Option__int32
        var inline202 bool = jp177 > 0
        if inline202 {
            var inline203 int32 = jp177 * 2
            var inline204 Option__int32 = Some{
                _0: inline203,
            }
            mtmp157 = inline204
        } else {
            mtmp157 = None{}
        }
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

func main0() struct{} {
    var t189 Option__int32 = maybe_total(true)
    var t190 string
    switch t189.(type) {
    case None:
        t190 = "none"
    case Some:
        var inline221 int32 = t189.(Some)._0
        var inline223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline221)
        var inline224 string = "some=" + inline223
        t190 = inline224
    default:
        panic("non-exhaustive match")
    }
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline218)
    var t191 Option__int32 = maybe_total(false)
    var t192 string
    switch t191.(type) {
    case None:
        t192 = "none"
    case Some:
        var inline213 int32 = t191.(Some)._0
        var inline215 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline213)
        var inline216 string = "some=" + inline215
        t192 = inline216
    default:
        panic("non-exhaustive match")
    }
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t195 string = _goml_runtime_core_int32_to_string(self__6)
    return t195
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
