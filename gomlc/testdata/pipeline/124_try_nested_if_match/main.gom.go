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

type Mode int32

const (
    Take Mode = 0
    Skip Mode = 1
)

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var jp171 int32
    if top__1 {
        switch mode__2 {
        case Take:
            var mtmp155 Option__int32
            if inner_flag__3 {
                var inline203 Option__int32 = Some{
                    _0: 8,
                }
                mtmp155 = inline203
            } else {
                mtmp155 = None{}
            }
            var jp176 int32
            switch mtmp155.(type) {
            case None:
                return None{}
            case Some:
                var x156 int32 = mtmp155.(Some)._0
                jp176 = x156
                var t177 int32 = jp176 + 1
                jp171 = t177
                var t172 Option__int32 = Some{
                    _0: jp171,
                }
                return t172
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp171 = 20
            var t172 Option__int32 = Some{
                _0: jp171,
            }
            return t172
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp157 Option__int32
        if inner_flag__3 {
            var inline205 Option__int32 = Some{
                _0: 8,
            }
            mtmp157 = inline205
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
            var t180 int32 = jp179 + 2
            jp171 = t180
            var t172 Option__int32 = Some{
                _0: jp171,
            }
            return t172
        default:
            panic("non-exhaustive match")
        }
    }
}

func main0() struct{} {
    var t188 Option__int32 = nested(true, Take, true)
    var t189 string
    switch t188.(type) {
    case None:
        t189 = "none"
    case Some:
        var inline228 int32 = t188.(Some)._0
        var inline230 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline228)
        var inline231 string = "some=" + inline230
        t189 = inline231
    default:
        panic("non-exhaustive match")
    }
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline225)
    var t190 Option__int32 = nested(true, Skip, false)
    var t191 string
    switch t190.(type) {
    case None:
        t191 = "none"
    case Some:
        var inline220 int32 = t190.(Some)._0
        var inline222 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline220)
        var inline223 string = "some=" + inline222
        t191 = inline223
    default:
        panic("non-exhaustive match")
    }
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline217)
    var t192 Option__int32 = nested(false, Take, false)
    var t193 string
    switch t192.(type) {
    case None:
        t193 = "none"
    case Some:
        var inline212 int32 = t192.(Some)._0
        var inline214 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline212)
        var inline215 string = "some=" + inline214
        t193 = inline215
    default:
        panic("non-exhaustive match")
    }
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t196 string = _goml_runtime_core_int32_to_string(self__6)
    return t196
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
