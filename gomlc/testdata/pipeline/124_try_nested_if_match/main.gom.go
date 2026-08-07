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
    var jp188 int32
    if top__1 {
        switch mode__2 {
        case Take:
            var mtmp172 Option__int32
            if inner_flag__3 {
                var inline220 Option__int32 = Some{
                    _0: 8,
                }
                mtmp172 = inline220
            } else {
                mtmp172 = None{}
            }
            var jp193 int32
            switch mtmp172.(type) {
            case None:
                return None{}
            case Some:
                var x173 int32 = mtmp172.(Some)._0
                jp193 = x173
                var t194 int32 = jp193 + 1
                jp188 = t194
                var t189 Option__int32 = Some{
                    _0: jp188,
                }
                return t189
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp188 = 20
            var t189 Option__int32 = Some{
                _0: jp188,
            }
            return t189
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp174 Option__int32
        if inner_flag__3 {
            var inline222 Option__int32 = Some{
                _0: 8,
            }
            mtmp174 = inline222
        } else {
            mtmp174 = None{}
        }
        var jp196 int32
        switch mtmp174.(type) {
        case None:
            return None{}
        case Some:
            var x175 int32 = mtmp174.(Some)._0
            jp196 = x175
            var t197 int32 = jp196 + 2
            jp188 = t197
            var t189 Option__int32 = Some{
                _0: jp188,
            }
            return t189
        default:
            panic("non-exhaustive match")
        }
    }
}

func main0() struct{} {
    var t205 Option__int32 = nested(true, Take, true)
    var t206 string
    switch t205.(type) {
    case None:
        t206 = "none"
    case Some:
        var inline245 int32 = t205.(Some)._0
        var inline247 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline245)
        var inline248 string = "some=" + inline247
        t206 = inline248
    default:
        panic("non-exhaustive match")
    }
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline242)
    var t207 Option__int32 = nested(true, Skip, false)
    var t208 string
    switch t207.(type) {
    case None:
        t208 = "none"
    case Some:
        var inline237 int32 = t207.(Some)._0
        var inline239 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline237)
        var inline240 string = "some=" + inline239
        t208 = inline240
    default:
        panic("non-exhaustive match")
    }
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline234)
    var t209 Option__int32 = nested(false, Take, false)
    var t210 string
    switch t209.(type) {
    case None:
        t210 = "none"
    case Some:
        var inline229 int32 = t209.(Some)._0
        var inline231 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline229)
        var inline232 string = "some=" + inline231
        t210 = inline232
    default:
        panic("non-exhaustive match")
    }
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline226)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t213 string = _goml_runtime_core_int32_to_string(self__35)
    return t213
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
