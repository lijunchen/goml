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
    var mtmp172 Option__int32
    if flag__2 {
        var inline223 Option__int32 = Some{
            _0: 3,
        }
        mtmp172 = inline223
    } else {
        mtmp172 = None{}
    }
    var jp194 int32
    switch mtmp172.(type) {
    case None:
        return None{}
    case Some:
        var x173 int32 = mtmp172.(Some)._0
        jp194 = x173
        var mtmp174 Option__int32
        var inline219 bool = jp194 > 0
        if inline219 {
            var inline220 int32 = jp194 * 2
            var inline221 Option__int32 = Some{
                _0: inline220,
            }
            mtmp174 = inline221
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
            var t197 int32 = jp194 + jp196
            var t198 Option__int32 = Some{
                _0: t197,
            }
            return t198
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t206 Option__int32 = maybe_total(true)
    var t207 string
    switch t206.(type) {
    case None:
        t207 = "none"
    case Some:
        var inline238 int32 = t206.(Some)._0
        var inline240 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline238)
        var inline241 string = "some=" + inline240
        t207 = inline241
    default:
        panic("non-exhaustive match")
    }
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline235)
    var t208 Option__int32 = maybe_total(false)
    var t209 string
    switch t208.(type) {
    case None:
        t209 = "none"
    case Some:
        var inline230 int32 = t208.(Some)._0
        var inline232 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline230)
        var inline233 string = "some=" + inline232
        t209 = inline233
    default:
        panic("non-exhaustive match")
    }
    var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline227)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t212 string = _goml_runtime_core_int32_to_string(self__35)
    return t212
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
