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

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var mtmp172 Option__int32
    if primary__2 {
        var inline230 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        mtmp172 = inline230
    } else {
        mtmp172 = Option__int32_None{}
    }
    var jp193 int32
    switch mtmp172.(type) {
    case Option__int32_None:
        return Option__string_None{}
    case Option__int32_Some:
        var x173 int32 = mtmp172.(Option__int32_Some)._0
        jp193 = x173
        var mtmp174 Option__int32
        if secondary__3 {
            var inline228 Option__int32 = Option__int32_Some{
                _0: 9,
            }
            mtmp174 = inline228
        } else {
            mtmp174 = Option__int32_None{}
        }
        var jp195 string
        switch mtmp174.(type) {
        case Option__int32_None:
            jp195 = "extra=none"
        case Option__int32_Some:
            var x175 int32 = mtmp174.(Option__int32_Some)._0
            var t201 string
            var inline224 string = _goml_runtime_core_int32_to_string(x175)
            t201 = inline224
            var t202 string = "extra=" + t201
            jp195 = t202
        default:
            panic("non-exhaustive match")
        }
        var t196 string
        var inline226 string = _goml_runtime_core_int32_to_string(jp193)
        t196 = inline226
        var t197 string = "value=" + t196
        var t198 string = t197 + ","
        var t199 string = t198 + jp195
        var t200 Option__string = Option__string_Some{
            _0: t199,
        }
        return t200
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t209 Option__string = mixed(true, true)
    var t210 string
    switch t209.(type) {
    case Option__string_None:
        t210 = "none"
    case Option__string_Some:
        var inline249 string = t209.(Option__string_Some)._0
        var inline251 string = "some=" + inline249
        t210 = inline251
    default:
        panic("non-exhaustive match")
    }
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline246)
    var t211 Option__string = mixed(true, false)
    var t212 string
    switch t211.(type) {
    case Option__string_None:
        t212 = "none"
    case Option__string_Some:
        var inline242 string = t211.(Option__string_Some)._0
        var inline244 string = "some=" + inline242
        t212 = inline244
    default:
        panic("non-exhaustive match")
    }
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline239)
    var t213 Option__string = mixed(false, true)
    var t214 string
    switch t213.(type) {
    case Option__string_None:
        t214 = "none"
    case Option__string_Some:
        var inline235 string = t213.(Option__string_Some)._0
        var inline237 string = "some=" + inline235
        t214 = inline237
    default:
        panic("non-exhaustive match")
    }
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline232)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
