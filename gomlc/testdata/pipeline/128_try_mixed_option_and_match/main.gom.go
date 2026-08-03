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
    var mtmp177 Option__int32
    if primary__2 {
        var inline235 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        mtmp177 = inline235
    } else {
        mtmp177 = Option__int32_None{}
    }
    var jp198 int32
    switch mtmp177.(type) {
    case Option__int32_None:
        return Option__string_None{}
    case Option__int32_Some:
        var x178 int32 = mtmp177.(Option__int32_Some)._0
        jp198 = x178
        var mtmp179 Option__int32
        if secondary__3 {
            var inline233 Option__int32 = Option__int32_Some{
                _0: 9,
            }
            mtmp179 = inline233
        } else {
            mtmp179 = Option__int32_None{}
        }
        var jp200 string
        switch mtmp179.(type) {
        case Option__int32_None:
            jp200 = "extra=none"
        case Option__int32_Some:
            var x180 int32 = mtmp179.(Option__int32_Some)._0
            var t206 string
            var inline229 string = _goml_runtime_core_int32_to_string(x180)
            t206 = inline229
            var t207 string = "extra=" + t206
            jp200 = t207
        default:
            panic("non-exhaustive match")
        }
        var t201 string
        var inline231 string = _goml_runtime_core_int32_to_string(jp198)
        t201 = inline231
        var t202 string = "value=" + t201
        var t203 string = t202 + ","
        var t204 string = t203 + jp200
        var t205 Option__string = Option__string_Some{
            _0: t204,
        }
        return t205
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t214 Option__string = mixed(true, true)
    var t215 string
    switch t214.(type) {
    case Option__string_None:
        t215 = "none"
    case Option__string_Some:
        var inline254 string = t214.(Option__string_Some)._0
        var inline256 string = "some=" + inline254
        t215 = inline256
    default:
        panic("non-exhaustive match")
    }
    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline251)
    var t216 Option__string = mixed(true, false)
    var t217 string
    switch t216.(type) {
    case Option__string_None:
        t217 = "none"
    case Option__string_Some:
        var inline247 string = t216.(Option__string_Some)._0
        var inline249 string = "some=" + inline247
        t217 = inline249
    default:
        panic("non-exhaustive match")
    }
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline244)
    var t218 Option__string = mixed(false, true)
    var t219 string
    switch t218.(type) {
    case Option__string_None:
        t219 = "none"
    case Option__string_Some:
        var inline240 string = t218.(Option__string_Some)._0
        var inline242 string = "some=" + inline240
        t219 = inline242
    default:
        panic("non-exhaustive match")
    }
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline237)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
