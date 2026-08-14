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
    var mtmp182 Option__int32
    if primary__2 {
        var inline240 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        mtmp182 = inline240
    } else {
        mtmp182 = Option__int32_None{}
    }
    var jp203 int32
    switch mtmp182.(type) {
    case Option__int32_None:
        return Option__string_None{}
    case Option__int32_Some:
        var x183 int32 = mtmp182.(Option__int32_Some)._0
        jp203 = x183
        var mtmp184 Option__int32
        if secondary__3 {
            var inline238 Option__int32 = Option__int32_Some{
                _0: 9,
            }
            mtmp184 = inline238
        } else {
            mtmp184 = Option__int32_None{}
        }
        var jp205 string
        switch mtmp184.(type) {
        case Option__int32_None:
            jp205 = "extra=none"
        case Option__int32_Some:
            var x185 int32 = mtmp184.(Option__int32_Some)._0
            var t211 string
            var inline234 string = _goml_runtime_core_int32_to_string(x185)
            t211 = inline234
            var t212 string = "extra=" + t211
            jp205 = t212
        default:
            panic("non-exhaustive match")
        }
        var t206 string
        var inline236 string = _goml_runtime_core_int32_to_string(jp203)
        t206 = inline236
        var t207 string = "value=" + t206
        var t208 string = t207 + ","
        var t209 string = t208 + jp205
        var t210 Option__string = Option__string_Some{
            _0: t209,
        }
        return t210
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t219 Option__string = mixed(true, true)
    var t220 string
    switch t219.(type) {
    case Option__string_None:
        t220 = "none"
    case Option__string_Some:
        var inline259 string = t219.(Option__string_Some)._0
        var inline261 string = "some=" + inline259
        t220 = inline261
    default:
        panic("non-exhaustive match")
    }
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
    _goml_runtime_core_string_println(inline256)
    var t221 Option__string = mixed(true, false)
    var t222 string
    switch t221.(type) {
    case Option__string_None:
        t222 = "none"
    case Option__string_Some:
        var inline252 string = t221.(Option__string_Some)._0
        var inline254 string = "some=" + inline252
        t222 = inline254
    default:
        panic("non-exhaustive match")
    }
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline249)
    var t223 Option__string = mixed(false, true)
    var t224 string
    switch t223.(type) {
    case Option__string_None:
        t224 = "none"
    case Option__string_Some:
        var inline245 string = t223.(Option__string_Some)._0
        var inline247 string = "some=" + inline245
        t224 = inline247
    default:
        panic("non-exhaustive match")
    }
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline242)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
