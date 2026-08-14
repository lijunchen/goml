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
    var mtmp187 Option__int32
    if primary__2 {
        var inline245 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        mtmp187 = inline245
    } else {
        mtmp187 = Option__int32_None{}
    }
    var jp208 int32
    switch mtmp187.(type) {
    case Option__int32_None:
        return Option__string_None{}
    case Option__int32_Some:
        var x188 int32 = mtmp187.(Option__int32_Some)._0
        jp208 = x188
        var mtmp189 Option__int32
        if secondary__3 {
            var inline243 Option__int32 = Option__int32_Some{
                _0: 9,
            }
            mtmp189 = inline243
        } else {
            mtmp189 = Option__int32_None{}
        }
        var jp210 string
        switch mtmp189.(type) {
        case Option__int32_None:
            jp210 = "extra=none"
        case Option__int32_Some:
            var x190 int32 = mtmp189.(Option__int32_Some)._0
            var t216 string
            var inline239 string = _goml_runtime_core_int32_to_string(x190)
            t216 = inline239
            var t217 string = "extra=" + t216
            jp210 = t217
        default:
            panic("non-exhaustive match")
        }
        var t211 string
        var inline241 string = _goml_runtime_core_int32_to_string(jp208)
        t211 = inline241
        var t212 string = "value=" + t211
        var t213 string = t212 + ","
        var t214 string = t213 + jp210
        var t215 Option__string = Option__string_Some{
            _0: t214,
        }
        return t215
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t224 Option__string = mixed(true, true)
    var t225 string
    switch t224.(type) {
    case Option__string_None:
        t225 = "none"
    case Option__string_Some:
        var inline264 string = t224.(Option__string_Some)._0
        var inline266 string = "some=" + inline264
        t225 = inline266
    default:
        panic("non-exhaustive match")
    }
    var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline261)
    var t226 Option__string = mixed(true, false)
    var t227 string
    switch t226.(type) {
    case Option__string_None:
        t227 = "none"
    case Option__string_Some:
        var inline257 string = t226.(Option__string_Some)._0
        var inline259 string = "some=" + inline257
        t227 = inline259
    default:
        panic("non-exhaustive match")
    }
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t227)
    _goml_runtime_core_string_println(inline254)
    var t228 Option__string = mixed(false, true)
    var t229 string
    switch t228.(type) {
    case Option__string_None:
        t229 = "none"
    case Option__string_Some:
        var inline250 string = t228.(Option__string_Some)._0
        var inline252 string = "some=" + inline250
        t229 = inline252
    default:
        panic("non-exhaustive match")
    }
    var inline247 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t229)
    _goml_runtime_core_string_println(inline247)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
