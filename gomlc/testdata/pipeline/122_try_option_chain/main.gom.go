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
    var mtmp187 Option__int32
    if flag__2 {
        var inline238 Option__int32 = Some{
            _0: 3,
        }
        mtmp187 = inline238
    } else {
        mtmp187 = None{}
    }
    var jp209 int32
    switch mtmp187.(type) {
    case None:
        return None{}
    case Some:
        var x188 int32 = mtmp187.(Some)._0
        jp209 = x188
        var mtmp189 Option__int32
        var inline234 bool = jp209 > 0
        if inline234 {
            var inline235 int32 = jp209 * 2
            var inline236 Option__int32 = Some{
                _0: inline235,
            }
            mtmp189 = inline236
        } else {
            mtmp189 = None{}
        }
        var jp211 int32
        switch mtmp189.(type) {
        case None:
            return None{}
        case Some:
            var x190 int32 = mtmp189.(Some)._0
            jp211 = x190
            var t212 int32 = jp209 + jp211
            var t213 Option__int32 = Some{
                _0: t212,
            }
            return t213
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t221 Option__int32 = maybe_total(true)
    var t222 string
    switch t221.(type) {
    case None:
        t222 = "none"
    case Some:
        var inline253 int32 = t221.(Some)._0
        var inline255 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline253)
        var inline256 string = "some=" + inline255
        t222 = inline256
    default:
        panic("non-exhaustive match")
    }
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline250)
    var t223 Option__int32 = maybe_total(false)
    var t224 string
    switch t223.(type) {
    case None:
        t224 = "none"
    case Some:
        var inline245 int32 = t223.(Some)._0
        var inline247 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline245)
        var inline248 string = "some=" + inline247
        t224 = inline248
    default:
        panic("non-exhaustive match")
    }
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline242)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t227 string = _goml_runtime_core_int32_to_string(self__33)
    return t227
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
