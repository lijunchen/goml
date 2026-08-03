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
    var mtmp177 Option__int32
    if flag__2 {
        var inline228 Option__int32 = Some{
            _0: 3,
        }
        mtmp177 = inline228
    } else {
        mtmp177 = None{}
    }
    var jp199 int32
    switch mtmp177.(type) {
    case None:
        return None{}
    case Some:
        var x178 int32 = mtmp177.(Some)._0
        jp199 = x178
        var mtmp179 Option__int32
        var inline224 bool = jp199 > 0
        if inline224 {
            var inline225 int32 = jp199 * 2
            var inline226 Option__int32 = Some{
                _0: inline225,
            }
            mtmp179 = inline226
        } else {
            mtmp179 = None{}
        }
        var jp201 int32
        switch mtmp179.(type) {
        case None:
            return None{}
        case Some:
            var x180 int32 = mtmp179.(Some)._0
            jp201 = x180
            var t202 int32 = jp199 + jp201
            var t203 Option__int32 = Some{
                _0: t202,
            }
            return t203
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t211 Option__int32 = maybe_total(true)
    var t212 string
    switch t211.(type) {
    case None:
        t212 = "none"
    case Some:
        var inline243 int32 = t211.(Some)._0
        var inline245 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline243)
        var inline246 string = "some=" + inline245
        t212 = inline246
    default:
        panic("non-exhaustive match")
    }
    var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline240)
    var t213 Option__int32 = maybe_total(false)
    var t214 string
    switch t213.(type) {
    case None:
        t214 = "none"
    case Some:
        var inline235 int32 = t213.(Some)._0
        var inline237 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline235)
        var inline238 string = "some=" + inline237
        t214 = inline238
    default:
        panic("non-exhaustive match")
    }
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline232)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t217 string = _goml_runtime_core_int32_to_string(self__35)
    return t217
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
