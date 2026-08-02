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
    var mtmp155 Option__int32
    if primary__2 {
        var inline213 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        mtmp155 = inline213
    } else {
        mtmp155 = Option__int32_None{}
    }
    var jp176 int32
    switch mtmp155.(type) {
    case Option__int32_None:
        return Option__string_None{}
    case Option__int32_Some:
        var x156 int32 = mtmp155.(Option__int32_Some)._0
        jp176 = x156
        var mtmp157 Option__int32
        if secondary__3 {
            var inline211 Option__int32 = Option__int32_Some{
                _0: 9,
            }
            mtmp157 = inline211
        } else {
            mtmp157 = Option__int32_None{}
        }
        var jp178 string
        switch mtmp157.(type) {
        case Option__int32_None:
            jp178 = "extra=none"
        case Option__int32_Some:
            var x158 int32 = mtmp157.(Option__int32_Some)._0
            var t184 string
            var inline207 string = _goml_runtime_core_int32_to_string(x158)
            t184 = inline207
            var t185 string = "extra=" + t184
            jp178 = t185
        default:
            panic("non-exhaustive match")
        }
        var t179 string
        var inline209 string = _goml_runtime_core_int32_to_string(jp176)
        t179 = inline209
        var t180 string = "value=" + t179
        var t181 string = t180 + ","
        var t182 string = t181 + jp178
        var t183 Option__string = Option__string_Some{
            _0: t182,
        }
        return t183
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t192 Option__string = mixed(true, true)
    var t193 string
    switch t192.(type) {
    case Option__string_None:
        t193 = "none"
    case Option__string_Some:
        var inline232 string = t192.(Option__string_Some)._0
        var inline234 string = "some=" + inline232
        t193 = inline234
    default:
        panic("non-exhaustive match")
    }
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline229)
    var t194 Option__string = mixed(true, false)
    var t195 string
    switch t194.(type) {
    case Option__string_None:
        t195 = "none"
    case Option__string_Some:
        var inline225 string = t194.(Option__string_Some)._0
        var inline227 string = "some=" + inline225
        t195 = inline227
    default:
        panic("non-exhaustive match")
    }
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline222)
    var t196 Option__string = mixed(false, true)
    var t197 string
    switch t196.(type) {
    case Option__string_None:
        t197 = "none"
    case Option__string_Some:
        var inline218 string = t196.(Option__string_Some)._0
        var inline220 string = "some=" + inline218
        t197 = inline220
    default:
        panic("non-exhaustive match")
    }
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline215)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
