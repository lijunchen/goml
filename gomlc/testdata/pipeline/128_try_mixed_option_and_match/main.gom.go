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

func maybe_primary(flag__0 bool) Option__int32 {
    if flag__0 {
        var t167 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        return t167
    } else {
        return Option__int32_None{}
    }
}

func maybe_secondary(flag__1 bool) Option__int32 {
    if flag__1 {
        var t172 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        return t172
    } else {
        return Option__int32_None{}
    }
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var mtmp155 Option__int32 = maybe_primary(primary__2)
    var jp176 int32
    switch mtmp155.(type) {
    case Option__int32_None:
        return Option__string_None{}
    case Option__int32_Some:
        var x156 int32 = mtmp155.(Option__int32_Some)._0
        jp176 = x156
        var mtmp157 Option__int32 = maybe_secondary(secondary__3)
        var jp178 string
        switch mtmp157.(type) {
        case Option__int32_None:
            jp178 = "extra=none"
        case Option__int32_Some:
            var x158 int32 = mtmp157.(Option__int32_Some)._0
            var t184 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x158)
            var t185 string = "extra=" + t184
            jp178 = t185
        default:
            panic("non-exhaustive match")
        }
        var t179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(jp176)
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

func show(opt__7 Option__string) string {
    switch opt__7.(type) {
    case Option__string_None:
        return "none"
    case Option__string_Some:
        var x159 string = opt__7.(Option__string_Some)._0
        var t190 string = "some=" + x159
        return t190
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t192 Option__string = mixed(true, true)
    var t193 string = show(t192)
    println__T_string(t193)
    var t194 Option__string = mixed(true, false)
    var t195 string = show(t194)
    println__T_string(t195)
    var t196 Option__string = mixed(false, true)
    var t197 string = show(t196)
    println__T_string(t197)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t200 string = _goml_runtime_core_int32_to_string(self__6)
    return t200
}

func println__T_string(value__1 string) struct{} {
    var t202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
