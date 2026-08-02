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
    var retv164 Option__int32
    var jp166 Option__int32
    if flag__0 {
        var t167 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        jp166 = t167
    } else {
        jp166 = Option__int32_None{}
    }
    retv164 = jp166
    return retv164
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var retv169 Option__int32
    var jp171 Option__int32
    if flag__1 {
        var t172 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        jp171 = t172
    } else {
        jp171 = Option__int32_None{}
    }
    retv169 = jp171
    return retv169
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var retv174 Option__string
    var mtmp155 Option__int32 = maybe_primary(primary__2)
    var jp176 int32
    switch mtmp155.(type) {
    case Option__int32_None:
        retv174 = Option__string_None{}
        return retv174
    case Option__int32_Some:
        var x156 int32 = mtmp155.(Option__int32_Some)._0
        var try_value__18 int32 = x156
        jp176 = try_value__18
        var value__4 int32 = jp176
        var mtmp157 Option__int32 = maybe_secondary(secondary__3)
        var jp178 string
        switch mtmp157.(type) {
        case Option__int32_None:
            jp178 = "extra=none"
        case Option__int32_Some:
            var x158 int32 = mtmp157.(Option__int32_Some)._0
            var extra__5 int32 = x158
            var t184 string = _goml_m_inherent_i_int32_i_int32_i_to__string(extra__5)
            var t185 string = "extra=" + t184
            jp178 = t185
        default:
            panic("non-exhaustive match")
        }
        var label__6 string = jp178
        var t179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t180 string = "value=" + t179
        var t181 string = t180 + ","
        var t182 string = t181 + label__6
        var t183 Option__string = Option__string_Some{
            _0: t182,
        }
        retv174 = t183
        return retv174
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__7 Option__string) string {
    var retv187 string
    var jp189 string
    switch opt__7.(type) {
    case Option__string_None:
        jp189 = "none"
    case Option__string_Some:
        var x159 string = opt__7.(Option__string_Some)._0
        var value__8 string = x159
        var t190 string = "some=" + value__8
        jp189 = t190
    default:
        panic("non-exhaustive match")
    }
    retv187 = jp189
    return retv187
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
    var retv199 string
    var t200 string = _goml_runtime_core_int32_to_string(self__6)
    retv199 = t200
    return retv199
}

func println__T_string(value__1 string) struct{} {
    var t202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv205 string
    retv205 = self__38
    return retv205
}

func main() {
    main0()
}
