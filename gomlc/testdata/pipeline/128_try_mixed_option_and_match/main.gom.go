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
    var retv161 Option__int32
    var jp163 Option__int32
    if flag__0 {
        var t164 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        jp163 = t164
    } else {
        jp163 = Option__int32_None{}
    }
    retv161 = jp163
    return retv161
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var retv166 Option__int32
    var jp168 Option__int32
    if flag__1 {
        var t169 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        jp168 = t169
    } else {
        jp168 = Option__int32_None{}
    }
    retv166 = jp168
    return retv166
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var retv171 Option__string
    var mtmp152 Option__int32 = maybe_primary(primary__2)
    var jp173 int32
    switch mtmp152.(type) {
    case Option__int32_None:
        retv171 = Option__string_None{}
        return retv171
    case Option__int32_Some:
        var x153 int32 = mtmp152.(Option__int32_Some)._0
        var try_value__18 int32 = x153
        jp173 = try_value__18
        var value__4 int32 = jp173
        var mtmp154 Option__int32 = maybe_secondary(secondary__3)
        var jp175 string
        switch mtmp154.(type) {
        case Option__int32_None:
            jp175 = "extra=none"
        case Option__int32_Some:
            var x155 int32 = mtmp154.(Option__int32_Some)._0
            var extra__5 int32 = x155
            var t181 string = _goml_m_inherent_i_int32_i_int32_i_to__string(extra__5)
            var t182 string = "extra=" + t181
            jp175 = t182
        default:
            panic("non-exhaustive match")
        }
        var label__6 string = jp175
        var t176 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t177 string = "value=" + t176
        var t178 string = t177 + ","
        var t179 string = t178 + label__6
        var t180 Option__string = Option__string_Some{
            _0: t179,
        }
        retv171 = t180
        return retv171
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__7 Option__string) string {
    var retv184 string
    var jp186 string
    switch opt__7.(type) {
    case Option__string_None:
        jp186 = "none"
    case Option__string_Some:
        var x156 string = opt__7.(Option__string_Some)._0
        var value__8 string = x156
        var t187 string = "some=" + value__8
        jp186 = t187
    default:
        panic("non-exhaustive match")
    }
    retv184 = jp186
    return retv184
}

func main0() struct{} {
    var t189 Option__string = mixed(true, true)
    var t190 string = show(t189)
    println__T_string(t190)
    var t191 Option__string = mixed(true, false)
    var t192 string = show(t191)
    println__T_string(t192)
    var t193 Option__string = mixed(false, true)
    var t194 string = show(t193)
    println__T_string(t194)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv196 string
    var t197 string = _goml_runtime_core_int32_to_string(self__6)
    retv196 = t197
    return retv196
}

func println__T_string(value__1 string) struct{} {
    var t199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv202 string
    retv202 = self__38
    return retv202
}

func main() {
    main0()
}
