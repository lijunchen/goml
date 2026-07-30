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
    var retv117 Option__int32
    var jp119 Option__int32
    if flag__0 {
        var t120 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        jp119 = t120
    } else {
        jp119 = Option__int32_None{}
    }
    retv117 = jp119
    return retv117
}

func maybe_secondary(flag__1 bool) Option__int32 {
    var retv122 Option__int32
    var jp124 Option__int32
    if flag__1 {
        var t125 Option__int32 = Option__int32_Some{
            _0: 9,
        }
        jp124 = t125
    } else {
        jp124 = Option__int32_None{}
    }
    retv122 = jp124
    return retv122
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var retv127 Option__string
    var mtmp108 Option__int32 = maybe_primary(primary__2)
    var jp129 int32
    switch mtmp108.(type) {
    case Option__int32_None:
        retv127 = Option__string_None{}
        return retv127
    case Option__int32_Some:
        var x109 int32 = mtmp108.(Option__int32_Some)._0
        var try_value__18 int32 = x109
        jp129 = try_value__18
        var value__4 int32 = jp129
        var mtmp110 Option__int32 = maybe_secondary(secondary__3)
        var jp131 string
        switch mtmp110.(type) {
        case Option__int32_None:
            jp131 = "extra=none"
        case Option__int32_Some:
            var x111 int32 = mtmp110.(Option__int32_Some)._0
            var extra__5 int32 = x111
            var t137 string = _goml_m_inherent_i_int32_i_int32_i_to__string(extra__5)
            var t138 string = "extra=" + t137
            jp131 = t138
        default:
            panic("non-exhaustive match")
        }
        var label__6 string = jp131
        var t132 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t133 string = "value=" + t132
        var t134 string = t133 + ","
        var t135 string = t134 + label__6
        var t136 Option__string = Option__string_Some{
            _0: t135,
        }
        retv127 = t136
        return retv127
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__7 Option__string) string {
    var retv140 string
    var jp142 string
    switch opt__7.(type) {
    case Option__string_None:
        jp142 = "none"
    case Option__string_Some:
        var x112 string = opt__7.(Option__string_Some)._0
        var value__8 string = x112
        var t143 string = "some=" + value__8
        jp142 = t143
    default:
        panic("non-exhaustive match")
    }
    retv140 = jp142
    return retv140
}

func main0() struct{} {
    var t145 Option__string = mixed(true, true)
    var t146 string = show(t145)
    println__T_string(t146)
    var t147 Option__string = mixed(true, false)
    var t148 string = show(t147)
    println__T_string(t148)
    var t149 Option__string = mixed(false, true)
    var t150 string = show(t149)
    println__T_string(t150)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv152 string
    var t153 string = _goml_runtime_core_int32_to_string(self__6)
    retv152 = t153
    return retv152
}

func println__T_string(value__1 string) struct{} {
    var t155 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t155)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv158 string
    retv158 = self__38
    return retv158
}

func main() {
    main0()
}
