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

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

type None struct {}

func (_ None) isOption__int32() {}

func maybe_value(flag__0 bool) Option__int32 {
    var retv160 Option__int32
    var jp162 Option__int32
    if flag__0 {
        var t163 Option__int32 = Some{
            _0: 41,
        }
        jp162 = t163
    } else {
        jp162 = None{}
    }
    retv160 = jp162
    return retv160
}

func compute(flag__1 bool) Option__int32 {
    var retv165 Option__int32
    var mtmp155 Option__int32 = maybe_value(flag__1)
    var jp167 int32
    switch mtmp155.(type) {
    case Some:
        var x156 int32 = mtmp155.(Some)._0
        var try_value__11 int32 = x156
        jp167 = try_value__11
        var value__2 int32 = jp167
        var t168 int32 = value__2 + 1
        var t169 Option__int32 = Some{
            _0: t168,
        }
        retv165 = t169
        return retv165
    case None:
        retv165 = None{}
        return retv165
    default:
        panic("non-exhaustive match")
    }
}

func show(value__3 Option__int32) string {
    var retv171 string
    var jp173 string
    switch value__3.(type) {
    case Some:
        var x157 int32 = value__3.(Some)._0
        var value__4 int32 = x157
        var t174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        jp173 = t174
    case None:
        jp173 = "none"
    default:
        panic("non-exhaustive match")
    }
    retv171 = jp173
    return retv171
}

func main0() struct{} {
    var t176 Option__int32 = compute(true)
    var t177 string = show(t176)
    println__T_string(t177)
    var t178 Option__int32 = compute(false)
    var t179 string = show(t178)
    println__T_string(t179)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv182 string
    var t183 string = _goml_runtime_core_int32_to_string(self__6)
    retv182 = t183
    return retv182
}

func println__T_string(value__1 string) struct{} {
    var t185 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t185)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv188 string
    retv188 = self__38
    return retv188
}

func main() {
    main0()
}
