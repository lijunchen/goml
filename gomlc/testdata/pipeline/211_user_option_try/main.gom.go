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
    if flag__0 {
        var t163 Option__int32 = Some{
            _0: 41,
        }
        return t163
    } else {
        return None{}
    }
}

func compute(flag__1 bool) Option__int32 {
    var mtmp155 Option__int32 = maybe_value(flag__1)
    var jp167 int32
    switch mtmp155.(type) {
    case Some:
        var x156 int32 = mtmp155.(Some)._0
        jp167 = x156
        var t168 int32 = jp167 + 1
        var t169 Option__int32 = Some{
            _0: t168,
        }
        return t169
    case None:
        return None{}
    default:
        panic("non-exhaustive match")
    }
}

func show(value__3 Option__int32) string {
    switch value__3.(type) {
    case Some:
        var x157 int32 = value__3.(Some)._0
        var t174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x157)
        return t174
    case None:
        return "none"
    default:
        panic("non-exhaustive match")
    }
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
    var t183 string = _goml_runtime_core_int32_to_string(self__6)
    return t183
}

func println__T_string(value__1 string) struct{} {
    var t185 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t185)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
