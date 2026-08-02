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

func maybe_value(flag__0 bool) Option__int32 {
    if flag__0 {
        var t164 Option__int32 = Some{
            _0: 4,
        }
        return t164
    } else {
        return None{}
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var t167 int32 = a__1 + b__2
    return t167
}

func plus_two(flag__3 bool) Option__int32 {
    var mtmp155 Option__int32 = maybe_value(flag__3)
    var jp171 int32
    switch mtmp155.(type) {
    case None:
        return None{}
    case Some:
        var x156 int32 = mtmp155.(Some)._0
        jp171 = x156
        var t172 int32 = add(jp171, 2)
        var t173 Option__int32 = Some{
            _0: t172,
        }
        return t173
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__int32) string {
    switch opt__4.(type) {
    case None:
        return "none"
    case Some:
        var x157 int32 = opt__4.(Some)._0
        var t178 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x157)
        var t179 string = "some=" + t178
        return t179
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t181 Option__int32 = plus_two(true)
    var t182 string = show(t181)
    println__T_string(t182)
    var t183 Option__int32 = plus_two(false)
    var t184 string = show(t183)
    println__T_string(t184)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t187 string = _goml_runtime_core_int32_to_string(self__6)
    return t187
}

func println__T_string(value__1 string) struct{} {
    var t189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t189)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
