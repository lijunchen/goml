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
    var retv161 Option__int32
    var jp163 Option__int32
    if flag__0 {
        var t164 Option__int32 = Some{
            _0: 4,
        }
        jp163 = t164
    } else {
        jp163 = None{}
    }
    retv161 = jp163
    return retv161
}

func add(a__1 int32, b__2 int32) int32 {
    var retv166 int32
    var t167 int32 = a__1 + b__2
    retv166 = t167
    return retv166
}

func plus_two(flag__3 bool) Option__int32 {
    var retv169 Option__int32
    var mtmp155 Option__int32 = maybe_value(flag__3)
    var jp171 int32
    switch mtmp155.(type) {
    case None:
        retv169 = None{}
        return retv169
    case Some:
        var x156 int32 = mtmp155.(Some)._0
        var try_value__15 int32 = x156
        jp171 = try_value__15
        var t172 int32 = add(jp171, 2)
        var t173 Option__int32 = Some{
            _0: t172,
        }
        retv169 = t173
        return retv169
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__int32) string {
    var retv175 string
    var jp177 string
    switch opt__4.(type) {
    case None:
        jp177 = "none"
    case Some:
        var x157 int32 = opt__4.(Some)._0
        var value__5 int32 = x157
        var t178 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t179 string = "some=" + t178
        jp177 = t179
    default:
        panic("non-exhaustive match")
    }
    retv175 = jp177
    return retv175
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
    var retv186 string
    var t187 string = _goml_runtime_core_int32_to_string(self__6)
    retv186 = t187
    return retv186
}

func println__T_string(value__1 string) struct{} {
    var t189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t189)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv192 string
    retv192 = self__38
    return retv192
}

func main() {
    main0()
}
