package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Option__string interface {
    isOption__string()
}

type None struct {}

func (_ None) isOption__string() {}

type Some struct {
    _0 string
}

func (_ Some) isOption__string() {}

func cut_prefix(case_id__0 int32) Option__string {
    var retv161 Option__string
    var t164 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(case_id__0, 0)
    var jp163 Option__string
    if t164 {
        var t165 Option__string = Some{
            _0: "ml",
        }
        jp163 = t165
    } else {
        jp163 = None{}
    }
    retv161 = jp163
    return retv161
}

func trim_go(case_id__1 int32) Option__string {
    var retv167 Option__string
    var mtmp155 Option__string = cut_prefix(case_id__1)
    var jp169 string
    switch mtmp155.(type) {
    case None:
        retv167 = None{}
        return retv167
    case Some:
        var x156 string = mtmp155.(Some)._0
        var try_value__13 string = x156
        jp169 = try_value__13
        var suffix__2 string = jp169
        var t170 string = suffix__2 + "!"
        var t171 Option__string = Some{
            _0: t170,
        }
        retv167 = t171
        return retv167
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    var retv173 string
    var jp175 string
    switch opt__3.(type) {
    case None:
        jp175 = "none"
    case Some:
        var x157 string = opt__3.(Some)._0
        var value__4 string = x157
        var t176 string = "some " + value__4
        jp175 = t176
    default:
        panic("non-exhaustive match")
    }
    retv173 = jp175
    return retv173
}

func main0() struct{} {
    var t178 Option__string = trim_go(0)
    var t179 string = show(t178)
    println__T_string(t179)
    var t180 Option__string = trim_go(1)
    var t181 string = show(t180)
    println__T_string(t181)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv183 bool
    var t184 bool = self__65 == other__66
    retv183 = t184
    return retv183
}

func println__T_string(value__1 string) struct{} {
    var t186 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t186)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv189 string
    retv189 = self__38
    return retv189
}

func main() {
    main0()
}
