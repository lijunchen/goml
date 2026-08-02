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
    var t164 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(case_id__0, 0)
    if t164 {
        var t165 Option__string = Some{
            _0: "ml",
        }
        return t165
    } else {
        return None{}
    }
}

func trim_go(case_id__1 int32) Option__string {
    var mtmp155 Option__string = cut_prefix(case_id__1)
    var jp169 string
    switch mtmp155.(type) {
    case None:
        return None{}
    case Some:
        var x156 string = mtmp155.(Some)._0
        jp169 = x156
        var t170 string = jp169 + "!"
        var t171 Option__string = Some{
            _0: t170,
        }
        return t171
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    switch opt__3.(type) {
    case None:
        return "none"
    case Some:
        var x157 string = opt__3.(Some)._0
        var t176 string = "some " + x157
        return t176
    default:
        panic("non-exhaustive match")
    }
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
    var t184 bool = self__65 == other__66
    return t184
}

func println__T_string(value__1 string) struct{} {
    var t186 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t186)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
