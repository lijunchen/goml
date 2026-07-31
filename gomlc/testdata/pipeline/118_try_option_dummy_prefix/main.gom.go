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
    var retv158 Option__string
    var t161 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(case_id__0, 0)
    var jp160 Option__string
    if t161 {
        var t162 Option__string = Some{
            _0: "ml",
        }
        jp160 = t162
    } else {
        jp160 = None{}
    }
    retv158 = jp160
    return retv158
}

func trim_go(case_id__1 int32) Option__string {
    var retv164 Option__string
    var mtmp152 Option__string = cut_prefix(case_id__1)
    var jp166 string
    switch mtmp152.(type) {
    case None:
        retv164 = None{}
        return retv164
    case Some:
        var x153 string = mtmp152.(Some)._0
        var try_value__13 string = x153
        jp166 = try_value__13
        var suffix__2 string = jp166
        var t167 string = suffix__2 + "!"
        var t168 Option__string = Some{
            _0: t167,
        }
        retv164 = t168
        return retv164
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    var retv170 string
    var jp172 string
    switch opt__3.(type) {
    case None:
        jp172 = "none"
    case Some:
        var x154 string = opt__3.(Some)._0
        var value__4 string = x154
        var t173 string = "some " + value__4
        jp172 = t173
    default:
        panic("non-exhaustive match")
    }
    retv170 = jp172
    return retv170
}

func main0() struct{} {
    var t175 Option__string = trim_go(0)
    var t176 string = show(t175)
    println__T_string(t176)
    var t177 Option__string = trim_go(1)
    var t178 string = show(t177)
    println__T_string(t178)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv180 bool
    var t181 bool = self__65 == other__66
    retv180 = t181
    return retv180
}

func println__T_string(value__1 string) struct{} {
    var t183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t183)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv186 string
    retv186 = self__38
    return retv186
}

func main() {
    main0()
}
