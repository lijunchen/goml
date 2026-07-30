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
    var retv114 Option__string
    var t117 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(case_id__0, 0)
    var jp116 Option__string
    if t117 {
        var t118 Option__string = Some{
            _0: "ml",
        }
        jp116 = t118
    } else {
        jp116 = None{}
    }
    retv114 = jp116
    return retv114
}

func trim_go(case_id__1 int32) Option__string {
    var retv120 Option__string
    var mtmp108 Option__string = cut_prefix(case_id__1)
    var jp122 string
    switch mtmp108.(type) {
    case None:
        retv120 = None{}
        return retv120
    case Some:
        var x109 string = mtmp108.(Some)._0
        var try_value__13 string = x109
        jp122 = try_value__13
        var suffix__2 string = jp122
        var t123 string = suffix__2 + "!"
        var t124 Option__string = Some{
            _0: t123,
        }
        retv120 = t124
        return retv120
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    var retv126 string
    var jp128 string
    switch opt__3.(type) {
    case None:
        jp128 = "none"
    case Some:
        var x110 string = opt__3.(Some)._0
        var value__4 string = x110
        var t129 string = "some " + value__4
        jp128 = t129
    default:
        panic("non-exhaustive match")
    }
    retv126 = jp128
    return retv126
}

func main0() struct{} {
    var t131 Option__string = trim_go(0)
    var t132 string = show(t131)
    println__T_string(t132)
    var t133 Option__string = trim_go(1)
    var t134 string = show(t133)
    println__T_string(t134)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv136 bool
    var t137 bool = self__65 == other__66
    retv136 = t137
    return retv136
}

func println__T_string(value__1 string) struct{} {
    var t139 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t139)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv142 string
    retv142 = self__38
    return retv142
}

func main() {
    main0()
}
