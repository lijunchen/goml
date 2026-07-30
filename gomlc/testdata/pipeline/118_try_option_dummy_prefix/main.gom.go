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
    var retv74 Option__string
    var t77 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(case_id__0, 0)
    var jp76 Option__string
    if t77 {
        var t78 Option__string = Some{
            _0: "ml",
        }
        jp76 = t78
    } else {
        jp76 = None{}
    }
    retv74 = jp76
    return retv74
}

func trim_go(case_id__1 int32) Option__string {
    var retv80 Option__string
    var mtmp68 Option__string = cut_prefix(case_id__1)
    var jp82 string
    switch mtmp68.(type) {
    case None:
        retv80 = None{}
        return retv80
    case Some:
        var x69 string = mtmp68.(Some)._0
        var try_value__13 string = x69
        jp82 = try_value__13
        var suffix__2 string = jp82
        var t83 string = suffix__2 + "!"
        var t84 Option__string = Some{
            _0: t83,
        }
        retv80 = t84
        return retv80
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    var retv86 string
    var jp88 string
    switch opt__3.(type) {
    case None:
        jp88 = "none"
    case Some:
        var x70 string = opt__3.(Some)._0
        var value__4 string = x70
        var t89 string = "some " + value__4
        jp88 = t89
    default:
        panic("non-exhaustive match")
    }
    retv86 = jp88
    return retv86
}

func main0() struct{} {
    var t91 Option__string = trim_go(0)
    var t92 string = show(t91)
    println__T_string(t92)
    var t93 Option__string = trim_go(1)
    var t94 string = show(t93)
    println__T_string(t94)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv96 bool
    var t97 bool = self__65 == other__66
    retv96 = t97
    return retv96
}

func println__T_string(value__1 string) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv102 string
    retv102 = self__38
    return retv102
}

func main() {
    main0()
}
