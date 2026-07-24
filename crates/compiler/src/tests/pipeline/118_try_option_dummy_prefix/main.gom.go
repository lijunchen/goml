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
    var retv67 Option__string
    var t70 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(case_id__0, 0)
    var jp69 Option__string
    if t70 {
        var t71 Option__string = Some{
            _0: "ml",
        }
        jp69 = t71
    } else {
        jp69 = None{}
    }
    retv67 = jp69
    return retv67
}

func trim_go(case_id__1 int32) Option__string {
    var retv73 Option__string
    var mtmp61 Option__string = cut_prefix(case_id__1)
    var jp75 string
    switch mtmp61.(type) {
    case None:
        retv73 = None{}
        return retv73
    case Some:
        var x62 string = mtmp61.(Some)._0
        var try_value__13 string = x62
        jp75 = try_value__13
        var suffix__2 string = jp75
        var t76 string = suffix__2 + "!"
        var t77 Option__string = Some{
            _0: t76,
        }
        retv73 = t77
        return retv73
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    var retv79 string
    var jp81 string
    switch opt__3.(type) {
    case None:
        jp81 = "none"
    case Some:
        var x63 string = opt__3.(Some)._0
        var value__4 string = x63
        var t82 string = "some " + value__4
        jp81 = t82
    default:
        panic("non-exhaustive match")
    }
    retv79 = jp81
    return retv79
}

func main0() struct{} {
    var t84 Option__string = trim_go(0)
    var t85 string = show(t84)
    println__T_string(t85)
    var t86 Option__string = trim_go(1)
    var t87 string = show(t86)
    println__T_string(t87)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__61 int32, other__62 int32) bool {
    var retv89 bool
    var t90 bool = self__61 == other__62
    retv89 = t90
    return retv89
}

func println__T_string(value__1 string) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv95 string
    retv95 = self__37
    return retv95
}

func main() {
    main0()
}
