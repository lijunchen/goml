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
    var retv13 Option__string
    var t16 bool = case_id__0 == 0
    var jp15 Option__string
    if t16 {
        var t17 Option__string = Some{
            _0: "ml",
        }
        jp15 = t17
    } else {
        jp15 = None{}
    }
    retv13 = jp15
    return retv13
}

func trim_go(case_id__1 int32) Option__string {
    var retv19 Option__string
    var mtmp7 Option__string = cut_prefix(case_id__1)
    var jp21 string
    switch mtmp7.(type) {
    case None:
        retv19 = None{}
        return retv19
    case Some:
        var x8 string = mtmp7.(Some)._0
        var try_value__13 string = x8
        jp21 = try_value__13
        var suffix__2 string = jp21
        var t22 string = suffix__2 + "!"
        var t23 Option__string = Some{
            _0: t22,
        }
        retv19 = t23
        return retv19
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    var retv25 string
    var jp27 string
    switch opt__3.(type) {
    case None:
        jp27 = "none"
    case Some:
        var x9 string = opt__3.(Some)._0
        var value__4 string = x9
        var t28 string = "some " + value__4
        jp27 = t28
    default:
        panic("non-exhaustive match")
    }
    retv25 = jp27
    return retv25
}

func main0() struct{} {
    var t30 Option__string = trim_go(0)
    var t31 string = show(t30)
    println__T_string(t31)
    var t32 Option__string = trim_go(1)
    var t33 string = show(t32)
    println__T_string(t33)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t35 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t35)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv38 string
    retv38 = self__9
    return retv38
}

func main() {
    main0()
}
