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
    var retv10 Option__string
    var t13 bool = case_id__0 == 0
    var jp12 Option__string
    if t13 {
        var t14 Option__string = Some{
            _0: "ml",
        }
        jp12 = t14
    } else {
        jp12 = None{}
    }
    retv10 = jp12
    return retv10
}

func trim_go(case_id__1 int32) Option__string {
    var retv16 Option__string
    var mtmp4 Option__string = cut_prefix(case_id__1)
    var jp18 string
    switch mtmp4.(type) {
    case None:
        retv16 = None{}
        return retv16
    case Some:
        var x5 string = mtmp4.(Some)._0
        var try_value__13 string = x5
        jp18 = try_value__13
        var suffix__2 string = jp18
        var t19 string = suffix__2 + "!"
        var t20 Option__string = Some{
            _0: t19,
        }
        retv16 = t20
        return retv16
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    var retv22 string
    var jp24 string
    switch opt__3.(type) {
    case None:
        jp24 = "none"
    case Some:
        var x6 string = opt__3.(Some)._0
        var value__4 string = x6
        var t25 string = "some " + value__4
        jp24 = t25
    default:
        panic("non-exhaustive match")
    }
    retv22 = jp24
    return retv22
}

func main0() struct{} {
    var t27 Option__string = trim_go(0)
    var t28 string = show(t27)
    println__T_string(t28)
    var t29 Option__string = trim_go(1)
    var t30 string = show(t29)
    println__T_string(t30)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t32 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t32)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv35 string
    retv35 = self__9
    return retv35
}

func main() {
    main0()
}
