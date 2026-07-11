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
    var retv28 Option__string
    var t31 bool = case_id__0 == 0
    var jp30 Option__string
    if t31 {
        var t32 Option__string = Some{
            _0: "ml",
        }
        jp30 = t32
    } else {
        jp30 = None{}
    }
    retv28 = jp30
    return retv28
}

func trim_go(case_id__1 int32) Option__string {
    var retv34 Option__string
    var mtmp22 Option__string = cut_prefix(case_id__1)
    var jp36 string
    switch mtmp22.(type) {
    case None:
        retv34 = None{}
        return retv34
    case Some:
        var x23 string = mtmp22.(Some)._0
        var try_value__13 string = x23
        jp36 = try_value__13
        var suffix__2 string = jp36
        var t37 string = suffix__2 + "!"
        var t38 Option__string = Some{
            _0: t37,
        }
        retv34 = t38
        return retv34
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    var retv40 string
    var jp42 string
    switch opt__3.(type) {
    case None:
        jp42 = "none"
    case Some:
        var x24 string = opt__3.(Some)._0
        var value__4 string = x24
        var t43 string = "some " + value__4
        jp42 = t43
    default:
        panic("non-exhaustive match")
    }
    retv40 = jp42
    return retv40
}

func main0() struct{} {
    var t45 Option__string = trim_go(0)
    var t46 string = show(t45)
    println__T_string(t46)
    var t47 Option__string = trim_go(1)
    var t48 string = show(t47)
    println__T_string(t48)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t50 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t50)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv53 string
    retv53 = self__9
    return retv53
}

func main() {
    main0()
}
