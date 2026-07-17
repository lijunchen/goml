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
    var retv64 Option__string
    var t67 bool = case_id__0 == 0
    var jp66 Option__string
    if t67 {
        var t68 Option__string = Some{
            _0: "ml",
        }
        jp66 = t68
    } else {
        jp66 = None{}
    }
    retv64 = jp66
    return retv64
}

func trim_go(case_id__1 int32) Option__string {
    var retv70 Option__string
    var mtmp58 Option__string = cut_prefix(case_id__1)
    var jp72 string
    switch mtmp58.(type) {
    case None:
        retv70 = None{}
        return retv70
    case Some:
        var x59 string = mtmp58.(Some)._0
        var try_value__13 string = x59
        jp72 = try_value__13
        var suffix__2 string = jp72
        var t73 string = suffix__2 + "!"
        var t74 Option__string = Some{
            _0: t73,
        }
        retv70 = t74
        return retv70
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    var retv76 string
    var jp78 string
    switch opt__3.(type) {
    case None:
        jp78 = "none"
    case Some:
        var x60 string = opt__3.(Some)._0
        var value__4 string = x60
        var t79 string = "some " + value__4
        jp78 = t79
    default:
        panic("non-exhaustive match")
    }
    retv76 = jp78
    return retv76
}

func main0() struct{} {
    var t81 Option__string = trim_go(0)
    var t82 string = show(t81)
    println__T_string(t82)
    var t83 Option__string = trim_go(1)
    var t84 string = show(t83)
    println__T_string(t84)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t86 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t86)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv89 string
    retv89 = self__34
    return retv89
}

func main() {
    main0()
}
