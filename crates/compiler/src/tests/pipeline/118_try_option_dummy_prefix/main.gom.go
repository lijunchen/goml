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
    var retv70 Option__string
    var t73 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(case_id__0, 0)
    var jp72 Option__string
    if t73 {
        var t74 Option__string = Some{
            _0: "ml",
        }
        jp72 = t74
    } else {
        jp72 = None{}
    }
    retv70 = jp72
    return retv70
}

func trim_go(case_id__1 int32) Option__string {
    var retv76 Option__string
    var mtmp64 Option__string = cut_prefix(case_id__1)
    var jp78 string
    switch mtmp64.(type) {
    case None:
        retv76 = None{}
        return retv76
    case Some:
        var x65 string = mtmp64.(Some)._0
        var try_value__13 string = x65
        jp78 = try_value__13
        var suffix__2 string = jp78
        var t79 string = suffix__2 + "!"
        var t80 Option__string = Some{
            _0: t79,
        }
        retv76 = t80
        return retv76
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__string) string {
    var retv82 string
    var jp84 string
    switch opt__3.(type) {
    case None:
        jp84 = "none"
    case Some:
        var x66 string = opt__3.(Some)._0
        var value__4 string = x66
        var t85 string = "some " + value__4
        jp84 = t85
    default:
        panic("non-exhaustive match")
    }
    retv82 = jp84
    return retv82
}

func main0() struct{} {
    var t87 Option__string = trim_go(0)
    var t88 string = show(t87)
    println__T_string(t88)
    var t89 Option__string = trim_go(1)
    var t90 string = show(t89)
    println__T_string(t90)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv92 bool
    var t93 bool = self__65 == other__66
    retv92 = t93
    return retv92
}

func println__T_string(value__1 string) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv98 string
    retv98 = self__38
    return retv98
}

func main() {
    main0()
}
