package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type _goml_m_Option_____o_string_c_string_q_ interface {
    is_goml_m_Option_____o_string_c_string_q_()
}

type _goml_m_Option_____o_string_c_string_q__None struct {}

func (_ _goml_m_Option_____o_string_c_string_q__None) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option_____o_string_c_string_q__Some struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Option_____o_string_c_string_q__Some) is_goml_m_Option_____o_string_c_string_q_() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func cut_pair(ok__0 bool) _goml_m_Option_____o_string_c_string_q_ {
    var retv67 _goml_m_Option_____o_string_c_string_q_
    var jp69 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t70 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t71 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t70,
        }
        jp69 = t71
    } else {
        jp69 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv67 = jp69
    return retv67
}

func describe(ok__1 bool) Option__string {
    var retv73 Option__string
    var mtmp58 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    var jp75 Tuple2_6string_6string
    switch mtmp58.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv73 = Option__string_None{}
        return retv73
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x59 Tuple2_6string_6string = mtmp58.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__13 Tuple2_6string_6string = x59
        jp75 = try_value__13
        var mtmp60 Tuple2_6string_6string = jp75
        var x61 string = mtmp60._0
        var x62 string = mtmp60._1
        var after__3 string = x62
        var before__2 string = x61
        var t76 string = before__2 + "|"
        var t77 string = t76 + after__3
        var t78 Option__string = Option__string_Some{
            _0: t77,
        }
        retv73 = t78
        return retv73
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__string) string {
    var retv80 string
    var jp82 string
    switch opt__4.(type) {
    case Option__string_None:
        jp82 = "none"
    case Option__string_Some:
        var x63 string = opt__4.(Option__string_Some)._0
        var value__5 string = x63
        var t83 string = "some " + value__5
        jp82 = t83
    default:
        panic("non-exhaustive match")
    }
    retv80 = jp82
    return retv80
}

func main0() struct{} {
    var t85 Option__string = describe(true)
    var t86 string = show(t85)
    println__T_string(t86)
    var t87 Option__string = describe(false)
    var t88 string = show(t87)
    println__T_string(t88)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv93 string
    retv93 = self__34
    return retv93
}

func main() {
    main0()
}
