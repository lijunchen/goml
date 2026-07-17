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
    var retv70 _goml_m_Option_____o_string_c_string_q_
    var jp72 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t73 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t74 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t73,
        }
        jp72 = t74
    } else {
        jp72 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv70 = jp72
    return retv70
}

func describe(ok__1 bool) Option__string {
    var retv76 Option__string
    var mtmp61 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    var jp78 Tuple2_6string_6string
    switch mtmp61.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv76 = Option__string_None{}
        return retv76
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x62 Tuple2_6string_6string = mtmp61.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__13 Tuple2_6string_6string = x62
        jp78 = try_value__13
        var mtmp63 Tuple2_6string_6string = jp78
        var x64 string = mtmp63._0
        var x65 string = mtmp63._1
        var after__3 string = x65
        var before__2 string = x64
        var t79 string = before__2 + "|"
        var t80 string = t79 + after__3
        var t81 Option__string = Option__string_Some{
            _0: t80,
        }
        retv76 = t81
        return retv76
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__string) string {
    var retv83 string
    var jp85 string
    switch opt__4.(type) {
    case Option__string_None:
        jp85 = "none"
    case Option__string_Some:
        var x66 string = opt__4.(Option__string_Some)._0
        var value__5 string = x66
        var t86 string = "some " + value__5
        jp85 = t86
    default:
        panic("non-exhaustive match")
    }
    retv83 = jp85
    return retv83
}

func main0() struct{} {
    var t88 Option__string = describe(true)
    var t89 string = show(t88)
    println__T_string(t89)
    var t90 Option__string = describe(false)
    var t91 string = show(t90)
    println__T_string(t91)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv96 string
    retv96 = self__37
    return retv96
}

func main() {
    main0()
}
