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
            _0: "left",
            _1: "right",
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

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    var retv76 _goml_m_Option_____o_string_c_string_q_
    var t77 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    retv76 = t77
    return retv76
}

func describe(ok__2 bool) Option__string {
    var retv79 Option__string
    var mtmp61 _goml_m_Option_____o_string_c_string_q_ = pair(ok__2)
    var jp81 Tuple2_6string_6string
    switch mtmp61.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv79 = Option__string_None{}
        return retv79
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x62 Tuple2_6string_6string = mtmp61.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__16 Tuple2_6string_6string = x62
        jp81 = try_value__16
        var mtmp63 Tuple2_6string_6string = jp81
        var x64 string = mtmp63._0
        var x65 string = mtmp63._1
        var after__4 string = x65
        var before__3 string = x64
        var t82 string = before__3 + ":"
        var t83 string = t82 + after__4
        var t84 Option__string = Option__string_Some{
            _0: t83,
        }
        retv79 = t84
        return retv79
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__string) string {
    var retv86 string
    var jp88 string
    switch opt__5.(type) {
    case Option__string_None:
        jp88 = "none"
    case Option__string_Some:
        var x66 string = opt__5.(Option__string_Some)._0
        var value__6 string = x66
        var t89 string = "some " + value__6
        jp88 = t89
    default:
        panic("non-exhaustive match")
    }
    retv86 = jp88
    return retv86
}

func main0() struct{} {
    var t91 Option__string = describe(true)
    var t92 string = show(t91)
    println__T_string(t92)
    var t93 Option__string = describe(false)
    var t94 string = show(t93)
    println__T_string(t94)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t96 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t96)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv99 string
    retv99 = self__37
    return retv99
}

func main() {
    main0()
}
