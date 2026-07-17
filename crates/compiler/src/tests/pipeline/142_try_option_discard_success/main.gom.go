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
    var retv65 _goml_m_Option_____o_string_c_string_q_
    var jp67 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t68 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t69 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t68,
        }
        jp67 = t69
    } else {
        jp67 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv65 = jp67
    return retv65
}

func check(ok__1 bool) Option__string {
    var retv71 Option__string
    var mtmp58 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    switch mtmp58.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv71 = Option__string_None{}
        return retv71
    case _goml_m_Option_____o_string_c_string_q__Some:
        var t74 Option__string = Option__string_Some{
            _0: "ok",
        }
        retv71 = t74
        return retv71
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    var retv76 string
    var jp78 string
    switch opt__2.(type) {
    case Option__string_None:
        jp78 = "none"
    case Option__string_Some:
        var x61 string = opt__2.(Option__string_Some)._0
        var value__3 string = x61
        var t79 string = "some " + value__3
        jp78 = t79
    default:
        panic("non-exhaustive match")
    }
    retv76 = jp78
    return retv76
}

func main0() struct{} {
    var t81 Option__string = check(true)
    var t82 string = show(t81)
    println__T_string(t82)
    var t83 Option__string = check(false)
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
