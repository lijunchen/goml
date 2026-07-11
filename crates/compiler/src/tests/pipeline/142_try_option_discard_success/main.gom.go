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
    var retv14 _goml_m_Option_____o_string_c_string_q_
    var jp16 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t17 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t18 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t17,
        }
        jp16 = t18
    } else {
        jp16 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv14 = jp16
    return retv14
}

func check(ok__1 bool) Option__string {
    var retv20 Option__string
    var mtmp7 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    switch mtmp7.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv20 = Option__string_None{}
        return retv20
    case _goml_m_Option_____o_string_c_string_q__Some:
        var t23 Option__string = Option__string_Some{
            _0: "ok",
        }
        retv20 = t23
        return retv20
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    var retv25 string
    var jp27 string
    switch opt__2.(type) {
    case Option__string_None:
        jp27 = "none"
    case Option__string_Some:
        var x10 string = opt__2.(Option__string_Some)._0
        var value__3 string = x10
        var t28 string = "some " + value__3
        jp27 = t28
    default:
        panic("non-exhaustive match")
    }
    retv25 = jp27
    return retv25
}

func main0() struct{} {
    var t30 Option__string = check(true)
    var t31 string = show(t30)
    println__T_string(t31)
    var t32 Option__string = check(false)
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
