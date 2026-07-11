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
    var retv11 _goml_m_Option_____o_string_c_string_q_
    var jp13 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t14 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t15 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t14,
        }
        jp13 = t15
    } else {
        jp13 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv11 = jp13
    return retv11
}

func check(ok__1 bool) Option__string {
    var retv17 Option__string
    var mtmp4 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    switch mtmp4.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv17 = Option__string_None{}
        return retv17
    case _goml_m_Option_____o_string_c_string_q__Some:
        var t20 Option__string = Option__string_Some{
            _0: "ok",
        }
        retv17 = t20
        return retv17
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    var retv22 string
    var jp24 string
    switch opt__2.(type) {
    case Option__string_None:
        jp24 = "none"
    case Option__string_Some:
        var x7 string = opt__2.(Option__string_Some)._0
        var value__3 string = x7
        var t25 string = "some " + value__3
        jp24 = t25
    default:
        panic("non-exhaustive match")
    }
    retv22 = jp24
    return retv22
}

func main0() struct{} {
    var t27 Option__string = check(true)
    var t28 string = show(t27)
    println__T_string(t28)
    var t29 Option__string = check(false)
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
