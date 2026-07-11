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
    var retv29 _goml_m_Option_____o_string_c_string_q_
    var jp31 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t32 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t33 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t32,
        }
        jp31 = t33
    } else {
        jp31 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv29 = jp31
    return retv29
}

func check(ok__1 bool) Option__string {
    var retv35 Option__string
    var mtmp22 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    switch mtmp22.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv35 = Option__string_None{}
        return retv35
    case _goml_m_Option_____o_string_c_string_q__Some:
        var t38 Option__string = Option__string_Some{
            _0: "ok",
        }
        retv35 = t38
        return retv35
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    var retv40 string
    var jp42 string
    switch opt__2.(type) {
    case Option__string_None:
        jp42 = "none"
    case Option__string_Some:
        var x25 string = opt__2.(Option__string_Some)._0
        var value__3 string = x25
        var t43 string = "some " + value__3
        jp42 = t43
    default:
        panic("non-exhaustive match")
    }
    retv40 = jp42
    return retv40
}

func main0() struct{} {
    var t45 Option__string = check(true)
    var t46 string = show(t45)
    println__T_string(t46)
    var t47 Option__string = check(false)
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
