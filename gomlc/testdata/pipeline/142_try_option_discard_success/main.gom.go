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
    var retv159 _goml_m_Option_____o_string_c_string_q_
    var jp161 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t162 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t163 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t162,
        }
        jp161 = t163
    } else {
        jp161 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv159 = jp161
    return retv159
}

func check(ok__1 bool) Option__string {
    var retv165 Option__string
    var mtmp152 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    switch mtmp152.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv165 = Option__string_None{}
        return retv165
    case _goml_m_Option_____o_string_c_string_q__Some:
        var t168 Option__string = Option__string_Some{
            _0: "ok",
        }
        retv165 = t168
        return retv165
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__2 Option__string) string {
    var retv170 string
    var jp172 string
    switch opt__2.(type) {
    case Option__string_None:
        jp172 = "none"
    case Option__string_Some:
        var x155 string = opt__2.(Option__string_Some)._0
        var value__3 string = x155
        var t173 string = "some " + value__3
        jp172 = t173
    default:
        panic("non-exhaustive match")
    }
    retv170 = jp172
    return retv170
}

func main0() struct{} {
    var t175 Option__string = check(true)
    var t176 string = show(t175)
    println__T_string(t176)
    var t177 Option__string = check(false)
    var t178 string = show(t177)
    println__T_string(t178)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t180 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t180)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv183 string
    retv183 = self__38
    return retv183
}

func main() {
    main0()
}
