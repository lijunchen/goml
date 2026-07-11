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
    var retv31 _goml_m_Option_____o_string_c_string_q_
    var jp33 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var t34 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t35 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t34,
        }
        jp33 = t35
    } else {
        jp33 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    retv31 = jp33
    return retv31
}

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    var retv37 _goml_m_Option_____o_string_c_string_q_
    var t38 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    retv37 = t38
    return retv37
}

func describe(ok__2 bool) Option__string {
    var retv40 Option__string
    var mtmp22 _goml_m_Option_____o_string_c_string_q_ = pair(ok__2)
    var jp42 Tuple2_6string_6string
    switch mtmp22.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv40 = Option__string_None{}
        return retv40
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x23 Tuple2_6string_6string = mtmp22.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__16 Tuple2_6string_6string = x23
        jp42 = try_value__16
        var mtmp24 Tuple2_6string_6string = jp42
        var x25 string = mtmp24._0
        var x26 string = mtmp24._1
        var after__4 string = x26
        var before__3 string = x25
        var t43 string = before__3 + ":"
        var t44 string = t43 + after__4
        var t45 Option__string = Option__string_Some{
            _0: t44,
        }
        retv40 = t45
        return retv40
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__string) string {
    var retv47 string
    var jp49 string
    switch opt__5.(type) {
    case Option__string_None:
        jp49 = "none"
    case Option__string_Some:
        var x27 string = opt__5.(Option__string_Some)._0
        var value__6 string = x27
        var t50 string = "some " + value__6
        jp49 = t50
    default:
        panic("non-exhaustive match")
    }
    retv47 = jp49
    return retv47
}

func main0() struct{} {
    var t52 Option__string = describe(true)
    var t53 string = show(t52)
    println__T_string(t53)
    var t54 Option__string = describe(false)
    var t55 string = show(t54)
    println__T_string(t55)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t57 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t57)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv60 string
    retv60 = self__9
    return retv60
}

func main() {
    main0()
}
