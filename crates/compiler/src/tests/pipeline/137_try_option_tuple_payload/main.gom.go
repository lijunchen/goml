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
            _0: "alpha",
            _1: "beta",
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

func describe(ok__1 bool) Option__string {
    var retv37 Option__string
    var mtmp22 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__1)
    var jp39 Tuple2_6string_6string
    switch mtmp22.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        retv37 = Option__string_None{}
        return retv37
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x23 Tuple2_6string_6string = mtmp22.(_goml_m_Option_____o_string_c_string_q__Some)._0
        var try_value__13 Tuple2_6string_6string = x23
        jp39 = try_value__13
        var mtmp24 Tuple2_6string_6string = jp39
        var x25 string = mtmp24._0
        var x26 string = mtmp24._1
        var after__3 string = x26
        var before__2 string = x25
        var t40 string = before__2 + "|"
        var t41 string = t40 + after__3
        var t42 Option__string = Option__string_Some{
            _0: t41,
        }
        retv37 = t42
        return retv37
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__string) string {
    var retv44 string
    var jp46 string
    switch opt__4.(type) {
    case Option__string_None:
        jp46 = "none"
    case Option__string_Some:
        var x27 string = opt__4.(Option__string_Some)._0
        var value__5 string = x27
        var t47 string = "some " + value__5
        jp46 = t47
    default:
        panic("non-exhaustive match")
    }
    retv44 = jp46
    return retv44
}

func main0() struct{} {
    var t49 Option__string = describe(true)
    var t50 string = show(t49)
    println__T_string(t50)
    var t51 Option__string = describe(false)
    var t52 string = show(t51)
    println__T_string(t52)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t54 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t54)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv57 string
    retv57 = self__9
    return retv57
}

func main() {
    main0()
}
