package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type Option__bool interface {
    isOption__bool()
}

type Option__bool_None struct {}

func (_ Option__bool_None) isOption__bool() {}

type Option__bool_Some struct {
    _0 bool
}

func (_ Option__bool_Some) isOption__bool() {}

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

func step_some(i__0 int32) Option__bool {
    var retv72 Option__bool
    var t75 bool = i__0 < 3
    var jp74 Option__bool
    if t75 {
        var t76 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp74 = t76
    } else {
        var t77 Option__bool = Option__bool_Some{
            _0: false,
        }
        jp74 = t77
    }
    retv72 = jp74
    return retv72
}

func step_none(i__1 int32) Option__bool {
    var retv79 Option__bool
    var t82 bool = i__1 < 2
    var jp81 Option__bool
    if t82 {
        var t83 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp81 = t83
    } else {
        jp81 = Option__bool_None{}
    }
    retv79 = jp81
    return retv79
}

func run_some() Option__int32 {
    var retv85 Option__int32
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop89:
    for {
        var t90 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var mtmp58 Option__bool = step_some(t90)
        var jp92 bool
        switch mtmp58.(type) {
        case Option__bool_None:
            retv85 = Option__int32_None{}
            return retv85
        case Option__bool_Some:
            var x59 bool = mtmp58.(Option__bool_Some)._0
            var try_value__31 bool = x59
            jp92 = try_value__31
            if jp92 {
                var t93 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
                var t94 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t95 int32 = t93 + t94
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__3, t95)
                var t96 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t97 int32 = t96 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t97)
                continue
            } else {
                break Loop_loop89
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t87 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
    var t88 Option__int32 = Option__int32_Some{
        _0: t87,
    }
    retv85 = t88
    return retv85
}

func run_none() Option__int32 {
    var retv99 Option__int32
    var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop103:
    for {
        var t104 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
        var mtmp63 Option__bool = step_none(t104)
        var jp106 bool
        switch mtmp63.(type) {
        case Option__bool_None:
            retv99 = Option__int32_None{}
            return retv99
        case Option__bool_Some:
            var x64 bool = mtmp63.(Option__bool_Some)._0
            var try_value__67 bool = x64
            jp106 = try_value__67
            if jp106 {
                var t107 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t108 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t109 int32 = t107 + t108
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t109)
                var t110 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t111 int32 = t110 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t111)
                continue
            } else {
                break Loop_loop103
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t101 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    var t102 Option__int32 = Option__int32_Some{
        _0: t101,
    }
    retv99 = t102
    return retv99
}

func show(x__6 Option__int32) string {
    var retv113 string
    var jp115 string
    switch x__6.(type) {
    case Option__int32_None:
        jp115 = "none"
    case Option__int32_Some:
        var x68 int32 = x__6.(Option__int32_Some)._0
        var v__7 int32 = x68
        var t116 string = _goml_m_inherent_i_int32_i_int32_i_to__string(v__7)
        var t117 string = "some=" + t116
        jp115 = t117
    default:
        panic("non-exhaustive match")
    }
    retv113 = jp115
    return retv113
}

func main0() struct{} {
    var t119 Option__int32 = run_some()
    var t120 string = show(t119)
    println__T_string(t120)
    var t121 Option__int32 = run_none()
    var t122 string = show(t121)
    println__T_string(t122)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv124 *ref_int32_x
    var t125 *ref_int32_x = ref__Ref_5int32(value__201)
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv127 int32
    var t128 int32 = ref_get__Ref_5int32(self__202)
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv132 string
    var t133 string = _goml_runtime_core_int32_to_string(self__2)
    retv132 = t133
    return retv132
}

func println__T_string(value__1 string) struct{} {
    var t135 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t135)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv138 string
    retv138 = self__34
    return retv138
}

func main() {
    main0()
}
