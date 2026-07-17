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
    var retv75 Option__bool
    var t78 bool = i__0 < 3
    var jp77 Option__bool
    if t78 {
        var t79 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp77 = t79
    } else {
        var t80 Option__bool = Option__bool_Some{
            _0: false,
        }
        jp77 = t80
    }
    retv75 = jp77
    return retv75
}

func step_none(i__1 int32) Option__bool {
    var retv82 Option__bool
    var t85 bool = i__1 < 2
    var jp84 Option__bool
    if t85 {
        var t86 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp84 = t86
    } else {
        jp84 = Option__bool_None{}
    }
    retv82 = jp84
    return retv82
}

func run_some() Option__int32 {
    var retv88 Option__int32
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop92:
    for {
        var t93 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var mtmp61 Option__bool = step_some(t93)
        var jp95 bool
        switch mtmp61.(type) {
        case Option__bool_None:
            retv88 = Option__int32_None{}
            return retv88
        case Option__bool_Some:
            var x62 bool = mtmp61.(Option__bool_Some)._0
            var try_value__31 bool = x62
            jp95 = try_value__31
            if jp95 {
                var t96 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
                var t97 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t98 int32 = t96 + t97
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__3, t98)
                var t99 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t100 int32 = t99 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t100)
                continue
            } else {
                break Loop_loop92
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t90 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
    var t91 Option__int32 = Option__int32_Some{
        _0: t90,
    }
    retv88 = t91
    return retv88
}

func run_none() Option__int32 {
    var retv102 Option__int32
    var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop106:
    for {
        var t107 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
        var mtmp66 Option__bool = step_none(t107)
        var jp109 bool
        switch mtmp66.(type) {
        case Option__bool_None:
            retv102 = Option__int32_None{}
            return retv102
        case Option__bool_Some:
            var x67 bool = mtmp66.(Option__bool_Some)._0
            var try_value__67 bool = x67
            jp109 = try_value__67
            if jp109 {
                var t110 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t111 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t112 int32 = t110 + t111
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t112)
                var t113 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t114 int32 = t113 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t114)
                continue
            } else {
                break Loop_loop106
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t104 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    var t105 Option__int32 = Option__int32_Some{
        _0: t104,
    }
    retv102 = t105
    return retv102
}

func show(x__6 Option__int32) string {
    var retv116 string
    var jp118 string
    switch x__6.(type) {
    case Option__int32_None:
        jp118 = "none"
    case Option__int32_Some:
        var x71 int32 = x__6.(Option__int32_Some)._0
        var v__7 int32 = x71
        var t119 string = _goml_m_inherent_i_int32_i_int32_i_to__string(v__7)
        var t120 string = "some=" + t119
        jp118 = t120
    default:
        panic("non-exhaustive match")
    }
    retv116 = jp118
    return retv116
}

func main0() struct{} {
    var t122 Option__int32 = run_some()
    var t123 string = show(t122)
    println__T_string(t123)
    var t124 Option__int32 = run_none()
    var t125 string = show(t124)
    println__T_string(t125)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv127 *ref_int32_x
    var t128 *ref_int32_x = ref__Ref_5int32(value__204)
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv130 int32
    var t131 int32 = ref_get__Ref_5int32(self__205)
    retv130 = t131
    return retv130
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv135 string
    var t136 string = _goml_runtime_core_int32_to_string(self__5)
    retv135 = t136
    return retv135
}

func println__T_string(value__1 string) struct{} {
    var t138 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t138)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv141 string
    retv141 = self__37
    return retv141
}

func main() {
    main0()
}
