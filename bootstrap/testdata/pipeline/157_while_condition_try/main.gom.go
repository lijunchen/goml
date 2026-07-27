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
    var retv78 Option__bool
    var t81 bool = i__0 < 3
    var jp80 Option__bool
    if t81 {
        var t82 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp80 = t82
    } else {
        var t83 Option__bool = Option__bool_Some{
            _0: false,
        }
        jp80 = t83
    }
    retv78 = jp80
    return retv78
}

func step_none(i__1 int32) Option__bool {
    var retv85 Option__bool
    var t88 bool = i__1 < 2
    var jp87 Option__bool
    if t88 {
        var t89 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp87 = t89
    } else {
        jp87 = Option__bool_None{}
    }
    retv85 = jp87
    return retv85
}

func run_some() Option__int32 {
    var retv91 Option__int32
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop95:
    for {
        var t96 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var mtmp64 Option__bool = step_some(t96)
        var jp98 bool
        switch mtmp64.(type) {
        case Option__bool_None:
            retv91 = Option__int32_None{}
            return retv91
        case Option__bool_Some:
            var x65 bool = mtmp64.(Option__bool_Some)._0
            var try_value__31 bool = x65
            jp98 = try_value__31
            if jp98 {
                var t99 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
                var t100 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t101 int32 = t99 + t100
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__3, t101)
                var t102 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t103 int32 = t102 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t103)
                continue
            } else {
                break Loop_loop95
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t93 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
    var t94 Option__int32 = Option__int32_Some{
        _0: t93,
    }
    retv91 = t94
    return retv91
}

func run_none() Option__int32 {
    var retv105 Option__int32
    var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop109:
    for {
        var t110 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
        var mtmp69 Option__bool = step_none(t110)
        var jp112 bool
        switch mtmp69.(type) {
        case Option__bool_None:
            retv105 = Option__int32_None{}
            return retv105
        case Option__bool_Some:
            var x70 bool = mtmp69.(Option__bool_Some)._0
            var try_value__67 bool = x70
            jp112 = try_value__67
            if jp112 {
                var t113 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t114 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t115 int32 = t113 + t114
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t115)
                var t116 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t117 int32 = t116 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t117)
                continue
            } else {
                break Loop_loop109
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t107 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    var t108 Option__int32 = Option__int32_Some{
        _0: t107,
    }
    retv105 = t108
    return retv105
}

func show(x__6 Option__int32) string {
    var retv119 string
    var jp121 string
    switch x__6.(type) {
    case Option__int32_None:
        jp121 = "none"
    case Option__int32_Some:
        var x74 int32 = x__6.(Option__int32_Some)._0
        var v__7 int32 = x74
        var t122 string = _goml_m_inherent_i_int32_i_int32_i_to__string(v__7)
        var t123 string = "some=" + t122
        jp121 = t123
    default:
        panic("non-exhaustive match")
    }
    retv119 = jp121
    return retv119
}

func main0() struct{} {
    var t125 Option__int32 = run_some()
    var t126 string = show(t125)
    println__T_string(t126)
    var t127 Option__int32 = run_none()
    var t128 string = show(t127)
    println__T_string(t128)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv130 *ref_int32_x
    var t131 *ref_int32_x = ref__Ref_5int32(value__209)
    retv130 = t131
    return retv130
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv133 int32
    var t134 int32 = ref_get__Ref_5int32(self__210)
    retv133 = t134
    return retv133
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv138 string
    var t139 string = _goml_runtime_core_int32_to_string(self__6)
    retv138 = t139
    return retv138
}

func println__T_string(value__1 string) struct{} {
    var t141 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t141)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv144 string
    retv144 = self__38
    return retv144
}

func main() {
    main0()
}
