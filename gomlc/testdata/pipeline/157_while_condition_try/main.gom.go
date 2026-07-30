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
    var retv82 Option__bool
    var t85 bool = i__0 < 3
    var jp84 Option__bool
    if t85 {
        var t86 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp84 = t86
    } else {
        var t87 Option__bool = Option__bool_Some{
            _0: false,
        }
        jp84 = t87
    }
    retv82 = jp84
    return retv82
}

func step_none(i__1 int32) Option__bool {
    var retv89 Option__bool
    var t92 bool = i__1 < 2
    var jp91 Option__bool
    if t92 {
        var t93 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp91 = t93
    } else {
        jp91 = Option__bool_None{}
    }
    retv89 = jp91
    return retv89
}

func run_some() Option__int32 {
    var retv95 Option__int32
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop99:
    for {
        var t100 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var mtmp68 Option__bool = step_some(t100)
        var jp102 bool
        switch mtmp68.(type) {
        case Option__bool_None:
            retv95 = Option__int32_None{}
            return retv95
        case Option__bool_Some:
            var x69 bool = mtmp68.(Option__bool_Some)._0
            var try_value__31 bool = x69
            jp102 = try_value__31
            if jp102 {
                var t103 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
                var t104 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t105 int32 = t103 + t104
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__3, t105)
                var t106 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t107 int32 = t106 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t107)
                continue
            } else {
                break Loop_loop99
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t97 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
    var t98 Option__int32 = Option__int32_Some{
        _0: t97,
    }
    retv95 = t98
    return retv95
}

func run_none() Option__int32 {
    var retv109 Option__int32
    var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop113:
    for {
        var t114 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
        var mtmp73 Option__bool = step_none(t114)
        var jp116 bool
        switch mtmp73.(type) {
        case Option__bool_None:
            retv109 = Option__int32_None{}
            return retv109
        case Option__bool_Some:
            var x74 bool = mtmp73.(Option__bool_Some)._0
            var try_value__67 bool = x74
            jp116 = try_value__67
            if jp116 {
                var t117 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t118 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t119 int32 = t117 + t118
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t119)
                var t120 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t121 int32 = t120 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t121)
                continue
            } else {
                break Loop_loop113
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t111 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    var t112 Option__int32 = Option__int32_Some{
        _0: t111,
    }
    retv109 = t112
    return retv109
}

func show(x__6 Option__int32) string {
    var retv123 string
    var jp125 string
    switch x__6.(type) {
    case Option__int32_None:
        jp125 = "none"
    case Option__int32_Some:
        var x78 int32 = x__6.(Option__int32_Some)._0
        var v__7 int32 = x78
        var t126 string = _goml_m_inherent_i_int32_i_int32_i_to__string(v__7)
        var t127 string = "some=" + t126
        jp125 = t127
    default:
        panic("non-exhaustive match")
    }
    retv123 = jp125
    return retv123
}

func main0() struct{} {
    var t129 Option__int32 = run_some()
    var t130 string = show(t129)
    println__T_string(t130)
    var t131 Option__int32 = run_none()
    var t132 string = show(t131)
    println__T_string(t132)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv134 *ref_int32_x
    var t135 *ref_int32_x = ref__Ref_5int32(value__207)
    retv134 = t135
    return retv134
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv137 int32
    var t138 int32 = ref_get__Ref_5int32(self__208)
    retv137 = t138
    return retv137
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv142 string
    var t143 string = _goml_runtime_core_int32_to_string(self__6)
    retv142 = t143
    return retv142
}

func println__T_string(value__1 string) struct{} {
    var t145 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t145)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv148 string
    retv148 = self__38
    return retv148
}

func main() {
    main0()
}
