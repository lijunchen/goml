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
    var retv21 Option__bool
    var t24 bool = i__0 < 3
    var jp23 Option__bool
    if t24 {
        var t25 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp23 = t25
    } else {
        var t26 Option__bool = Option__bool_Some{
            _0: false,
        }
        jp23 = t26
    }
    retv21 = jp23
    return retv21
}

func step_none(i__1 int32) Option__bool {
    var retv28 Option__bool
    var t31 bool = i__1 < 2
    var jp30 Option__bool
    if t31 {
        var t32 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp30 = t32
    } else {
        jp30 = Option__bool_None{}
    }
    retv28 = jp30
    return retv28
}

func run_some() Option__int32 {
    var retv34 Option__int32
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop38:
    for {
        var t39 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var mtmp7 Option__bool = step_some(t39)
        var jp41 bool
        switch mtmp7.(type) {
        case Option__bool_None:
            retv34 = Option__int32_None{}
            return retv34
        case Option__bool_Some:
            var x8 bool = mtmp7.(Option__bool_Some)._0
            var try_value__31 bool = x8
            jp41 = try_value__31
            if jp41 {
                var t42 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
                var t43 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t44 int32 = t42 + t43
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__3, t44)
                var t45 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t46 int32 = t45 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t46)
                continue
            } else {
                break Loop_loop38
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t36 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
    var t37 Option__int32 = Option__int32_Some{
        _0: t36,
    }
    retv34 = t37
    return retv34
}

func run_none() Option__int32 {
    var retv48 Option__int32
    var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop52:
    for {
        var t53 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
        var mtmp12 Option__bool = step_none(t53)
        var jp55 bool
        switch mtmp12.(type) {
        case Option__bool_None:
            retv48 = Option__int32_None{}
            return retv48
        case Option__bool_Some:
            var x13 bool = mtmp12.(Option__bool_Some)._0
            var try_value__67 bool = x13
            jp55 = try_value__67
            if jp55 {
                var t56 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t57 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t58 int32 = t56 + t57
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t58)
                var t59 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t60 int32 = t59 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t60)
                continue
            } else {
                break Loop_loop52
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t50 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    var t51 Option__int32 = Option__int32_Some{
        _0: t50,
    }
    retv48 = t51
    return retv48
}

func show(x__6 Option__int32) string {
    var retv62 string
    var jp64 string
    switch x__6.(type) {
    case Option__int32_None:
        jp64 = "none"
    case Option__int32_Some:
        var x17 int32 = x__6.(Option__int32_Some)._0
        var v__7 int32 = x17
        var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(v__7)
        var t66 string = "some=" + t65
        jp64 = t66
    default:
        panic("non-exhaustive match")
    }
    retv62 = jp64
    return retv62
}

func main0() struct{} {
    var t68 Option__int32 = run_some()
    var t69 string = show(t68)
    println__T_string(t69)
    var t70 Option__int32 = run_none()
    var t71 string = show(t70)
    println__T_string(t71)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv73 *ref_int32_x
    var t74 *ref_int32_x = ref__Ref_5int32(value__114)
    retv73 = t74
    return retv73
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv76 int32
    var t77 int32 = ref_get__Ref_5int32(self__115)
    retv76 = t77
    return retv76
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int32_to_string(self__2)
    retv81 = t82
    return retv81
}

func println__T_string(value__1 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv87 string
    retv87 = self__9
    return retv87
}

func main() {
    main0()
}
