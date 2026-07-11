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
    var retv18 Option__bool
    var t21 bool = i__0 < 3
    var jp20 Option__bool
    if t21 {
        var t22 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp20 = t22
    } else {
        var t23 Option__bool = Option__bool_Some{
            _0: false,
        }
        jp20 = t23
    }
    retv18 = jp20
    return retv18
}

func step_none(i__1 int32) Option__bool {
    var retv25 Option__bool
    var t28 bool = i__1 < 2
    var jp27 Option__bool
    if t28 {
        var t29 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp27 = t29
    } else {
        jp27 = Option__bool_None{}
    }
    retv25 = jp27
    return retv25
}

func run_some() Option__int32 {
    var retv31 Option__int32
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop35:
    for {
        var t36 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var mtmp4 Option__bool = step_some(t36)
        var jp38 bool
        switch mtmp4.(type) {
        case Option__bool_None:
            retv31 = Option__int32_None{}
            return retv31
        case Option__bool_Some:
            var x5 bool = mtmp4.(Option__bool_Some)._0
            var try_value__31 bool = x5
            jp38 = try_value__31
            if jp38 {
                var t39 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
                var t40 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t41 int32 = t39 + t40
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__3, t41)
                var t42 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t43 int32 = t42 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t43)
                continue
            } else {
                break Loop_loop35
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t33 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
    var t34 Option__int32 = Option__int32_Some{
        _0: t33,
    }
    retv31 = t34
    return retv31
}

func run_none() Option__int32 {
    var retv45 Option__int32
    var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop49:
    for {
        var t50 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
        var mtmp9 Option__bool = step_none(t50)
        var jp52 bool
        switch mtmp9.(type) {
        case Option__bool_None:
            retv45 = Option__int32_None{}
            return retv45
        case Option__bool_Some:
            var x10 bool = mtmp9.(Option__bool_Some)._0
            var try_value__67 bool = x10
            jp52 = try_value__67
            if jp52 {
                var t53 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t54 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t55 int32 = t53 + t54
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t55)
                var t56 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t57 int32 = t56 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t57)
                continue
            } else {
                break Loop_loop49
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t47 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    var t48 Option__int32 = Option__int32_Some{
        _0: t47,
    }
    retv45 = t48
    return retv45
}

func show(x__6 Option__int32) string {
    var retv59 string
    var jp61 string
    switch x__6.(type) {
    case Option__int32_None:
        jp61 = "none"
    case Option__int32_Some:
        var x14 int32 = x__6.(Option__int32_Some)._0
        var v__7 int32 = x14
        var t62 string = _goml_m_inherent_i_int32_i_int32_i_to__string(v__7)
        var t63 string = "some=" + t62
        jp61 = t63
    default:
        panic("non-exhaustive match")
    }
    retv59 = jp61
    return retv59
}

func main0() struct{} {
    var t65 Option__int32 = run_some()
    var t66 string = show(t65)
    println__T_string(t66)
    var t67 Option__int32 = run_none()
    var t68 string = show(t67)
    println__T_string(t68)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv70 *ref_int32_x
    var t71 *ref_int32_x = ref__Ref_5int32(value__102)
    retv70 = t71
    return retv70
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv73 int32
    var t74 int32 = ref_get__Ref_5int32(self__103)
    retv73 = t74
    return retv73
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv78 string
    var t79 string = _goml_runtime_core_int32_to_string(self__2)
    retv78 = t79
    return retv78
}

func println__T_string(value__1 string) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv84 string
    retv84 = self__9
    return retv84
}

func main() {
    main0()
}
