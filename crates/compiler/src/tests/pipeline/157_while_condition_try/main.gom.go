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
    var retv36 Option__bool
    var t39 bool = i__0 < 3
    var jp38 Option__bool
    if t39 {
        var t40 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp38 = t40
    } else {
        var t41 Option__bool = Option__bool_Some{
            _0: false,
        }
        jp38 = t41
    }
    retv36 = jp38
    return retv36
}

func step_none(i__1 int32) Option__bool {
    var retv43 Option__bool
    var t46 bool = i__1 < 2
    var jp45 Option__bool
    if t46 {
        var t47 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp45 = t47
    } else {
        jp45 = Option__bool_None{}
    }
    retv43 = jp45
    return retv43
}

func run_some() Option__int32 {
    var retv49 Option__int32
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop53:
    for {
        var t54 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var mtmp22 Option__bool = step_some(t54)
        var jp56 bool
        switch mtmp22.(type) {
        case Option__bool_None:
            retv49 = Option__int32_None{}
            return retv49
        case Option__bool_Some:
            var x23 bool = mtmp22.(Option__bool_Some)._0
            var try_value__31 bool = x23
            jp56 = try_value__31
            if jp56 {
                var t57 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
                var t58 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t59 int32 = t57 + t58
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__3, t59)
                var t60 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t61 int32 = t60 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t61)
                continue
            } else {
                break Loop_loop53
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t51 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
    var t52 Option__int32 = Option__int32_Some{
        _0: t51,
    }
    retv49 = t52
    return retv49
}

func run_none() Option__int32 {
    var retv63 Option__int32
    var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop67:
    for {
        var t68 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
        var mtmp27 Option__bool = step_none(t68)
        var jp70 bool
        switch mtmp27.(type) {
        case Option__bool_None:
            retv63 = Option__int32_None{}
            return retv63
        case Option__bool_Some:
            var x28 bool = mtmp27.(Option__bool_Some)._0
            var try_value__67 bool = x28
            jp70 = try_value__67
            if jp70 {
                var t71 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t72 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t73 int32 = t71 + t72
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t73)
                var t74 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t75 int32 = t74 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t75)
                continue
            } else {
                break Loop_loop67
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t65 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    var t66 Option__int32 = Option__int32_Some{
        _0: t65,
    }
    retv63 = t66
    return retv63
}

func show(x__6 Option__int32) string {
    var retv77 string
    var jp79 string
    switch x__6.(type) {
    case Option__int32_None:
        jp79 = "none"
    case Option__int32_Some:
        var x32 int32 = x__6.(Option__int32_Some)._0
        var v__7 int32 = x32
        var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(v__7)
        var t81 string = "some=" + t80
        jp79 = t81
    default:
        panic("non-exhaustive match")
    }
    retv77 = jp79
    return retv77
}

func main0() struct{} {
    var t83 Option__int32 = run_some()
    var t84 string = show(t83)
    println__T_string(t84)
    var t85 Option__int32 = run_none()
    var t86 string = show(t85)
    println__T_string(t86)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv88 *ref_int32_x
    var t89 *ref_int32_x = ref__Ref_5int32(value__137)
    retv88 = t89
    return retv88
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv91 int32
    var t92 int32 = ref_get__Ref_5int32(self__138)
    retv91 = t92
    return retv91
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv96 string
    var t97 string = _goml_runtime_core_int32_to_string(self__2)
    retv96 = t97
    return retv96
}

func println__T_string(value__1 string) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv102 string
    retv102 = self__9
    return retv102
}

func main() {
    main0()
}
