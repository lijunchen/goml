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

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func step(i__0 int32) Option__int32 {
    var retv17 Option__int32
    var t20 bool = i__0 == 2
    var jp19 Option__int32
    if t20 {
        jp19 = None{}
    } else {
        var t21 int32 = i__0 + 10
        var t22 Option__int32 = Some{
            _0: t21,
        }
        jp19 = t22
    }
    retv17 = jp19
    return retv17
}

func accumulate(limit__1 int32) Option__int32 {
    var retv24 Option__int32
    var sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop28:
    for {
        var t29 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
        var t30 bool = t29 < limit__1
        if t30 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
            var t31 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__3, t31)
            var t37 bool = cur__4 == 1
            if t37 {
                continue
            } else {
                var mtmp9 Option__int32 = step(cur__4)
                var jp34 int32
                switch mtmp9.(type) {
                case None:
                    retv24 = None{}
                    return retv24
                case Some:
                    var x10 int32 = mtmp9.(Some)._0
                    var try_value__43 int32 = x10
                    jp34 = try_value__43
                    var value__5 int32 = jp34
                    var t35 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
                    var t36 int32 = t35 + value__5
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__2, t36)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop28
        }
    }
    var t26 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
    var t27 Option__int32 = Some{
        _0: t26,
    }
    retv24 = t27
    return retv24
}

func show(opt__6 Option__int32) string {
    var retv39 string
    var jp41 string
    switch opt__6.(type) {
    case None:
        jp41 = "none"
    case Some:
        var x13 int32 = opt__6.(Some)._0
        var value__7 int32 = x13
        var t42 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t43 string = "some=" + t42
        jp41 = t43
    default:
        panic("non-exhaustive match")
    }
    retv39 = jp41
    return retv39
}

func main0() struct{} {
    var t45 Option__int32 = accumulate(2)
    var t46 string = show(t45)
    println__T_string(t46)
    var t47 Option__int32 = accumulate(4)
    var t48 string = show(t47)
    println__T_string(t48)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv50 *ref_int32_x
    var t51 *ref_int32_x = ref__Ref_5int32(value__114)
    retv50 = t51
    return retv50
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv53 int32
    var t54 int32 = ref_get__Ref_5int32(self__115)
    retv53 = t54
    return retv53
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv58 string
    var t59 string = _goml_runtime_core_int32_to_string(self__2)
    retv58 = t59
    return retv58
}

func println__T_string(value__1 string) struct{} {
    var t61 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t61)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv64 string
    retv64 = self__9
    return retv64
}

func main() {
    main0()
}
