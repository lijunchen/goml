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
    var retv14 Option__int32
    var t17 bool = i__0 == 2
    var jp16 Option__int32
    if t17 {
        jp16 = None{}
    } else {
        var t18 int32 = i__0 + 10
        var t19 Option__int32 = Some{
            _0: t18,
        }
        jp16 = t19
    }
    retv14 = jp16
    return retv14
}

func accumulate(limit__1 int32) Option__int32 {
    var retv21 Option__int32
    var sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop25:
    for {
        var t26 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
        var t27 bool = t26 < limit__1
        if t27 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
            var t28 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__3, t28)
            var t34 bool = cur__4 == 1
            if t34 {
                continue
            } else {
                var mtmp6 Option__int32 = step(cur__4)
                var jp31 int32
                switch mtmp6.(type) {
                case None:
                    retv21 = None{}
                    return retv21
                case Some:
                    var x7 int32 = mtmp6.(Some)._0
                    var try_value__43 int32 = x7
                    jp31 = try_value__43
                    var value__5 int32 = jp31
                    var t32 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
                    var t33 int32 = t32 + value__5
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__2, t33)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop25
        }
    }
    var t23 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
    var t24 Option__int32 = Some{
        _0: t23,
    }
    retv21 = t24
    return retv21
}

func show(opt__6 Option__int32) string {
    var retv36 string
    var jp38 string
    switch opt__6.(type) {
    case None:
        jp38 = "none"
    case Some:
        var x10 int32 = opt__6.(Some)._0
        var value__7 int32 = x10
        var t39 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t40 string = "some=" + t39
        jp38 = t40
    default:
        panic("non-exhaustive match")
    }
    retv36 = jp38
    return retv36
}

func main0() struct{} {
    var t42 Option__int32 = accumulate(2)
    var t43 string = show(t42)
    println__T_string(t43)
    var t44 Option__int32 = accumulate(4)
    var t45 string = show(t44)
    println__T_string(t45)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv47 *ref_int32_x
    var t48 *ref_int32_x = ref__Ref_5int32(value__102)
    retv47 = t48
    return retv47
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv50 int32
    var t51 int32 = ref_get__Ref_5int32(self__103)
    retv50 = t51
    return retv50
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv55 string
    var t56 string = _goml_runtime_core_int32_to_string(self__2)
    retv55 = t56
    return retv55
}

func println__T_string(value__1 string) struct{} {
    var t58 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t58)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv61 string
    retv61 = self__9
    return retv61
}

func main() {
    main0()
}
