package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
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

func main0() struct{} {
    var sum__0 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    Loop_loop31:
    for {
        var t32 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
        var t33 bool = t32 <= 100
        if t33 {
            var t40 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t41 bool = t40 == 50
            if t41 {
                break Loop_loop31
            } else {
                var t35 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__0)
                var t36 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
                var t37 int32 = t35 + t36
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__0, t37)
                var t38 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
                var t39 int32 = t38 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__1, t39)
                continue
            }
        } else {
            break Loop_loop31
        }
    }
    print__T_string("sum up to break: ")
    var t18 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__0)
    println__T_int32(t18)
    var even_sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var j__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    Loop_loop21:
    for {
        var t22 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
        var t23 bool = t22 <= 10
        if t23 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var t24 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__3, t24)
            var t26 int32 = cur__4 / 2
            var t27 int32 = t26 * 2
            var t28 bool = cur__4 == t27
            if t28 {
                var t29 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(even_sum__2)
                var t30 int32 = t29 + cur__4
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(even_sum__2, t30)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop21
        }
    }
    print__T_string("even sum: ")
    var t20 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(even_sum__2)
    println__T_int32(t20)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv43 *ref_int32_x
    var t44 *ref_int32_x = ref__Ref_5int32(value__102)
    retv43 = t44
    return retv43
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv46 int32
    var t47 int32 = ref_get__Ref_5int32(self__103)
    retv46 = t47
    return retv46
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t51 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t51)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t54 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t54)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv57 string
    retv57 = self__9
    return retv57
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv59 string
    var t60 string = _goml_runtime_core_int32_to_string(self__13)
    retv59 = t60
    return retv59
}

func main() {
    main0()
}
