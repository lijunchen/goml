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
    Loop_loop49:
    for {
        var t50 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
        var t51 bool = t50 <= 100
        if t51 {
            var t58 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t59 bool = t58 == 50
            if t59 {
                break Loop_loop49
            } else {
                var t53 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__0)
                var t54 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
                var t55 int32 = t53 + t54
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__0, t55)
                var t56 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
                var t57 int32 = t56 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__1, t57)
                continue
            }
        } else {
            break Loop_loop49
        }
    }
    print__T_string("sum up to break: ")
    var t36 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__0)
    println__T_int32(t36)
    var even_sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var j__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    Loop_loop39:
    for {
        var t40 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
        var t41 bool = t40 <= 10
        if t41 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var t42 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__3, t42)
            var t44 int32 = cur__4 / 2
            var t45 int32 = t44 * 2
            var t46 bool = cur__4 == t45
            if t46 {
                var t47 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(even_sum__2)
                var t48 int32 = t47 + cur__4
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(even_sum__2, t48)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop39
        }
    }
    print__T_string("even sum: ")
    var t38 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(even_sum__2)
    println__T_int32(t38)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv61 *ref_int32_x
    var t62 *ref_int32_x = ref__Ref_5int32(value__140)
    retv61 = t62
    return retv61
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv64 int32
    var t65 int32 = ref_get__Ref_5int32(self__141)
    retv64 = t65
    return retv64
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t69)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv75 string
    retv75 = self__9
    return retv75
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv77 string
    var t78 string = _goml_runtime_core_int32_to_string(self__13)
    retv77 = t78
    return retv77
}

func main() {
    main0()
}
