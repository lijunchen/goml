package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_float32_to_string(x float32) string {
    return _goml_fmt.Sprintf("%g", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_3_5uint8(arr [3]uint8, index int32) uint8 {
    return arr[index]
}

func array_get__Array_2_7float32(arr [2]float32, index int32) float32 {
    return arr[index]
}

func array_get__Array_2_5int64(arr [2]int64, index int32) int64 {
    return arr[index]
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
    var arr__0 [3]uint8 = [3]uint8{10, 20, 30}
    var i__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop39:
    for {
        var t40 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
        var t41 bool = t40 < 3
        if t41 {
            var t42 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t43 uint8 = array_get__Array_3_5uint8(arr__0, t42)
            var t44 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t43)
            println__T_string(t44)
            var t45 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t46 int32 = t45 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__1, t46)
            continue
        } else {
            break Loop_loop39
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t31 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t32 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t31)
    println__T_string(t32)
    var t33 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t34 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t33)
    println__T_string(t34)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t35 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t36 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t35)
    println__T_string(t36)
    var t37 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t38 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t37)
    println__T_string(t38)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv48 *ref_int32_x
    var t49 *ref_int32_x = ref__Ref_5int32(value__140)
    retv48 = t49
    return retv48
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv51 int32
    var t52 int32 = ref_get__Ref_5int32(self__141)
    retv51 = t52
    return retv51
}

func println__T_string(value__1 string) struct{} {
    var t54 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t54)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__15 uint8) string {
    var retv57 string
    var t58 string = _goml_runtime_core_uint8_to_string(self__15)
    retv57 = t58
    return retv57
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv62 string
    var t63 string = _goml_runtime_core_float32_to_string(self__19)
    retv62 = t63
    return retv62
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__14 int64) string {
    var retv65 string
    var t66 string = _goml_runtime_core_int64_to_string(self__14)
    retv65 = t66
    return retv65
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv68 string
    retv68 = self__9
    return retv68
}

func main() {
    main0()
}
