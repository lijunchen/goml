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
    Loop_loop21:
    for {
        var t22 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
        var t23 bool = t22 < 3
        if t23 {
            var t24 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t25 uint8 = array_get__Array_3_5uint8(arr__0, t24)
            var t26 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t25)
            println__T_string(t26)
            var t27 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t28 int32 = t27 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__1, t28)
            continue
        } else {
            break Loop_loop21
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t13 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t14 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t13)
    println__T_string(t14)
    var t15 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t16 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t15)
    println__T_string(t16)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t17 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t18 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t17)
    println__T_string(t18)
    var t19 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t20 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t19)
    println__T_string(t20)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv30 *ref_int32_x
    var t31 *ref_int32_x = ref__Ref_5int32(value__102)
    retv30 = t31
    return retv30
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv33 int32
    var t34 int32 = ref_get__Ref_5int32(self__103)
    retv33 = t34
    return retv33
}

func println__T_string(value__1 string) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__15 uint8) string {
    var retv39 string
    var t40 string = _goml_runtime_core_uint8_to_string(self__15)
    retv39 = t40
    return retv39
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv44 string
    var t45 string = _goml_runtime_core_float32_to_string(self__19)
    retv44 = t45
    return retv44
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__14 int64) string {
    var retv47 string
    var t48 string = _goml_runtime_core_int64_to_string(self__14)
    retv47 = t48
    return retv47
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv50 string
    retv50 = self__9
    return retv50
}

func main() {
    main0()
}
