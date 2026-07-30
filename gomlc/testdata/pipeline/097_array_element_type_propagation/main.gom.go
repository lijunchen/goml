package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_float32_to_string(x float32) string {
    var formatted string = _goml_strconv.FormatFloat(float64(x), 102, -1, 32)
    if formatted == "+Inf" {
        return "inf"
    }
    if formatted == "-Inf" {
        return "-inf"
    }
    return formatted
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_3_5uint8(arr [3]uint8, index int) uint8 {
    return arr[index]
}

func array_get__Array_2_7float32(arr [2]float32, index int) float32 {
    return arr[index]
}

func array_get__Array_2_5int64(arr [2]int64, index int) int64 {
    return arr[index]
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() struct{} {
    var arr__0 [3]uint8 = [3]uint8{10, 20, 30}
    var i__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop125:
    for {
        var t126 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t127 bool = t126 < 3
        if t127 {
            var t128 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t129 uint8 = array_get__Array_3_5uint8(arr__0, t128)
            var t130 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t129)
            println__T_string(t130)
            var t131 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t132 int = t131 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t132)
            continue
        } else {
            break Loop_loop125
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t117 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t118 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t117)
    println__T_string(t118)
    var t119 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t120 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t119)
    println__T_string(t120)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t121 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t122 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t121)
    println__T_string(t122)
    var t123 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t124 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t123)
    println__T_string(t124)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv134 *ref_int_x
    var t135 *ref_int_x = ref__Ref_3int(value__207)
    retv134 = t135
    return retv134
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv137 int
    var t138 int = ref_get__Ref_3int(self__208)
    retv137 = t138
    return retv137
}

func println__T_string(value__1 string) struct{} {
    var t140 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t140)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv143 string
    var t144 string = _goml_runtime_core_uint8_to_string(self__45)
    retv143 = t144
    return retv143
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv148 string
    var t149 string = _goml_runtime_core_float32_to_string(self__49)
    retv148 = t149
    return retv148
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv151 string
    var t152 string = _goml_runtime_core_int64_to_string(self__44)
    retv151 = t152
    return retv151
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv154 string
    retv154 = self__38
    return retv154
}

func main() {
    main0()
}
