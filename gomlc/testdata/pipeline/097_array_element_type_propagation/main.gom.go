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
    Loop_loop169:
    for {
        var t170 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t171 bool = t170 < 3
        if t171 {
            var t172 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t173 uint8 = array_get__Array_3_5uint8(arr__0, t172)
            var t174 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t173)
            println__T_string(t174)
            var t175 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t176 int = t175 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t176)
            continue
        } else {
            break Loop_loop169
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t161 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t162 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t161)
    println__T_string(t162)
    var t163 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t164 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t163)
    println__T_string(t164)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t165 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t166 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t165)
    println__T_string(t166)
    var t167 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t168 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t167)
    println__T_string(t168)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv178 *ref_int_x
    var t179 *ref_int_x = ref__Ref_3int(value__207)
    retv178 = t179
    return retv178
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv181 int
    var t182 int = ref_get__Ref_3int(self__208)
    retv181 = t182
    return retv181
}

func println__T_string(value__1 string) struct{} {
    var t184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t184)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv187 string
    var t188 string = _goml_runtime_core_uint8_to_string(self__45)
    retv187 = t188
    return retv187
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv192 string
    var t193 string = _goml_runtime_core_float32_to_string(self__49)
    retv192 = t193
    return retv192
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv195 string
    var t196 string = _goml_runtime_core_int64_to_string(self__44)
    retv195 = t196
    return retv195
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv198 string
    retv198 = self__38
    return retv198
}

func main() {
    main0()
}
