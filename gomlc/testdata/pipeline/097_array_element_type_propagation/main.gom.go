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
    Loop_loop172:
    for {
        var t173 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t174 bool = t173 < 3
        if t174 {
            var t175 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t176 uint8 = array_get__Array_3_5uint8(arr__0, t175)
            var t177 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t176)
            println__T_string(t177)
            var t178 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t179 int = t178 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t179)
            continue
        } else {
            break Loop_loop172
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t164 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t165 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t164)
    println__T_string(t165)
    var t166 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t167 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t166)
    println__T_string(t167)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t168 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t169 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t168)
    println__T_string(t169)
    var t170 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t171 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t170)
    println__T_string(t171)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv181 *ref_int_x
    var t182 *ref_int_x = ref__Ref_3int(value__207)
    retv181 = t182
    return retv181
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv184 int
    var t185 int = ref_get__Ref_3int(self__208)
    retv184 = t185
    return retv184
}

func println__T_string(value__1 string) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv190 string
    var t191 string = _goml_runtime_core_uint8_to_string(self__45)
    retv190 = t191
    return retv190
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv195 string
    var t196 string = _goml_runtime_core_float32_to_string(self__49)
    retv195 = t196
    return retv195
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv198 string
    var t199 string = _goml_runtime_core_int64_to_string(self__44)
    retv198 = t199
    return retv198
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv201 string
    retv201 = self__38
    return retv201
}

func main() {
    main0()
}
