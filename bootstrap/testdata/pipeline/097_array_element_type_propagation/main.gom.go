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
    Loop_loop81:
    for {
        var t82 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t83 bool = t82 < 3
        if t83 {
            var t84 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t85 uint8 = array_get__Array_3_5uint8(arr__0, t84)
            var t86 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t85)
            println__T_string(t86)
            var t87 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t88 int = t87 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t88)
            continue
        } else {
            break Loop_loop81
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t73 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t74 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t73)
    println__T_string(t74)
    var t75 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t76 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t75)
    println__T_string(t76)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t77 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t78 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t77)
    println__T_string(t78)
    var t79 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t80 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t79)
    println__T_string(t80)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv90 *ref_int_x
    var t91 *ref_int_x = ref__Ref_3int(value__209)
    retv90 = t91
    return retv90
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv93 int
    var t94 int = ref_get__Ref_3int(self__210)
    retv93 = t94
    return retv93
}

func println__T_string(value__1 string) struct{} {
    var t96 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t96)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv99 string
    var t100 string = _goml_runtime_core_uint8_to_string(self__45)
    retv99 = t100
    return retv99
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv104 string
    var t105 string = _goml_runtime_core_float32_to_string(self__49)
    retv104 = t105
    return retv104
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv107 string
    var t108 string = _goml_runtime_core_int64_to_string(self__44)
    retv107 = t108
    return retv107
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv110 string
    retv110 = self__38
    return retv110
}

func main() {
    main0()
}
