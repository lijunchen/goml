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
    Loop_loop85:
    for {
        var t86 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
        var t87 bool = t86 < 3
        if t87 {
            var t88 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t89 uint8 = array_get__Array_3_5uint8(arr__0, t88)
            var t90 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t89)
            println__T_string(t90)
            var t91 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__1)
            var t92 int = t91 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__1, t92)
            continue
        } else {
            break Loop_loop85
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t77 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t78 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t77)
    println__T_string(t78)
    var t79 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t80 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t79)
    println__T_string(t80)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t81 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t82 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t81)
    println__T_string(t82)
    var t83 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t84 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t83)
    println__T_string(t84)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv94 *ref_int_x
    var t95 *ref_int_x = ref__Ref_3int(value__207)
    retv94 = t95
    return retv94
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv97 int
    var t98 int = ref_get__Ref_3int(self__208)
    retv97 = t98
    return retv97
}

func println__T_string(value__1 string) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv103 string
    var t104 string = _goml_runtime_core_uint8_to_string(self__45)
    retv103 = t104
    return retv103
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv108 string
    var t109 string = _goml_runtime_core_float32_to_string(self__49)
    retv108 = t109
    return retv108
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv111 string
    var t112 string = _goml_runtime_core_int64_to_string(self__44)
    retv111 = t112
    return retv111
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv114 string
    retv114 = self__38
    return retv114
}

func main() {
    main0()
}
