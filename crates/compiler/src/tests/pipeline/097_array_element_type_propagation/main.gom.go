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
    Loop_loop78:
    for {
        var t79 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
        var t80 bool = t79 < 3
        if t80 {
            var t81 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t82 uint8 = array_get__Array_3_5uint8(arr__0, t81)
            var t83 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t82)
            println__T_string(t83)
            var t84 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t85 int32 = t84 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__1, t85)
            continue
        } else {
            break Loop_loop78
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t70 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t71 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t70)
    println__T_string(t71)
    var t72 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t73 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t72)
    println__T_string(t73)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t74 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t75 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t74)
    println__T_string(t75)
    var t76 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t77 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t76)
    println__T_string(t77)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv87 *ref_int32_x
    var t88 *ref_int32_x = ref__Ref_5int32(value__204)
    retv87 = t88
    return retv87
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv90 int32
    var t91 int32 = ref_get__Ref_5int32(self__205)
    retv90 = t91
    return retv90
}

func println__T_string(value__1 string) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__43 uint8) string {
    var retv96 string
    var t97 string = _goml_runtime_core_uint8_to_string(self__43)
    retv96 = t97
    return retv96
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__47 float32) string {
    var retv101 string
    var t102 string = _goml_runtime_core_float32_to_string(self__47)
    retv101 = t102
    return retv101
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__42 int64) string {
    var retv104 string
    var t105 string = _goml_runtime_core_int64_to_string(self__42)
    retv104 = t105
    return retv104
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv107 string
    retv107 = self__37
    return retv107
}

func main() {
    main0()
}
