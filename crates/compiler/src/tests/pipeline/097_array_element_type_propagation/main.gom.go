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
    Loop_loop75:
    for {
        var t76 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
        var t77 bool = t76 < 3
        if t77 {
            var t78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t79 uint8 = array_get__Array_3_5uint8(arr__0, t78)
            var t80 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t79)
            println__T_string(t80)
            var t81 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t82 int32 = t81 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__1, t82)
            continue
        } else {
            break Loop_loop75
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t67 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t68 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t67)
    println__T_string(t68)
    var t69 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t70 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t69)
    println__T_string(t70)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t71 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t72 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t71)
    println__T_string(t72)
    var t73 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t74 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(t73)
    println__T_string(t74)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv84 *ref_int32_x
    var t85 *ref_int32_x = ref__Ref_5int32(value__200)
    retv84 = t85
    return retv84
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv87 int32
    var t88 int32 = ref_get__Ref_5int32(self__201)
    retv87 = t88
    return retv87
}

func println__T_string(value__1 string) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__40 uint8) string {
    var retv93 string
    var t94 string = _goml_runtime_core_uint8_to_string(self__40)
    retv93 = t94
    return retv93
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__44 float32) string {
    var retv98 string
    var t99 string = _goml_runtime_core_float32_to_string(self__44)
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__39 int64) string {
    var retv101 string
    var t102 string = _goml_runtime_core_int64_to_string(self__39)
    retv101 = t102
    return retv101
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv104 string
    retv104 = self__34
    return retv104
}

func main() {
    main0()
}
