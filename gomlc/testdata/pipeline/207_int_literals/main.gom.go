package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint16_to_string(x uint16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint32_to_string(x uint32) string {
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

func _goml_runtime_core_float64_to_string(x float64) string {
    var formatted string = _goml_strconv.FormatFloat(x, 102, -1, 64)
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

func array_get__Array_2_5uint8(arr [2]uint8, index int) uint8 {
    return arr[index]
}

func array_get__Array_2_6uint16(arr [2]uint16, index int) uint16 {
    return arr[index]
}

type ref_uint32_x struct {
    value uint32
}

func ref__Ref_6uint32(value uint32) *ref_uint32_x {
    return &ref_uint32_x{
        value: value,
    }
}

func ref_get__Ref_6uint32(reference *ref_uint32_x) uint32 {
    return reference.value
}

func increment(value__0 uint8) uint8 {
    var retv80 uint8
    var t81 uint8 = value__0 + 1
    retv80 = t81
    return retv80
}

func one_float32() float32 {
    var retv83 float32
    retv83 = 1
    return retv83
}

func two_int16() int16 {
    var retv85 int16
    retv85 = 2
    return retv85
}

func read_uint32(value__1 *ref_uint32_x) uint32 {
    var retv87 uint32
    var t88 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(value__1)
    retv87 = t88
    return retv87
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp91 uint8
    if true {
        jp91 = 1
    } else {
        jp91 = small__4
    }
    var branch__6 uint8 = jp91
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t92 int = default_integer__2 + 2
    var t93 string = _goml_m_inherent_i_int_i_int_i_to__string(t92)
    println__T_string(t93)
    var t94 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(default_float__3)
    println__T_string(t94)
    var t95 uint8 = increment(small__4)
    var t96 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t95)
    println__T_string(t96)
    var t97 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t98 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t97)
    println__T_string(t98)
    var t99 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(branch__6)
    println__T_string(t99)
    var t100 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t101 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(t100)
    println__T_string(t101)
    var t102 float32 = one_float32()
    var t103 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t102)
    println__T_string(t103)
    var t104 int16 = two_int16()
    var t105 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(t104)
    println__T_string(t105)
    var t106 uint32 = read_uint32(delayed__8)
    var t107 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t106)
    println__T_string(t107)
    var t108 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__9, 65)
    var t109 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t108)
    println__T_string(t109)
    var jp111 string
    switch byte__9 {
    case 65:
        jp111 = "byte"
    default:
        jp111 = "other"
    }
    println__T_string(jp111)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__208 *ref_uint32_x) uint32 {
    var retv113 uint32
    var t114 uint32 = ref_get__Ref_6uint32(self__208)
    retv113 = t114
    return retv113
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__207 uint32) *ref_uint32_x {
    var retv116 *ref_uint32_x
    var t117 *ref_uint32_x = ref__Ref_6uint32(value__207)
    retv116 = t117
    return retv116
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv119 uint8
    var t120 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv119 = t120
    return retv119
}

func println__T_string(value__1 string) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t122)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv125 string
    var t126 string = _goml_runtime_core_int_to_string(self__5)
    retv125 = t126
    return retv125
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv128 string
    var t129 string = _goml_runtime_core_float64_to_string(self__50)
    retv128 = t129
    return retv128
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv131 string
    var t132 string = _goml_runtime_core_uint8_to_string(self__45)
    retv131 = t132
    return retv131
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv134 string
    var t135 string = _goml_runtime_core_uint16_to_string(self__46)
    retv134 = t135
    return retv134
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv137 string
    var t138 string = _goml_runtime_core_float32_to_string(self__49)
    retv137 = t138
    return retv137
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv140 string
    var t141 string = _goml_runtime_core_int16_to_string(self__42)
    retv140 = t141
    return retv140
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv143 string
    var t144 string = _goml_runtime_core_uint32_to_string(self__47)
    retv143 = t144
    return retv143
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv146 bool
    var t147 bool = self__69 == other__70
    retv146 = t147
    return retv146
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv149 string
    var t150 string = _goml_runtime_core_bool_to_string(self__37)
    retv149 = t150
    return retv149
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv152 string
    retv152 = self__38
    return retv152
}

func main() {
    main0()
}
