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
    var i__1 *ref_int_x
    var inline242 int = 0
    var inline243 *ref_int_x = ref__Ref_3int(inline242)
    i__1 = inline243
    Loop_loop172:
    for {
        var t173 int
        var inline220 int = ref_get__Ref_3int(i__1)
        t173 = inline220
        var t174 bool = t173 < 3
        if t174 {
            var t175 int
            var inline218 int = ref_get__Ref_3int(i__1)
            t175 = inline218
            var t176 uint8 = array_get__Array_3_5uint8(arr__0, t175)
            var t177 string
            var inline216 string = _goml_runtime_core_uint8_to_string(t176)
            t177 = inline216
            var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
            _goml_runtime_core_string_println(inline213)
            var t178 int
            var inline211 int = ref_get__Ref_3int(i__1)
            t178 = inline211
            var t179 int = t178 + 1
            ref_set__Ref_3int(i__1, t179)
            continue
        } else {
            break Loop_loop172
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t164 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t165 string
    var inline240 string = _goml_runtime_core_float32_to_string(t164)
    t165 = inline240
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
    _goml_runtime_core_string_println(inline237)
    var t166 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t167 string
    var inline235 string = _goml_runtime_core_float32_to_string(t166)
    t167 = inline235
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline232)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t168 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t169 string
    var inline230 string = _goml_runtime_core_int64_to_string(t168)
    t169 = inline230
    var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
    _goml_runtime_core_string_println(inline227)
    var t170 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t171 string
    var inline225 string = _goml_runtime_core_int64_to_string(t170)
    t171 = inline225
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
    _goml_runtime_core_string_println(inline222)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
