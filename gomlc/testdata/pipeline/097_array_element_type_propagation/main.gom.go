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
    var inline264 int = 0
    var inline265 *ref_int_x = ref__Ref_3int(inline264)
    i__1 = inline265
    Loop_loop194:
    for {
        var t195 int
        var inline242 int = ref_get__Ref_3int(i__1)
        t195 = inline242
        var t196 bool = t195 < 3
        if t196 {
            var t197 int
            var inline240 int = ref_get__Ref_3int(i__1)
            t197 = inline240
            var t198 uint8 = array_get__Array_3_5uint8(arr__0, t197)
            var t199 string
            var inline238 string = _goml_runtime_core_uint8_to_string(t198)
            t199 = inline238
            var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
            _goml_runtime_core_string_println(inline235)
            var t200 int
            var inline233 int = ref_get__Ref_3int(i__1)
            t200 = inline233
            var t201 int = t200 + 1
            ref_set__Ref_3int(i__1, t201)
            continue
        } else {
            break Loop_loop194
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t186 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t187 string
    var inline262 string = _goml_runtime_core_float32_to_string(t186)
    t187 = inline262
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline259)
    var t188 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t189 string
    var inline257 string = _goml_runtime_core_float32_to_string(t188)
    t189 = inline257
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline254)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t190 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t191 string
    var inline252 string = _goml_runtime_core_int64_to_string(t190)
    t191 = inline252
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline249)
    var t192 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t193 string
    var inline247 string = _goml_runtime_core_int64_to_string(t192)
    t193 = inline247
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline244)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
