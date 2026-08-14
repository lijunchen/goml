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
    var inline269 int = 0
    var inline270 *ref_int_x = ref__Ref_3int(inline269)
    i__1 = inline270
    Loop_loop199:
    for {
        var t200 int
        var inline247 int = ref_get__Ref_3int(i__1)
        t200 = inline247
        var t201 bool = t200 < 3
        if t201 {
            var t202 int
            var inline245 int = ref_get__Ref_3int(i__1)
            t202 = inline245
            var t203 uint8 = array_get__Array_3_5uint8(arr__0, t202)
            var t204 string
            var inline243 string = _goml_runtime_core_uint8_to_string(t203)
            t204 = inline243
            var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
            _goml_runtime_core_string_println(inline240)
            var t205 int
            var inline238 int = ref_get__Ref_3int(i__1)
            t205 = inline238
            var t206 int = t205 + 1
            ref_set__Ref_3int(i__1, t206)
            continue
        } else {
            break Loop_loop199
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t191 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t192 string
    var inline267 string = _goml_runtime_core_float32_to_string(t191)
    t192 = inline267
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline264)
    var t193 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t194 string
    var inline262 string = _goml_runtime_core_float32_to_string(t193)
    t194 = inline262
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline259)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t195 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t196 string
    var inline257 string = _goml_runtime_core_int64_to_string(t195)
    t196 = inline257
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline254)
    var t197 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t198 string
    var inline252 string = _goml_runtime_core_int64_to_string(t197)
    t198 = inline252
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline249)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
