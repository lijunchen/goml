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
    var inline274 int = 0
    var inline275 *ref_int_x = ref__Ref_3int(inline274)
    i__1 = inline275
    Loop_loop204:
    for {
        var t205 int
        var inline252 int = ref_get__Ref_3int(i__1)
        t205 = inline252
        var t206 bool = t205 < 3
        if t206 {
            var t207 int
            var inline250 int = ref_get__Ref_3int(i__1)
            t207 = inline250
            var t208 uint8 = array_get__Array_3_5uint8(arr__0, t207)
            var t209 string
            var inline248 string = _goml_runtime_core_uint8_to_string(t208)
            t209 = inline248
            var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
            _goml_runtime_core_string_println(inline245)
            var t210 int
            var inline243 int = ref_get__Ref_3int(i__1)
            t210 = inline243
            var t211 int = t210 + 1
            ref_set__Ref_3int(i__1, t211)
            continue
        } else {
            break Loop_loop204
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t196 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t197 string
    var inline272 string = _goml_runtime_core_float32_to_string(t196)
    t197 = inline272
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline269)
    var t198 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t199 string
    var inline267 string = _goml_runtime_core_float32_to_string(t198)
    t199 = inline267
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline264)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t200 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t201 string
    var inline262 string = _goml_runtime_core_int64_to_string(t200)
    t201 = inline262
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline259)
    var t202 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t203 string
    var inline257 string = _goml_runtime_core_int64_to_string(t202)
    t203 = inline257
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline254)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
