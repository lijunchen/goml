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
    var inline259 int = 0
    var inline260 *ref_int_x = ref__Ref_3int(inline259)
    i__1 = inline260
    Loop_loop189:
    for {
        var t190 int
        var inline237 int = ref_get__Ref_3int(i__1)
        t190 = inline237
        var t191 bool = t190 < 3
        if t191 {
            var t192 int
            var inline235 int = ref_get__Ref_3int(i__1)
            t192 = inline235
            var t193 uint8 = array_get__Array_3_5uint8(arr__0, t192)
            var t194 string
            var inline233 string = _goml_runtime_core_uint8_to_string(t193)
            t194 = inline233
            var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
            _goml_runtime_core_string_println(inline230)
            var t195 int
            var inline228 int = ref_get__Ref_3int(i__1)
            t195 = inline228
            var t196 int = t195 + 1
            ref_set__Ref_3int(i__1, t196)
            continue
        } else {
            break Loop_loop189
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t181 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t182 string
    var inline257 string = _goml_runtime_core_float32_to_string(t181)
    t182 = inline257
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline254)
    var t183 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t184 string
    var inline252 string = _goml_runtime_core_float32_to_string(t183)
    t184 = inline252
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline249)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t185 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t186 string
    var inline247 string = _goml_runtime_core_int64_to_string(t185)
    t186 = inline247
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline244)
    var t187 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t188 string
    var inline242 string = _goml_runtime_core_int64_to_string(t187)
    t188 = inline242
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline239)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
