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

type Ordering int32

func main0() struct{} {
    var arr__0 [3]uint8 = [3]uint8{10, 20, 30}
    var i__1 *ref_int_x
    var inline498 int = 0
    var inline499 *ref_int_x = ref__Ref_3int(inline498)
    i__1 = inline499
    Loop_loop428:
    for {
        var t429 int
        var inline476 int = ref_get__Ref_3int(i__1)
        t429 = inline476
        var t430 bool = t429 < 3
        if t430 {
            var t431 int
            var inline474 int = ref_get__Ref_3int(i__1)
            t431 = inline474
            var t432 uint8 = array_get__Array_3_5uint8(arr__0, t431)
            var t433 string
            var inline472 string = _goml_runtime_core_uint8_to_string(t432)
            t433 = inline472
            var inline469 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
            _goml_runtime_core_string_println(inline469)
            var t434 int
            var inline467 int = ref_get__Ref_3int(i__1)
            t434 = inline467
            var t435 int = t434 + 1
            ref_set__Ref_3int(i__1, t435)
            continue
        } else {
            break Loop_loop428
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t420 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t421 string
    var inline496 string = _goml_runtime_core_float32_to_string(t420)
    t421 = inline496
    var inline493 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline493)
    var t422 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t423 string
    var inline491 string = _goml_runtime_core_float32_to_string(t422)
    t423 = inline491
    var inline488 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline488)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t424 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t425 string
    var inline486 string = _goml_runtime_core_int64_to_string(t424)
    t425 = inline486
    var inline483 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline483)
    var t426 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t427 string
    var inline481 string = _goml_runtime_core_int64_to_string(t426)
    t427 = inline481
    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline478)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
