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
    var inline495 int = 0
    var inline496 *ref_int_x = ref__Ref_3int(inline495)
    i__1 = inline496
    Loop_loop425:
    for {
        var t426 int
        var inline473 int = ref_get__Ref_3int(i__1)
        t426 = inline473
        var t427 bool = t426 < 3
        if t427 {
            var t428 int
            var inline471 int = ref_get__Ref_3int(i__1)
            t428 = inline471
            var t429 uint8 = array_get__Array_3_5uint8(arr__0, t428)
            var t430 string
            var inline469 string = _goml_runtime_core_uint8_to_string(t429)
            t430 = inline469
            var inline466 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
            _goml_runtime_core_string_println(inline466)
            var t431 int
            var inline464 int = ref_get__Ref_3int(i__1)
            t431 = inline464
            var t432 int = t431 + 1
            ref_set__Ref_3int(i__1, t432)
            continue
        } else {
            break Loop_loop425
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t417 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t418 string
    var inline493 string = _goml_runtime_core_float32_to_string(t417)
    t418 = inline493
    var inline490 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
    _goml_runtime_core_string_println(inline490)
    var t419 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t420 string
    var inline488 string = _goml_runtime_core_float32_to_string(t419)
    t420 = inline488
    var inline485 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline485)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t421 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t422 string
    var inline483 string = _goml_runtime_core_int64_to_string(t421)
    t422 = inline483
    var inline480 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline480)
    var t423 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t424 string
    var inline478 string = _goml_runtime_core_int64_to_string(t423)
    t424 = inline478
    var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline475)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
