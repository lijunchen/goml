package main

import (
    _goml_fmt "fmt"
)

func int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func float32_to_string(x float32) string {
    return _goml_fmt.Sprintf("%g", x)
}

func string_println(s string) struct{} {
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

type GoError = error

func main0() struct{} {
    var arr__0 [3]uint8 = [3]uint8{10, 20, 30}
    var i__1 *ref_int32_x = ref__Ref_5int32(0)
    Loop_loop17:
    for {
        var t18 int32 = ref_get__Ref_5int32(i__1)
        var t19 bool = t18 < 3
        if t19 {
            var t20 int32 = ref_get__Ref_5int32(i__1)
            var t21 uint8 = array_get__Array_3_5uint8(arr__0, t20)
            var t22 string = uint8_to_string(t21)
            string_println(t22)
            var t23 int32 = ref_get__Ref_5int32(i__1)
            var t24 int32 = t23 + 1
            ref_set__Ref_5int32(i__1, t24)
            continue
        } else {
            break Loop_loop17
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t9 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t10 string = float32_to_string(t9)
    string_println(t10)
    var t11 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t12 string = float32_to_string(t11)
    string_println(t12)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t13 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t14 string = int64_to_string(t13)
    string_println(t14)
    var t15 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t16 string = int64_to_string(t15)
    string_println(t16)
    return struct{}{}
}

func main() {
    main0()
}
