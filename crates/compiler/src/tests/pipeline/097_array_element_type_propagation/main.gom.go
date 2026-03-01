package main

import (
    "fmt"
)

func int64_to_string(x int64) string {
    return fmt.Sprintf("%d", x)
}

func uint8_to_string(x uint8) string {
    return fmt.Sprintf("%d", x)
}

func float32_to_string(x float32) string {
    return fmt.Sprintf("%g", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func array_get__Array_3_uint8(arr [3]uint8, index int32) uint8 {
    return arr[index]
}

func array_get__Array_2_float32(arr [2]float32, index int32) float32 {
    return arr[index]
}

func array_get__Array_2_int64(arr [2]int64, index int32) int64 {
    return arr[index]
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() struct{} {
    var arr__0 [3]uint8 = [3]uint8{10, 20, 30}
    var i__1 *ref_int32_x = ref__Ref_int32(0)
    for {
        var t17 int32 = ref_get__Ref_int32(i__1)
        var t18 bool = t17 < 3
        if !t18 {
            break
        }
        var t19 int32 = ref_get__Ref_int32(i__1)
        var t20 uint8 = array_get__Array_3_uint8(arr__0, t19)
        var t21 string = uint8_to_string(t20)
        string_println(t21)
        var t22 int32 = ref_get__Ref_int32(i__1)
        var t23 int32 = t22 + 1
        ref_set__Ref_int32(i__1, t23)
        continue
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t8 float32 = array_get__Array_2_float32(floats__2, 0)
    var t9 string = float32_to_string(t8)
    string_println(t9)
    var t10 float32 = array_get__Array_2_float32(floats__2, 1)
    var t11 string = float32_to_string(t10)
    string_println(t11)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t12 int64 = array_get__Array_2_int64(longs__3, 0)
    var t13 string = int64_to_string(t12)
    string_println(t13)
    var t14 int64 = array_get__Array_2_int64(longs__3, 1)
    var t15 string = int64_to_string(t14)
    string_println(t15)
    return struct{}{}
}

func main() {
    main0()
}
