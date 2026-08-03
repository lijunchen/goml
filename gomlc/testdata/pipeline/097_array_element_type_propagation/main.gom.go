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
    var inline223 int = 0
    var inline224 *ref_int_x = ref__Ref_3int(inline223)
    i__1 = inline224
    Loop_loop153:
    for {
        var t154 int
        var inline201 int = ref_get__Ref_3int(i__1)
        t154 = inline201
        var t155 bool = t154 < 3
        if t155 {
            var t156 int
            var inline199 int = ref_get__Ref_3int(i__1)
            t156 = inline199
            var t157 uint8 = array_get__Array_3_5uint8(arr__0, t156)
            var t158 string
            var inline197 string = _goml_runtime_core_uint8_to_string(t157)
            t158 = inline197
            var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t158)
            _goml_runtime_core_string_println(inline194)
            var t159 int
            var inline192 int = ref_get__Ref_3int(i__1)
            t159 = inline192
            var t160 int = t159 + 1
            ref_set__Ref_3int(i__1, t160)
            continue
        } else {
            break Loop_loop153
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t145 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t146 string
    var inline221 string = _goml_runtime_core_float32_to_string(t145)
    t146 = inline221
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
    _goml_runtime_core_string_println(inline218)
    var t147 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t148 string
    var inline216 string = _goml_runtime_core_float32_to_string(t147)
    t148 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
    _goml_runtime_core_string_println(inline213)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t149 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t150 string
    var inline211 string = _goml_runtime_core_int64_to_string(t149)
    t150 = inline211
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t150)
    _goml_runtime_core_string_println(inline208)
    var t151 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t152 string
    var inline206 string = _goml_runtime_core_int64_to_string(t151)
    t152 = inline206
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t152)
    _goml_runtime_core_string_println(inline203)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
