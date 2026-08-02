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

func main0() struct{} {
    var a__1 uint8
    var inline202 uint8 = 42
    a__1 = inline202
    var t159 string
    var inline200 string = _goml_runtime_core_uint8_to_string(a__1)
    t159 = inline200
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
    _goml_runtime_core_string_println(inline197)
    var b__2 float32
    var inline195 float32 = 3.140000104904175
    b__2 = inline195
    var t160 string
    var inline193 string = _goml_runtime_core_float32_to_string(b__2)
    t160 = inline193
    var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline190)
    var c__3 int64
    var inline188 int64 = 100
    c__3 = inline188
    var t161 string
    var inline186 string = _goml_runtime_core_int64_to_string(c__3)
    t161 = inline186
    var inline183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t161)
    _goml_runtime_core_string_println(inline183)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
