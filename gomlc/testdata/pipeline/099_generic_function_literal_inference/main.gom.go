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
    var inline183 uint8 = 42
    a__1 = inline183
    var t140 string
    var inline181 string = _goml_runtime_core_uint8_to_string(a__1)
    t140 = inline181
    var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t140)
    _goml_runtime_core_string_println(inline178)
    var b__2 float32
    var inline176 float32 = 3.140000104904175
    b__2 = inline176
    var t141 string
    var inline174 string = _goml_runtime_core_float32_to_string(b__2)
    t141 = inline174
    var inline171 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t141)
    _goml_runtime_core_string_println(inline171)
    var c__3 int64
    var inline169 int64 = 100
    c__3 = inline169
    var t142 string
    var inline167 string = _goml_runtime_core_int64_to_string(c__3)
    t142 = inline167
    var inline164 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t142)
    _goml_runtime_core_string_println(inline164)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
