package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

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

type Pair__uint8__float32 struct {
    first uint8
    second float32
}

func main0() struct{} {
    var t158 uint8 = 10
    var t159 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t158)
    println__T_string(t159)
    var t160 float32 = 3.140000104904175
    var t161 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t160)
    println__T_string(t161)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t163)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var t167 string = _goml_runtime_core_uint8_to_string(self__45)
    return t167
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var t170 string = _goml_runtime_core_float32_to_string(self__49)
    return t170
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
