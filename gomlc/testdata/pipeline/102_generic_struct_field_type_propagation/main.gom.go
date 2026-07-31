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
    var p__0 Pair__uint8__float32 = Pair__uint8__float32{
        first: 10,
        second: 3.140000104904175,
    }
    var t155 uint8 = p__0.first
    var t156 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t155)
    println__T_string(t156)
    var t157 float32 = p__0.second
    var t158 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t157)
    println__T_string(t158)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t160 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t160)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv163 string
    var t164 string = _goml_runtime_core_uint8_to_string(self__45)
    retv163 = t164
    return retv163
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv166 string
    var t167 string = _goml_runtime_core_float32_to_string(self__49)
    retv166 = t167
    return retv166
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv169 string
    retv169 = self__38
    return retv169
}

func main() {
    main0()
}
