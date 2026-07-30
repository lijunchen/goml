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
    var t111 uint8 = p__0.first
    var t112 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t111)
    println__T_string(t112)
    var t113 float32 = p__0.second
    var t114 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(t113)
    println__T_string(t114)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t116 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t116)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv119 string
    var t120 string = _goml_runtime_core_uint8_to_string(self__45)
    retv119 = t120
    return retv119
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv122 string
    var t123 string = _goml_runtime_core_float32_to_string(self__49)
    retv122 = t123
    return retv122
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv125 string
    retv125 = self__38
    return retv125
}

func main() {
    main0()
}
