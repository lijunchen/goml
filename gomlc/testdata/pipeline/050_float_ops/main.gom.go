package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
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

func _goml_runtime_core_float64_to_string(x float64) string {
    var formatted string = _goml_strconv.FormatFloat(x, 102, -1, 64)
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
    var start32__13 float32 = 1.25
    var end32__14 float32 = 5.75
    var half__15 float32 = 0.5
    var scale__16 float32 = 2
    var mid32__17 float32
    var inline236 float32 = end32__14 - start32__13
    var inline237 float32 = inline236 * half__15
    var inline238 float32 = start32__13 + inline237
    mid32__17 = inline238
    var neg_end32__18 float32 = -end32__14
    var ratio32__19 float32 = end32__14 / scale__16
    var less32__20 bool = start32__13 < end32__14
    var dx__21 float64 = 6.5
    var dy__22 float64 = 3.5
    var quarter__23 float64 = 0.25
    var energy__24 float64
    var inline231 float64 = dx__21 * dx__21
    var inline232 float64 = dy__22 * dy__22
    var inline233 float64 = inline231 + inline232
    var inline234 float64 = inline233 / 2
    energy__24 = inline234
    var neg_dx__25 float64 = -dx__21
    var t160 float64 = energy__24 + dy__22
    var t161 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t160 - t161
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    var inline226 string = "mid32="
    var inline227 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(mid32__17)
    var inline228 string = inline226 + inline227
    println__T_string(inline228)
    var inline221 string = "neg_end32="
    var inline222 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(neg_end32__18)
    var inline223 string = inline221 + inline222
    println__T_string(inline223)
    var inline216 string = "ratio32="
    var inline217 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(ratio32__19)
    var inline218 string = inline216 + inline217
    println__T_string(inline218)
    var t162 string
    var inline214 string = _goml_runtime_core_bool_to_string(less32__20)
    t162 = inline214
    var t163 string = "less32=" + t162
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline211)
    var inline206 string = "energy="
    var inline207 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(energy__24)
    var inline208 string = inline206 + inline207
    println__T_string(inline208)
    var inline201 string = "neg_dx="
    var inline202 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(neg_dx__25)
    var inline203 string = inline201 + inline202
    println__T_string(inline203)
    var inline196 string = "adjusted="
    var inline197 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(adjusted__26)
    var inline198 string = inline196 + inline197
    println__T_string(inline198)
    var t164 string
    var inline194 string = _goml_runtime_core_bool_to_string(less64__28)
    t164 = inline194
    var t165 string = "less64=" + t164
    var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
    _goml_runtime_core_string_println(inline191)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__78 float32) string {
    var t168 string = _goml_runtime_core_float32_to_string(self__78)
    return t168
}

func println__T_string(value__31 string) struct{} {
    var t170 string
    t170 = value__31
    _goml_runtime_core_string_println(t170)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__79 float64) string {
    var t174 string = _goml_runtime_core_float64_to_string(self__79)
    return t174
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
