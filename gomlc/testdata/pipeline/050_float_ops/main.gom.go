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
    var inline255 float32 = end32__14 - start32__13
    var inline256 float32 = inline255 * half__15
    var inline257 float32 = start32__13 + inline256
    mid32__17 = inline257
    var neg_end32__18 float32 = -end32__14
    var ratio32__19 float32 = end32__14 / scale__16
    var less32__20 bool = start32__13 < end32__14
    var dx__21 float64 = 6.5
    var dy__22 float64 = 3.5
    var quarter__23 float64 = 0.25
    var energy__24 float64
    var inline250 float64 = dx__21 * dx__21
    var inline251 float64 = dy__22 * dy__22
    var inline252 float64 = inline250 + inline251
    var inline253 float64 = inline252 / 2
    energy__24 = inline253
    var neg_dx__25 float64 = -dx__21
    var t179 float64 = energy__24 + dy__22
    var t180 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t179 - t180
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    var inline245 string = "mid32="
    var inline246 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(mid32__17)
    var inline247 string = inline245 + inline246
    println__T_string(inline247)
    var inline240 string = "neg_end32="
    var inline241 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(neg_end32__18)
    var inline242 string = inline240 + inline241
    println__T_string(inline242)
    var inline235 string = "ratio32="
    var inline236 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(ratio32__19)
    var inline237 string = inline235 + inline236
    println__T_string(inline237)
    var t181 string
    var inline233 string = _goml_runtime_core_bool_to_string(less32__20)
    t181 = inline233
    var t182 string = "less32=" + t181
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline230)
    var inline225 string = "energy="
    var inline226 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(energy__24)
    var inline227 string = inline225 + inline226
    println__T_string(inline227)
    var inline220 string = "neg_dx="
    var inline221 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(neg_dx__25)
    var inline222 string = inline220 + inline221
    println__T_string(inline222)
    var inline215 string = "adjusted="
    var inline216 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(adjusted__26)
    var inline217 string = inline215 + inline216
    println__T_string(inline217)
    var t183 string
    var inline213 string = _goml_runtime_core_bool_to_string(less64__28)
    t183 = inline213
    var t184 string = "less64=" + t183
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var t187 string = _goml_runtime_core_float32_to_string(self__49)
    return t187
}

func println__T_string(value__1 string) struct{} {
    var t189 string
    t189 = value__1
    _goml_runtime_core_string_println(t189)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var t193 string = _goml_runtime_core_float64_to_string(self__50)
    return t193
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
