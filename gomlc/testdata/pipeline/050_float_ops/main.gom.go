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
    var inline277 float32 = end32__14 - start32__13
    var inline278 float32 = inline277 * half__15
    var inline279 float32 = start32__13 + inline278
    mid32__17 = inline279
    var neg_end32__18 float32 = -end32__14
    var ratio32__19 float32 = end32__14 / scale__16
    var less32__20 bool = start32__13 < end32__14
    var dx__21 float64 = 6.5
    var dy__22 float64 = 3.5
    var quarter__23 float64 = 0.25
    var energy__24 float64
    var inline272 float64 = dx__21 * dx__21
    var inline273 float64 = dy__22 * dy__22
    var inline274 float64 = inline272 + inline273
    var inline275 float64 = inline274 / 2
    energy__24 = inline275
    var neg_dx__25 float64 = -dx__21
    var t201 float64 = energy__24 + dy__22
    var t202 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t201 - t202
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    var inline267 string = "mid32="
    var inline268 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(mid32__17)
    var inline269 string = inline267 + inline268
    println__T_string(inline269)
    var inline262 string = "neg_end32="
    var inline263 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(neg_end32__18)
    var inline264 string = inline262 + inline263
    println__T_string(inline264)
    var inline257 string = "ratio32="
    var inline258 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(ratio32__19)
    var inline259 string = inline257 + inline258
    println__T_string(inline259)
    var t203 string
    var inline255 string = _goml_runtime_core_bool_to_string(less32__20)
    t203 = inline255
    var t204 string = "less32=" + t203
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline252)
    var inline247 string = "energy="
    var inline248 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(energy__24)
    var inline249 string = inline247 + inline248
    println__T_string(inline249)
    var inline242 string = "neg_dx="
    var inline243 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(neg_dx__25)
    var inline244 string = inline242 + inline243
    println__T_string(inline244)
    var inline237 string = "adjusted="
    var inline238 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(adjusted__26)
    var inline239 string = inline237 + inline238
    println__T_string(inline239)
    var t205 string
    var inline235 string = _goml_runtime_core_bool_to_string(less64__28)
    t205 = inline235
    var t206 string = "less64=" + t205
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline232)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__78 float32) string {
    var t209 string = _goml_runtime_core_float32_to_string(self__78)
    return t209
}

func println__T_string(value__31 string) struct{} {
    var t211 string
    t211 = value__31
    _goml_runtime_core_string_println(t211)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__79 float64) string {
    var t215 string = _goml_runtime_core_float64_to_string(self__79)
    return t215
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
