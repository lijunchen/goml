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
    var inline272 float32 = end32__14 - start32__13
    var inline273 float32 = inline272 * half__15
    var inline274 float32 = start32__13 + inline273
    mid32__17 = inline274
    var neg_end32__18 float32 = -end32__14
    var ratio32__19 float32 = end32__14 / scale__16
    var less32__20 bool = start32__13 < end32__14
    var dx__21 float64 = 6.5
    var dy__22 float64 = 3.5
    var quarter__23 float64 = 0.25
    var energy__24 float64
    var inline267 float64 = dx__21 * dx__21
    var inline268 float64 = dy__22 * dy__22
    var inline269 float64 = inline267 + inline268
    var inline270 float64 = inline269 / 2
    energy__24 = inline270
    var neg_dx__25 float64 = -dx__21
    var t196 float64 = energy__24 + dy__22
    var t197 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t196 - t197
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    var inline262 string = "mid32="
    var inline263 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(mid32__17)
    var inline264 string = inline262 + inline263
    println__T_string(inline264)
    var inline257 string = "neg_end32="
    var inline258 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(neg_end32__18)
    var inline259 string = inline257 + inline258
    println__T_string(inline259)
    var inline252 string = "ratio32="
    var inline253 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(ratio32__19)
    var inline254 string = inline252 + inline253
    println__T_string(inline254)
    var t198 string
    var inline250 string = _goml_runtime_core_bool_to_string(less32__20)
    t198 = inline250
    var t199 string = "less32=" + t198
    var inline247 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline247)
    var inline242 string = "energy="
    var inline243 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(energy__24)
    var inline244 string = inline242 + inline243
    println__T_string(inline244)
    var inline237 string = "neg_dx="
    var inline238 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(neg_dx__25)
    var inline239 string = inline237 + inline238
    println__T_string(inline239)
    var inline232 string = "adjusted="
    var inline233 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(adjusted__26)
    var inline234 string = inline232 + inline233
    println__T_string(inline234)
    var t200 string
    var inline230 string = _goml_runtime_core_bool_to_string(less64__28)
    t200 = inline230
    var t201 string = "less64=" + t200
    var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline227)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__76 float32) string {
    var t204 string = _goml_runtime_core_float32_to_string(self__76)
    return t204
}

func println__T_string(value__1 string) struct{} {
    var t206 string
    t206 = value__1
    _goml_runtime_core_string_println(t206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__77 float64) string {
    var t210 string = _goml_runtime_core_float64_to_string(self__77)
    return t210
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
