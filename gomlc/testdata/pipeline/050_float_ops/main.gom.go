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
    var inline287 float32 = end32__14 - start32__13
    var inline288 float32 = inline287 * half__15
    var inline289 float32 = start32__13 + inline288
    mid32__17 = inline289
    var neg_end32__18 float32 = -end32__14
    var ratio32__19 float32 = end32__14 / scale__16
    var less32__20 bool = start32__13 < end32__14
    var dx__21 float64 = 6.5
    var dy__22 float64 = 3.5
    var quarter__23 float64 = 0.25
    var energy__24 float64
    var inline282 float64 = dx__21 * dx__21
    var inline283 float64 = dy__22 * dy__22
    var inline284 float64 = inline282 + inline283
    var inline285 float64 = inline284 / 2
    energy__24 = inline285
    var neg_dx__25 float64 = -dx__21
    var t211 float64 = energy__24 + dy__22
    var t212 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t211 - t212
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    var inline277 string = "mid32="
    var inline278 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(mid32__17)
    var inline279 string = inline277 + inline278
    println__T_string(inline279)
    var inline272 string = "neg_end32="
    var inline273 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(neg_end32__18)
    var inline274 string = inline272 + inline273
    println__T_string(inline274)
    var inline267 string = "ratio32="
    var inline268 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(ratio32__19)
    var inline269 string = inline267 + inline268
    println__T_string(inline269)
    var t213 string
    var inline265 string = _goml_runtime_core_bool_to_string(less32__20)
    t213 = inline265
    var t214 string = "less32=" + t213
    var inline262 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline262)
    var inline257 string = "energy="
    var inline258 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(energy__24)
    var inline259 string = inline257 + inline258
    println__T_string(inline259)
    var inline252 string = "neg_dx="
    var inline253 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(neg_dx__25)
    var inline254 string = inline252 + inline253
    println__T_string(inline254)
    var inline247 string = "adjusted="
    var inline248 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(adjusted__26)
    var inline249 string = inline247 + inline248
    println__T_string(inline249)
    var t215 string
    var inline245 string = _goml_runtime_core_bool_to_string(less64__28)
    t215 = inline245
    var t216 string = "less64=" + t215
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline242)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__76 float32) string {
    var t219 string = _goml_runtime_core_float32_to_string(self__76)
    return t219
}

func println__T_string(value__1 string) struct{} {
    var t221 string
    t221 = value__1
    _goml_runtime_core_string_println(t221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__77 float64) string {
    var t225 string = _goml_runtime_core_float64_to_string(self__77)
    return t225
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
