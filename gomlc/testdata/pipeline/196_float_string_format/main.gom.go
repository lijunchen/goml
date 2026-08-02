package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

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
    var t162 string
    var inline192 float64 = 18318654708.7
    var inline193 string = _goml_runtime_core_float64_to_string(inline192)
    t162 = inline193
    _goml_runtime_core_string_println(t162)
    var t163 string
    var inline189 float64 = 0.0000001
    var inline190 string = _goml_runtime_core_float64_to_string(inline189)
    t163 = inline190
    _goml_runtime_core_string_println(t163)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t164 float64 = negative_one__1 * zero__0
    var t165 string
    var inline187 string = _goml_runtime_core_float64_to_string(t164)
    t165 = inline187
    _goml_runtime_core_string_println(t165)
    var t166 float64 = 1 / zero__0
    var t167 string
    var inline185 string = _goml_runtime_core_float64_to_string(t166)
    t167 = inline185
    _goml_runtime_core_string_println(t167)
    var t168 float64 = -1
    var t169 float64 = t168 / zero__0
    var t170 string
    var inline183 string = _goml_runtime_core_float64_to_string(t169)
    t170 = inline183
    _goml_runtime_core_string_println(t170)
    var t171 float64 = zero__0 / zero__0
    var t172 string
    var inline181 string = _goml_runtime_core_float64_to_string(t171)
    t172 = inline181
    _goml_runtime_core_string_println(t172)
    var wide__2 float64 = 12345678
    var t173 string
    var inline179 string = _goml_runtime_core_float64_to_string(wide__2)
    t173 = inline179
    _goml_runtime_core_string_println(t173)
    return struct{}{}
}

func main() {
    main0()
}
