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
    var t143 string
    var inline199 float64 = 18318654708.7
    var inline200 string = _goml_runtime_core_float64_to_string(inline199)
    t143 = inline200
    var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t143)
    _goml_runtime_core_string_println(inline196)
    var t144 string
    var inline193 float64 = 0.0000001
    var inline194 string = _goml_runtime_core_float64_to_string(inline193)
    t144 = inline194
    var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t144)
    _goml_runtime_core_string_println(inline190)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t145 float64 = negative_one__1 * zero__0
    var t146 string
    var inline188 string = _goml_runtime_core_float64_to_string(t145)
    t146 = inline188
    var inline185 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
    _goml_runtime_core_string_println(inline185)
    var t147 float64 = 1 / zero__0
    var t148 string
    var inline183 string = _goml_runtime_core_float64_to_string(t147)
    t148 = inline183
    var inline180 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
    _goml_runtime_core_string_println(inline180)
    var t149 float64 = -1
    var t150 float64 = t149 / zero__0
    var t151 string
    var inline178 string = _goml_runtime_core_float64_to_string(t150)
    t151 = inline178
    var inline175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t151)
    _goml_runtime_core_string_println(inline175)
    var t152 float64 = zero__0 / zero__0
    var t153 string
    var inline173 string = _goml_runtime_core_float64_to_string(t152)
    t153 = inline173
    var inline170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t153)
    _goml_runtime_core_string_println(inline170)
    var wide__2 float64 = 12345678
    var t154 string
    var inline168 string = _goml_runtime_core_float64_to_string(wide__2)
    t154 = inline168
    var inline165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t154)
    _goml_runtime_core_string_println(inline165)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
