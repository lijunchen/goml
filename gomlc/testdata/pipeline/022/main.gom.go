package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var t171 int32
    var inline233 string = "hello"
    switch inline233 {
    case "hello":
        t171 = 1
    case "world":
        t171 = 2
    default:
        t171 = 3
    }
    var t172 string
    var inline231 string = _goml_runtime_core_int32_to_string(t171)
    t172 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
    _goml_runtime_core_string_println(inline228)
    var t173 int32
    var inline226 string = "planet"
    switch inline226 {
    case "hello":
        t173 = 1
    case "world":
        t173 = 2
    default:
        t173 = 3
    }
    var t174 string
    var inline224 string = _goml_runtime_core_int32_to_string(t173)
    t174 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline221)
    var t175 int32
    t175 = 4
    var t176 string
    var inline217 string = _goml_runtime_core_int32_to_string(t175)
    t176 = inline217
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
    _goml_runtime_core_string_println(inline214)
    var t177 int32
    t177 = 4
    var t178 string
    var inline210 string = _goml_runtime_core_int32_to_string(t177)
    t178 = inline210
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
    _goml_runtime_core_string_println(inline207)
    var t179 int32
    var inline205 string = "hello"
    switch inline205 {
    case "hello":
        t179 = 6
    default:
        t179 = 8
    }
    var t180 string
    var inline203 string = _goml_runtime_core_int32_to_string(t179)
    t180 = inline203
    var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline200)
    var t181 int32
    var inline198 string = "mars"
    switch inline198 {
    case "hello":
        t181 = 6
    default:
        t181 = 8
    }
    var t182 string
    var inline196 string = _goml_runtime_core_int32_to_string(t181)
    t182 = inline196
    var inline193 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline193)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
