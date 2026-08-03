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
    var t152 int32
    var inline214 string = "hello"
    switch inline214 {
    case "hello":
        t152 = 1
    case "world":
        t152 = 2
    default:
        t152 = 3
    }
    var t153 string
    var inline212 string = _goml_runtime_core_int32_to_string(t152)
    t153 = inline212
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t153)
    _goml_runtime_core_string_println(inline209)
    var t154 int32
    var inline207 string = "planet"
    switch inline207 {
    case "hello":
        t154 = 1
    case "world":
        t154 = 2
    default:
        t154 = 3
    }
    var t155 string
    var inline205 string = _goml_runtime_core_int32_to_string(t154)
    t155 = inline205
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t155)
    _goml_runtime_core_string_println(inline202)
    var t156 int32
    t156 = 4
    var t157 string
    var inline198 string = _goml_runtime_core_int32_to_string(t156)
    t157 = inline198
    var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t157)
    _goml_runtime_core_string_println(inline195)
    var t158 int32
    t158 = 4
    var t159 string
    var inline191 string = _goml_runtime_core_int32_to_string(t158)
    t159 = inline191
    var inline188 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
    _goml_runtime_core_string_println(inline188)
    var t160 int32
    var inline186 string = "hello"
    switch inline186 {
    case "hello":
        t160 = 6
    default:
        t160 = 8
    }
    var t161 string
    var inline184 string = _goml_runtime_core_int32_to_string(t160)
    t161 = inline184
    var inline181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t161)
    _goml_runtime_core_string_println(inline181)
    var t162 int32
    var inline179 string = "mars"
    switch inline179 {
    case "hello":
        t162 = 6
    default:
        t162 = 8
    }
    var t163 string
    var inline177 string = _goml_runtime_core_int32_to_string(t162)
    t163 = inline177
    var inline174 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline174)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
