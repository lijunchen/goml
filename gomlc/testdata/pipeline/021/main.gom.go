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

func match_int(n__0 int32) int32 {
    switch n__0 {
    case 0:
        return 10
    case 1:
        return 20
    default:
        return 30
    }
}

func main0() struct{} {
    var t158 int32 = match_int(0)
    var t159 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t158)
    println__T_string(t159)
    var t160 int32 = match_int(5)
    var t161 string
    var inline226 string = _goml_runtime_core_int32_to_string(t160)
    t161 = inline226
    println__T_string(t161)
    var t162 int32
    t162 = 40
    var t163 string
    var inline222 string = _goml_runtime_core_int32_to_string(t162)
    t163 = inline222
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline219)
    var t164 int32
    t164 = 40
    var t165 string
    var inline215 string = _goml_runtime_core_int32_to_string(t164)
    t165 = inline215
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
    _goml_runtime_core_string_println(inline212)
    var t166 int32
    var inline210 int32 = 2
    switch inline210 {
    case 2:
        t166 = 90
    case 3:
        t166 = 100
    default:
        t166 = 100
    }
    var t167 string
    var inline208 string = _goml_runtime_core_int32_to_string(t166)
    t167 = inline208
    var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline205)
    var t168 int32
    var inline203 int32 = 3
    switch inline203 {
    case 2:
        t168 = 90
    case 3:
        t168 = 100
    default:
        t168 = 100
    }
    var t169 string
    var inline201 string = _goml_runtime_core_int32_to_string(t168)
    t169 = inline201
    var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
    _goml_runtime_core_string_println(inline198)
    var t170 int32
    var inline196 int32 = 1
    switch inline196 {
    case 1:
        t170 = 60
    default:
        t170 = 80
    }
    var t171 string
    var inline194 string = _goml_runtime_core_int32_to_string(t170)
    t171 = inline194
    var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
    _goml_runtime_core_string_println(inline191)
    var t172 int32
    var inline189 int32 = 3
    switch inline189 {
    case 1:
        t172 = 60
    default:
        t172 = 80
    }
    var t173 string
    var inline187 string = _goml_runtime_core_int32_to_string(t172)
    t173 = inline187
    var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t173)
    _goml_runtime_core_string_println(inline184)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t176 string
    t176 = value__31
    _goml_runtime_core_string_println(t176)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t180 string = _goml_runtime_core_int32_to_string(self__35)
    return t180
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
