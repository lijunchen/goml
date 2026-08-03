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

type Light int32

const (
    Light_Red Light = 0
    Yellow Light = 1
    Green Light = 2
)

type Paint int32

const (
    Paint_Red Paint = 0
    Blue Paint = 1
)

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t148 int32
    switch light__2 {
    case Light_Red:
        t148 = 10
    case Yellow:
        t148 = 20
    case Green:
        t148 = 30
    default:
        panic("non-exhaustive match")
    }
    var t149 string
    var inline187 string = _goml_runtime_core_int32_to_string(t148)
    t149 = inline187
    var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t149)
    _goml_runtime_core_string_println(inline184)
    var t150 int32
    switch paint__3 {
    case Paint_Red:
        t150 = 1
    case Blue:
        t150 = 2
    default:
        panic("non-exhaustive match")
    }
    var t151 string
    var inline181 string = _goml_runtime_core_int32_to_string(t150)
    t151 = inline181
    var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t151)
    _goml_runtime_core_string_println(inline178)
    var t152 int32
    t152 = 30
    var t153 string
    var inline175 string = _goml_runtime_core_int32_to_string(t152)
    t153 = inline175
    var inline172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t153)
    _goml_runtime_core_string_println(inline172)
    var t154 int32
    t154 = 2
    var t155 string
    var inline169 string = _goml_runtime_core_int32_to_string(t154)
    t155 = inline169
    var inline166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t155)
    _goml_runtime_core_string_println(inline166)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
