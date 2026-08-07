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
    var t184 int32
    switch light__2 {
    case Light_Red:
        t184 = 10
    case Yellow:
        t184 = 20
    case Green:
        t184 = 30
    default:
        panic("non-exhaustive match")
    }
    var t185 string
    var inline223 string = _goml_runtime_core_int32_to_string(t184)
    t185 = inline223
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline220)
    var t186 int32
    switch paint__3 {
    case Paint_Red:
        t186 = 1
    case Blue:
        t186 = 2
    default:
        panic("non-exhaustive match")
    }
    var t187 string
    var inline217 string = _goml_runtime_core_int32_to_string(t186)
    t187 = inline217
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline214)
    var t188 int32
    t188 = 30
    var t189 string
    var inline211 string = _goml_runtime_core_int32_to_string(t188)
    t189 = inline211
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline208)
    var t190 int32
    t190 = 2
    var t191 string
    var inline205 string = _goml_runtime_core_int32_to_string(t190)
    t191 = inline205
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
