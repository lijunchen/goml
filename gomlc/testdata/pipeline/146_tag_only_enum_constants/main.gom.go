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
    var t189 int32
    switch light__2 {
    case Light_Red:
        t189 = 10
    case Yellow:
        t189 = 20
    case Green:
        t189 = 30
    default:
        panic("non-exhaustive match")
    }
    var t190 string
    var inline228 string = _goml_runtime_core_int32_to_string(t189)
    t190 = inline228
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline225)
    var t191 int32
    switch paint__3 {
    case Paint_Red:
        t191 = 1
    case Blue:
        t191 = 2
    default:
        panic("non-exhaustive match")
    }
    var t192 string
    var inline222 string = _goml_runtime_core_int32_to_string(t191)
    t192 = inline222
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline219)
    var t193 int32
    t193 = 30
    var t194 string
    var inline216 string = _goml_runtime_core_int32_to_string(t193)
    t194 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline213)
    var t195 int32
    t195 = 2
    var t196 string
    var inline210 string = _goml_runtime_core_int32_to_string(t195)
    t196 = inline210
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
