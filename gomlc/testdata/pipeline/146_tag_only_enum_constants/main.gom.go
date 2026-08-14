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
    var t199 int32
    switch light__2 {
    case Light_Red:
        t199 = 10
    case Yellow:
        t199 = 20
    case Green:
        t199 = 30
    default:
        panic("non-exhaustive match")
    }
    var t200 string
    var inline238 string = _goml_runtime_core_int32_to_string(t199)
    t200 = inline238
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline235)
    var t201 int32
    switch paint__3 {
    case Paint_Red:
        t201 = 1
    case Blue:
        t201 = 2
    default:
        panic("non-exhaustive match")
    }
    var t202 string
    var inline232 string = _goml_runtime_core_int32_to_string(t201)
    t202 = inline232
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline229)
    var t203 int32
    t203 = 30
    var t204 string
    var inline226 string = _goml_runtime_core_int32_to_string(t203)
    t204 = inline226
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline223)
    var t205 int32
    t205 = 2
    var t206 string
    var inline220 string = _goml_runtime_core_int32_to_string(t205)
    t206 = inline220
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline217)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
