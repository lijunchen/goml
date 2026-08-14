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
    var t194 int32
    switch light__2 {
    case Light_Red:
        t194 = 10
    case Yellow:
        t194 = 20
    case Green:
        t194 = 30
    default:
        panic("non-exhaustive match")
    }
    var t195 string
    var inline233 string = _goml_runtime_core_int32_to_string(t194)
    t195 = inline233
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline230)
    var t196 int32
    switch paint__3 {
    case Paint_Red:
        t196 = 1
    case Blue:
        t196 = 2
    default:
        panic("non-exhaustive match")
    }
    var t197 string
    var inline227 string = _goml_runtime_core_int32_to_string(t196)
    t197 = inline227
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline224)
    var t198 int32
    t198 = 30
    var t199 string
    var inline221 string = _goml_runtime_core_int32_to_string(t198)
    t199 = inline221
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline218)
    var t200 int32
    t200 = 2
    var t201 string
    var inline215 string = _goml_runtime_core_int32_to_string(t200)
    t201 = inline215
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline212)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
