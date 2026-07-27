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

func light_code(light__0 Light) int32 {
    var retv68 int32
    var jp70 int32
    switch light__0 {
    case Light_Red:
        jp70 = 10
    case Yellow:
        jp70 = 20
    case Green:
        jp70 = 30
    default:
        panic("non-exhaustive match")
    }
    retv68 = jp70
    return retv68
}

func paint_code(paint__1 Paint) int32 {
    var retv72 int32
    var jp74 int32
    switch paint__1 {
    case Paint_Red:
        jp74 = 1
    case Blue:
        jp74 = 2
    default:
        panic("non-exhaustive match")
    }
    retv72 = jp74
    return retv72
}

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t76 int32 = light_code(light__2)
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t76)
    println__T_string(t77)
    var t78 int32 = paint_code(paint__3)
    var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t78)
    println__T_string(t79)
    var t80 int32 = light_code(Green)
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t80)
    println__T_string(t81)
    var t82 int32 = paint_code(Blue)
    var t83 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t82)
    println__T_string(t83)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t86 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t86)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv89 string
    var t90 string = _goml_runtime_core_int32_to_string(self__6)
    retv89 = t90
    return retv89
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv92 string
    retv92 = self__38
    return retv92
}

func main() {
    main0()
}
