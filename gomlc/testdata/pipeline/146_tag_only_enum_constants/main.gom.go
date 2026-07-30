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
    var retv72 int32
    var jp74 int32
    switch light__0 {
    case Light_Red:
        jp74 = 10
    case Yellow:
        jp74 = 20
    case Green:
        jp74 = 30
    default:
        panic("non-exhaustive match")
    }
    retv72 = jp74
    return retv72
}

func paint_code(paint__1 Paint) int32 {
    var retv76 int32
    var jp78 int32
    switch paint__1 {
    case Paint_Red:
        jp78 = 1
    case Blue:
        jp78 = 2
    default:
        panic("non-exhaustive match")
    }
    retv76 = jp78
    return retv76
}

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t80 int32 = light_code(light__2)
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t80)
    println__T_string(t81)
    var t82 int32 = paint_code(paint__3)
    var t83 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t82)
    println__T_string(t83)
    var t84 int32 = light_code(Green)
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t84)
    println__T_string(t85)
    var t86 int32 = paint_code(Blue)
    var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t86)
    println__T_string(t87)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv93 string
    var t94 string = _goml_runtime_core_int32_to_string(self__6)
    retv93 = t94
    return retv93
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv96 string
    retv96 = self__38
    return retv96
}

func main() {
    main0()
}
