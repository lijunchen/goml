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
    var retv112 int32
    var jp114 int32
    switch light__0 {
    case Light_Red:
        jp114 = 10
    case Yellow:
        jp114 = 20
    case Green:
        jp114 = 30
    default:
        panic("non-exhaustive match")
    }
    retv112 = jp114
    return retv112
}

func paint_code(paint__1 Paint) int32 {
    var retv116 int32
    var jp118 int32
    switch paint__1 {
    case Paint_Red:
        jp118 = 1
    case Blue:
        jp118 = 2
    default:
        panic("non-exhaustive match")
    }
    retv116 = jp118
    return retv116
}

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t120 int32 = light_code(light__2)
    var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t120)
    println__T_string(t121)
    var t122 int32 = paint_code(paint__3)
    var t123 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t122)
    println__T_string(t123)
    var t124 int32 = light_code(Green)
    var t125 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t124)
    println__T_string(t125)
    var t126 int32 = paint_code(Blue)
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t126)
    println__T_string(t127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t130 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t130)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv133 string
    var t134 string = _goml_runtime_core_int32_to_string(self__6)
    retv133 = t134
    return retv133
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv136 string
    retv136 = self__38
    return retv136
}

func main() {
    main0()
}
