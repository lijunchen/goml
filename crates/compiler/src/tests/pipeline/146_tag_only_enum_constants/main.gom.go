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
    var retv11 int32
    var jp13 int32
    switch light__0 {
    case Light_Red:
        jp13 = 10
    case Yellow:
        jp13 = 20
    case Green:
        jp13 = 30
    default:
        panic("non-exhaustive match")
    }
    retv11 = jp13
    return retv11
}

func paint_code(paint__1 Paint) int32 {
    var retv15 int32
    var jp17 int32
    switch paint__1 {
    case Paint_Red:
        jp17 = 1
    case Blue:
        jp17 = 2
    default:
        panic("non-exhaustive match")
    }
    retv15 = jp17
    return retv15
}

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t19 int32 = light_code(light__2)
    var t20 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t19)
    println__T_string(t20)
    var t21 int32 = paint_code(paint__3)
    var t22 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t21)
    println__T_string(t22)
    var t23 int32 = light_code(Green)
    var t24 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t23)
    println__T_string(t24)
    var t25 int32 = paint_code(Blue)
    var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t25)
    println__T_string(t26)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t29 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t29)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv32 string
    var t33 string = _goml_runtime_core_int32_to_string(self__2)
    retv32 = t33
    return retv32
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv35 string
    retv35 = self__9
    return retv35
}

func main() {
    main0()
}
