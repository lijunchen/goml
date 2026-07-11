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
    var retv8 int32
    var jp10 int32
    switch light__0 {
    case Light_Red:
        jp10 = 10
    case Yellow:
        jp10 = 20
    case Green:
        jp10 = 30
    default:
        panic("non-exhaustive match")
    }
    retv8 = jp10
    return retv8
}

func paint_code(paint__1 Paint) int32 {
    var retv12 int32
    var jp14 int32
    switch paint__1 {
    case Paint_Red:
        jp14 = 1
    case Blue:
        jp14 = 2
    default:
        panic("non-exhaustive match")
    }
    retv12 = jp14
    return retv12
}

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t16 int32 = light_code(light__2)
    var t17 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t16)
    println__T_string(t17)
    var t18 int32 = paint_code(paint__3)
    var t19 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t18)
    println__T_string(t19)
    var t20 int32 = light_code(Green)
    var t21 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t20)
    println__T_string(t21)
    var t22 int32 = paint_code(Blue)
    var t23 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t22)
    println__T_string(t23)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t26 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t26)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv29 string
    var t30 string = _goml_runtime_core_int32_to_string(self__2)
    retv29 = t30
    return retv29
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv32 string
    retv32 = self__9
    return retv32
}

func main() {
    main0()
}
