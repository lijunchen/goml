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
    var retv26 int32
    var jp28 int32
    switch light__0 {
    case Light_Red:
        jp28 = 10
    case Yellow:
        jp28 = 20
    case Green:
        jp28 = 30
    default:
        panic("non-exhaustive match")
    }
    retv26 = jp28
    return retv26
}

func paint_code(paint__1 Paint) int32 {
    var retv30 int32
    var jp32 int32
    switch paint__1 {
    case Paint_Red:
        jp32 = 1
    case Blue:
        jp32 = 2
    default:
        panic("non-exhaustive match")
    }
    retv30 = jp32
    return retv30
}

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t34 int32 = light_code(light__2)
    var t35 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t34)
    println__T_string(t35)
    var t36 int32 = paint_code(paint__3)
    var t37 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t36)
    println__T_string(t37)
    var t38 int32 = light_code(Green)
    var t39 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t38)
    println__T_string(t39)
    var t40 int32 = paint_code(Blue)
    var t41 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t40)
    println__T_string(t41)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t44 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t44)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv47 string
    var t48 string = _goml_runtime_core_int32_to_string(self__2)
    retv47 = t48
    return retv47
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv50 string
    retv50 = self__9
    return retv50
}

func main() {
    main0()
}
