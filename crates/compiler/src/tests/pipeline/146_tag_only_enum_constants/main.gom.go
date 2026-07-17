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
    var retv62 int32
    var jp64 int32
    switch light__0 {
    case Light_Red:
        jp64 = 10
    case Yellow:
        jp64 = 20
    case Green:
        jp64 = 30
    default:
        panic("non-exhaustive match")
    }
    retv62 = jp64
    return retv62
}

func paint_code(paint__1 Paint) int32 {
    var retv66 int32
    var jp68 int32
    switch paint__1 {
    case Paint_Red:
        jp68 = 1
    case Blue:
        jp68 = 2
    default:
        panic("non-exhaustive match")
    }
    retv66 = jp68
    return retv66
}

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t70 int32 = light_code(light__2)
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t70)
    println__T_string(t71)
    var t72 int32 = paint_code(paint__3)
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t72)
    println__T_string(t73)
    var t74 int32 = light_code(Green)
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t74)
    println__T_string(t75)
    var t76 int32 = paint_code(Blue)
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t76)
    println__T_string(t77)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t80)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv83 string
    var t84 string = _goml_runtime_core_int32_to_string(self__2)
    retv83 = t84
    return retv83
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv86 string
    retv86 = self__34
    return retv86
}

func main() {
    main0()
}
