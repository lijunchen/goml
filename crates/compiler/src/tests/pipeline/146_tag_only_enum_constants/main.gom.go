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
    var retv65 int32
    var jp67 int32
    switch light__0 {
    case Light_Red:
        jp67 = 10
    case Yellow:
        jp67 = 20
    case Green:
        jp67 = 30
    default:
        panic("non-exhaustive match")
    }
    retv65 = jp67
    return retv65
}

func paint_code(paint__1 Paint) int32 {
    var retv69 int32
    var jp71 int32
    switch paint__1 {
    case Paint_Red:
        jp71 = 1
    case Blue:
        jp71 = 2
    default:
        panic("non-exhaustive match")
    }
    retv69 = jp71
    return retv69
}

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t73 int32 = light_code(light__2)
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t73)
    println__T_string(t74)
    var t75 int32 = paint_code(paint__3)
    var t76 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t75)
    println__T_string(t76)
    var t77 int32 = light_code(Green)
    var t78 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t77)
    println__T_string(t78)
    var t79 int32 = paint_code(Blue)
    var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t79)
    println__T_string(t80)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t83 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t83)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv86 string
    var t87 string = _goml_runtime_core_int32_to_string(self__5)
    retv86 = t87
    return retv86
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv89 string
    retv89 = self__37
    return retv89
}

func main() {
    main0()
}
