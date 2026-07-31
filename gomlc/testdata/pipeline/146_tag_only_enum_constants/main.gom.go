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
    var retv156 int32
    var jp158 int32
    switch light__0 {
    case Light_Red:
        jp158 = 10
    case Yellow:
        jp158 = 20
    case Green:
        jp158 = 30
    default:
        panic("non-exhaustive match")
    }
    retv156 = jp158
    return retv156
}

func paint_code(paint__1 Paint) int32 {
    var retv160 int32
    var jp162 int32
    switch paint__1 {
    case Paint_Red:
        jp162 = 1
    case Blue:
        jp162 = 2
    default:
        panic("non-exhaustive match")
    }
    retv160 = jp162
    return retv160
}

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t164 int32 = light_code(light__2)
    var t165 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t164)
    println__T_string(t165)
    var t166 int32 = paint_code(paint__3)
    var t167 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t166)
    println__T_string(t167)
    var t168 int32 = light_code(Green)
    var t169 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t168)
    println__T_string(t169)
    var t170 int32 = paint_code(Blue)
    var t171 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t170)
    println__T_string(t171)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t174 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t174)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv177 string
    var t178 string = _goml_runtime_core_int32_to_string(self__6)
    retv177 = t178
    return retv177
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv180 string
    retv180 = self__38
    return retv180
}

func main() {
    main0()
}
