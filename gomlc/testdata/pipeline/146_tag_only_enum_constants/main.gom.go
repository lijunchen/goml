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
    var retv159 int32
    var jp161 int32
    switch light__0 {
    case Light_Red:
        jp161 = 10
    case Yellow:
        jp161 = 20
    case Green:
        jp161 = 30
    default:
        panic("non-exhaustive match")
    }
    retv159 = jp161
    return retv159
}

func paint_code(paint__1 Paint) int32 {
    var retv163 int32
    var jp165 int32
    switch paint__1 {
    case Paint_Red:
        jp165 = 1
    case Blue:
        jp165 = 2
    default:
        panic("non-exhaustive match")
    }
    retv163 = jp165
    return retv163
}

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t167 int32 = light_code(light__2)
    var t168 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t167)
    println__T_string(t168)
    var t169 int32 = paint_code(paint__3)
    var t170 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t169)
    println__T_string(t170)
    var t171 int32 = light_code(Green)
    var t172 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t171)
    println__T_string(t172)
    var t173 int32 = paint_code(Blue)
    var t174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t173)
    println__T_string(t174)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t177 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t177)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv180 string
    var t181 string = _goml_runtime_core_int32_to_string(self__6)
    retv180 = t181
    return retv180
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv183 string
    retv183 = self__38
    return retv183
}

func main() {
    main0()
}
