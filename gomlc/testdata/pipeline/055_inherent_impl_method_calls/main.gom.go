package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Shape struct {}

func _goml_m_inherent_i_Shape_i_Shape_i_name(self__0 Shape) string {
    return "Shape"
}

func _goml_m_inherent_i_Shape_i_Shape_i_rename(self__1 Shape, suffix__2 string) string {
    var t142 string
    t142 = "Shape"
    var t143 string = t142 + suffix__2
    return t143
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var t146 string
    t146 = "Shape"
    var t147 string = left__4 + t146
    var t148 string = t147 + right__5
    return t148
}

func main0() struct{} {
    var shape__10 Shape = Shape{}
    var inline171 string = _goml_m_inherent_i_Shape_i_Shape_i_name(shape__10)
    var inline172 string = _goml_m_inherent_i_Shape_i_Shape_i_rename(shape__10, "!")
    var inline173 string = _goml_m_inherent_i_Shape_i_Shape_i_join(shape__10, inline171, inline172)
    println__T_string(inline173)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t152 string
    t152 = value__31
    _goml_runtime_core_string_println(t152)
    return struct{}{}
}

func main() {
    main0()
}
