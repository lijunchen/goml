package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint32 struct {
    items []uint32
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type Shape struct {}

type Ordering int32

func _goml_m_inherent_i_Shape_i_Shape_i_name(self__0 Shape) string {
    return "Shape"
}

func _goml_m_inherent_i_Shape_i_Shape_i_rename(self__1 Shape, suffix__2 string) string {
    var t802 string
    t802 = "Shape"
    var t803 string = t802 + suffix__2
    return t803
}

func _goml_m_inherent_i_Shape_i_Shape_i_join(self__3 Shape, left__4 string, right__5 string) string {
    var t806 string
    t806 = "Shape"
    var t807 string = left__4 + t806
    var t808 string = t807 + right__5
    return t808
}

func main0() struct{} {
    var shape__10 Shape = Shape{}
    var inline831 string = _goml_m_inherent_i_Shape_i_Shape_i_name(shape__10)
    var inline832 string = _goml_m_inherent_i_Shape_i_Shape_i_rename(shape__10, "!")
    var inline833 string = _goml_m_inherent_i_Shape_i_Shape_i_join(shape__10, inline831, inline832)
    println__T_string(inline833)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t812 string
    t812 = value__1
    _goml_runtime_core_string_println(t812)
    return struct{}{}
}

func main() {
    main0()
}
