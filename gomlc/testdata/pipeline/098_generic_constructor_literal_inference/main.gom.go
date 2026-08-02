package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Option__uint8 interface {
    isOption__uint8()
}

type Some struct {
    _0 uint8
}

func (_ Some) isOption__uint8() {}

type None struct {}

func (_ None) isOption__uint8() {}

func main0() struct{} {
    var x155 uint8 = 42
    var t159 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(x155)
    println__T_string(t159)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t163)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var t167 string = _goml_runtime_core_uint8_to_string(self__45)
    return t167
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
