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
    var x182 uint8 = 42
    var t186 string
    var inline201 string = _goml_runtime_core_uint8_to_string(x182)
    t186 = inline201
    var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline198)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
