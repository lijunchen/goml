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

type Wrap__int struct {
    value int
}

type Wrap__string struct {
    value string
}

type Ordering int32

func main0() struct{} {
    var t411 int32
    t411 = 1
    var t412 string
    var inline437 string = _goml_runtime_core_int32_to_string(t411)
    t412 = inline437
    var inline434 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t412)
    _goml_runtime_core_string_println(inline434)
    var t413 int32
    t413 = 1
    var t414 string
    var inline431 string = _goml_runtime_core_int32_to_string(t413)
    t414 = inline431
    var inline428 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t414)
    _goml_runtime_core_string_println(inline428)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
