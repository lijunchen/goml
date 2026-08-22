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

type Wrap__isize struct {
    value int
}

type Wrap__string struct {
    value string
}

type Ordering int32

func main0() struct{} {
    var t414 int32
    t414 = 1
    var t415 string
    var inline440 string = _goml_runtime_core_int32_to_string(t414)
    t415 = inline440
    var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t415)
    _goml_runtime_core_string_println(inline437)
    var t416 int32
    t416 = 1
    var t417 string
    var inline434 string = _goml_runtime_core_int32_to_string(t416)
    t417 = inline434
    var inline431 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t417)
    _goml_runtime_core_string_println(inline431)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
