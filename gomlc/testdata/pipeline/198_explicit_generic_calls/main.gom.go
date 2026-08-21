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

type Ordering int32

func main0() struct{} {
    var t413 string
    var inline440 string = "direct"
    t413 = inline440
    var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t413)
    _goml_runtime_core_string_println(inline437)
    var t414 int32
    var inline435 int32 = 42
    t414 = inline435
    var t415 string
    var inline433 string = _goml_runtime_core_int32_to_string(t414)
    t415 = inline433
    var inline430 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t415)
    _goml_runtime_core_string_println(inline430)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
