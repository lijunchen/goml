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
    var first__3 int32 = 1
    var t412 int32
    var inline433 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(first__3)
    t412 = inline433
    var inline430 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t412)
    _goml_runtime_core_string_println(inline430)
    var t413 int32
    t413 = 42
    var inline426 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t413)
    _goml_runtime_core_string_println(inline426)
    return struct{}{}
}

func _goml_m_trait__impl_i_Extra_i_int32_i_extra(self__1 int32) int32 {
    return 42
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t424 string = _goml_runtime_core_int32_to_string(self__154)
    return t424
}

func main() {
    main0()
}
