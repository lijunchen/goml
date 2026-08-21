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
    var t415 int32
    var inline436 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(first__3)
    t415 = inline436
    var inline433 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t415)
    _goml_runtime_core_string_println(inline433)
    var t416 int32
    t416 = 42
    var inline429 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t416)
    _goml_runtime_core_string_println(inline429)
    return struct{}{}
}

func _goml_m_trait__impl_i_Extra_i_int32_i_extra(self__1 int32) int32 {
    return 42
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t427 string = _goml_runtime_core_int32_to_string(self__154)
    return t427
}

func main() {
    main0()
}
