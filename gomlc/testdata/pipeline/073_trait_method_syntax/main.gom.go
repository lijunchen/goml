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

type S struct {
    value int32
}

type Ordering int32

func main0() struct{} {
    var t416 string
    var inline431 int32 = 7
    var inline432 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline431)
    var inline433 string = "S(" + inline432
    var inline434 string = inline433 + ")"
    t416 = inline434
    var inline428 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t416)
    _goml_runtime_core_string_println(inline428)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t419 string = _goml_runtime_core_int32_to_string(self__33)
    return t419
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
