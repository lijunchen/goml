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

type Box__string struct {
    value string
}

type Box__int32 struct {
    value int32
}

type Ordering int32

func _goml_m_trait__impl_i_Mark_i_int32_i_mark(self__0 int32) string {
    var inline435 string = _goml_runtime_core_int32_to_string(self__0)
    return inline435
}

func main0() struct{} {
    var t419 string
    var inline447 string = "text"
    var inline448 string = "string:" + inline447
    t419 = inline448
    var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline444)
    var t420 string
    var inline440 int32 = 7
    var inline441 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline440)
    var inline442 string = "blanket:" + inline441
    t420 = inline442
    var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline437)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
