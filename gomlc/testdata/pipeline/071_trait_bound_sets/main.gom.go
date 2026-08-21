package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type S struct {}

type Ordering int32

func _goml_m_trait__impl_i_A_i_S_i_foo(self__0 S) string {
    return "A"
}

func _goml_m_trait__impl_i_C_i_S_i_bar(self__2 S) string {
    return "C"
}

func main0() struct{} {
    var s__5 S = S{}
    var t420 string
    var inline442 string = _goml_m_trait__impl_i_A_i_S_i_foo(s__5)
    t420 = inline442
    var inline439 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline439)
    var t421 string
    var inline437 string = _goml_m_trait__impl_i_C_i_S_i_bar(s__5)
    t421 = inline437
    var inline434 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline434)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
