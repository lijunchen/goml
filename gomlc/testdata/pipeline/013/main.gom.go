package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var s__0 string = "abcde"
    var inline423 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s__0)
    _goml_runtime_core_string_println(inline423)
    var inline420 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s__0)
    _goml_runtime_core_string_print(inline420)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
