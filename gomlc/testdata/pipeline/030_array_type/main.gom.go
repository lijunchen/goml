package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

type Buffer struct {
    values [3]int32
}

type Ordering int32

func main0() struct{} {
    var inline421 string = "array"
    var inline422 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline421)
    _goml_runtime_core_string_print(inline422)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
