package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func main0() struct{} {
    var inline185 string = "array literal"
    var inline186 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline185)
    _goml_runtime_core_string_print(inline186)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
