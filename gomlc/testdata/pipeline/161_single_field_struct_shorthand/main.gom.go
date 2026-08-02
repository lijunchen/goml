package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Person struct {
    name string
}

func main0() struct{} {
    var name__0 string = "Alice"
    var inline164 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(name__0)
    _goml_runtime_core_string_println(inline164)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
