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

type Ordering int32

func main0() struct{} {
    var name__0 string = "Alice"
    var inline417 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(name__0)
    _goml_runtime_core_string_println(inline417)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
