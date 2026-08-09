package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func main0() struct{} {
    var x177 bool = false
    var inline188 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x177)
    _goml_runtime_core_string_print(inline188)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t186 string = _goml_runtime_core_bool_to_string(self__66)
    return t186
}

func main() {
    main0()
}
