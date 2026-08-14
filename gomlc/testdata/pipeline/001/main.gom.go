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

type Ordering int32

func main0() struct{} {
    var x413 bool = false
    var inline424 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x413)
    _goml_runtime_core_string_print(inline424)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t422 string = _goml_runtime_core_bool_to_string(self__148)
    return t422
}

func main() {
    main0()
}
