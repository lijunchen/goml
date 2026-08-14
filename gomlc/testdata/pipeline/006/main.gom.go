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
    var x187 bool = true
    var x188 bool = false
    switch x187 {
    case true:
        var inline204 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x188)
        _goml_runtime_core_string_print(inline204)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var x190 bool = true
    var x191 bool = true
    switch x190 {
    case true:
        var inline207 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x191)
        _goml_runtime_core_string_print(inline207)
        return struct{}{}
    case false:
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t202 string = _goml_runtime_core_bool_to_string(self__64)
    return t202
}

func main() {
    main0()
}
