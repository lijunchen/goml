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
    var x182 bool = true
    var x183 bool = false
    switch x182 {
    case true:
        var inline199 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x183)
        _goml_runtime_core_string_print(inline199)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var x185 bool = true
    var x186 bool = true
    switch x185 {
    case true:
        var inline202 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x186)
        _goml_runtime_core_string_print(inline202)
        return struct{}{}
    case false:
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t197 string = _goml_runtime_core_bool_to_string(self__64)
    return t197
}

func main() {
    main0()
}
