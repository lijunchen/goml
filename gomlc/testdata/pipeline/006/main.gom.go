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
    var x172 bool = true
    var x173 bool = false
    switch x172 {
    case true:
        var inline189 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x173)
        _goml_runtime_core_string_print(inline189)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var x175 bool = true
    var x176 bool = true
    switch x175 {
    case true:
        var inline192 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x176)
        _goml_runtime_core_string_print(inline192)
        return struct{}{}
    case false:
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t187 string = _goml_runtime_core_bool_to_string(self__64)
    return t187
}

func main() {
    main0()
}
