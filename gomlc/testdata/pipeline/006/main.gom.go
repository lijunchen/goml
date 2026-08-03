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

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

func main0() struct{} {
    var x177 bool = true
    var x178 bool = false
    switch x177 {
    case true:
        var inline194 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x178)
        _goml_runtime_core_string_print(inline194)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var x180 bool = true
    var x181 bool = true
    switch x180 {
    case true:
        var inline197 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x181)
        _goml_runtime_core_string_print(inline197)
        return struct{}{}
    case false:
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t192 string = _goml_runtime_core_bool_to_string(self__66)
    return t192
}

func main() {
    main0()
}
