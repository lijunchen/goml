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
    var x136 bool = true
    var x137 bool = false
    switch x136 {
    case true:
        var inline153 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x137)
        _goml_runtime_core_string_print(inline153)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var x139 bool = true
    var x140 bool = true
    switch x139 {
    case true:
        var inline156 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x140)
        _goml_runtime_core_string_print(inline156)
        return struct{}{}
    case false:
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t151 string = _goml_runtime_core_bool_to_string(self__66)
    return t151
}

func main() {
    main0()
}
