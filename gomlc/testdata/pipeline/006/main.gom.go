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
    var x411 bool = true
    var x412 bool = false
    switch x411 {
    case true:
        var inline428 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x412)
        _goml_runtime_core_string_print(inline428)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var x414 bool = true
    var x415 bool = true
    switch x414 {
    case true:
        var inline431 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x415)
        _goml_runtime_core_string_print(inline431)
        return struct{}{}
    case false:
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t426 string = _goml_runtime_core_bool_to_string(self__148)
    return t426
}

func main() {
    main0()
}
