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
    var x408 bool = true
    var x409 bool = false
    switch x408 {
    case true:
        var inline425 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x409)
        _goml_runtime_core_string_print(inline425)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var x411 bool = true
    var x412 bool = true
    switch x411 {
    case true:
        var inline428 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x412)
        _goml_runtime_core_string_print(inline428)
        return struct{}{}
    case false:
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t423 string = _goml_runtime_core_bool_to_string(self__148)
    return t423
}

func main() {
    main0()
}
