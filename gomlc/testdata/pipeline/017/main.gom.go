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

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

type Ordering int32

func main0() struct{} {
    var x__1 int = 123
    var t417 string
    var inline444 string = _goml_runtime_core_int_to_string(x__1)
    t417 = inline444
    var inline441 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t417)
    _goml_runtime_core_string_println(inline441)
    var x__2 bool = true
    var t418 string
    var inline439 string = _goml_runtime_core_bool_to_string(x__2)
    t418 = inline439
    var inline436 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
    _goml_runtime_core_string_println(inline436)
    var t419 string
    t419 = "(?, ?)"
    var inline432 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline432)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
