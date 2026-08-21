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

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

const (
    BASE int = 16
    ANSWER int = BASE * 2 + 10
    MASK uint8 = 240
    GREETING string = "constant" + " value"
    ENABLED bool = true
)

func main0() struct{} {
    var t415 string
    var inline450 string = _goml_runtime_core_int_to_string(ANSWER)
    t415 = inline450
    var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t415)
    _goml_runtime_core_string_println(inline447)
    var t416 string
    var inline445 string = _goml_runtime_core_uint8_to_string(MASK)
    t416 = inline445
    var inline442 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t416)
    _goml_runtime_core_string_println(inline442)
    var inline439 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(GREETING)
    _goml_runtime_core_string_println(inline439)
    var t417 string
    var inline437 string = _goml_runtime_core_bool_to_string(ENABLED)
    t417 = inline437
    var inline434 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t417)
    _goml_runtime_core_string_println(inline434)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
