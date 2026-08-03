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

const (
    base int = 16
    answer int = base * 2 + 10
    mask uint8 = 240
    greeting string = "constant" + " value"
    enabled bool = true
)

func main0() struct{} {
    var t181 string
    var inline216 string = _goml_runtime_core_int_to_string(answer)
    t181 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline213)
    var t182 string
    var inline211 string = _goml_runtime_core_uint8_to_string(mask)
    t182 = inline211
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline208)
    var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(greeting)
    _goml_runtime_core_string_println(inline205)
    var t183 string
    var inline203 string = _goml_runtime_core_bool_to_string(enabled)
    t183 = inline203
    var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline200)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
