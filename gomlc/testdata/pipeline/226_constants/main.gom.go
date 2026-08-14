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
    BASE int = 16
    ANSWER int = BASE * 2 + 10
    MASK uint8 = 240
    GREETING string = "constant" + " value"
    ENABLED bool = true
)

func main0() struct{} {
    var t186 string
    var inline221 string = _goml_runtime_core_int_to_string(ANSWER)
    t186 = inline221
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline218)
    var t187 string
    var inline216 string = _goml_runtime_core_uint8_to_string(MASK)
    t187 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline213)
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(GREETING)
    _goml_runtime_core_string_println(inline210)
    var t188 string
    var inline208 string = _goml_runtime_core_bool_to_string(ENABLED)
    t188 = inline208
    var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
