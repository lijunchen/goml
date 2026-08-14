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
    var t191 string
    var inline226 string = _goml_runtime_core_int_to_string(ANSWER)
    t191 = inline226
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline223)
    var t192 string
    var inline221 string = _goml_runtime_core_uint8_to_string(MASK)
    t192 = inline221
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline218)
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(GREETING)
    _goml_runtime_core_string_println(inline215)
    var t193 string
    var inline213 string = _goml_runtime_core_bool_to_string(ENABLED)
    t193 = inline213
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
