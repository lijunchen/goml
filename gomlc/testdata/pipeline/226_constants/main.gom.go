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
    var t159 string
    var inline194 string = _goml_runtime_core_int_to_string(answer)
    t159 = inline194
    var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
    _goml_runtime_core_string_println(inline191)
    var t160 string
    var inline189 string = _goml_runtime_core_uint8_to_string(mask)
    t160 = inline189
    var inline186 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline186)
    var inline183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(greeting)
    _goml_runtime_core_string_println(inline183)
    var t161 string
    var inline181 string = _goml_runtime_core_bool_to_string(enabled)
    t161 = inline181
    var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t161)
    _goml_runtime_core_string_println(inline178)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
