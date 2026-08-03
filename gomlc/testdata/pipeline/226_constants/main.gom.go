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
    var t140 string
    var inline175 string = _goml_runtime_core_int_to_string(answer)
    t140 = inline175
    var inline172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t140)
    _goml_runtime_core_string_println(inline172)
    var t141 string
    var inline170 string = _goml_runtime_core_uint8_to_string(mask)
    t141 = inline170
    var inline167 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t141)
    _goml_runtime_core_string_println(inline167)
    var inline164 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(greeting)
    _goml_runtime_core_string_println(inline164)
    var t142 string
    var inline162 string = _goml_runtime_core_bool_to_string(enabled)
    t142 = inline162
    var inline159 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t142)
    _goml_runtime_core_string_println(inline159)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
