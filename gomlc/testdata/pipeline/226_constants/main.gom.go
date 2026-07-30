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
    var t112 string = _goml_m_inherent_i_int_i_int_i_to__string(answer)
    _goml_runtime_core_string_println(t112)
    var t113 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(mask)
    _goml_runtime_core_string_println(t113)
    _goml_runtime_core_string_println(greeting)
    var t114 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(enabled)
    _goml_runtime_core_string_println(t114)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv117 string
    var t118 string = _goml_runtime_core_int_to_string(self__5)
    retv117 = t118
    return retv117
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv120 string
    var t121 string = _goml_runtime_core_uint8_to_string(self__45)
    retv120 = t121
    return retv120
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv123 string
    var t124 string = _goml_runtime_core_bool_to_string(self__37)
    retv123 = t124
    return retv123
}

func main() {
    main0()
}
