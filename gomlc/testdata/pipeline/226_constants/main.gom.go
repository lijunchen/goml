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
    var t159 string = _goml_m_inherent_i_int_i_int_i_to__string(answer)
    _goml_runtime_core_string_println(t159)
    var t160 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(mask)
    _goml_runtime_core_string_println(t160)
    _goml_runtime_core_string_println(greeting)
    var t161 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(enabled)
    _goml_runtime_core_string_println(t161)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv164 string
    var t165 string = _goml_runtime_core_int_to_string(self__5)
    retv164 = t165
    return retv164
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv167 string
    var t168 string = _goml_runtime_core_uint8_to_string(self__45)
    retv167 = t168
    return retv167
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv170 string
    var t171 string = _goml_runtime_core_bool_to_string(self__37)
    retv170 = t171
    return retv170
}

func main() {
    main0()
}
