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
    var t72 string = _goml_m_inherent_i_int_i_int_i_to__string(answer)
    _goml_runtime_core_string_println(t72)
    var t73 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(mask)
    _goml_runtime_core_string_println(t73)
    _goml_runtime_core_string_println(greeting)
    var t74 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(enabled)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv77 string
    var t78 string = _goml_runtime_core_int_to_string(self__5)
    retv77 = t78
    return retv77
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv80 string
    var t81 string = _goml_runtime_core_uint8_to_string(self__45)
    retv80 = t81
    return retv80
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv83 string
    var t84 string = _goml_runtime_core_bool_to_string(self__37)
    retv83 = t84
    return retv83
}

func main() {
    main0()
}
