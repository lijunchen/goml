package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var x__0 int32 = 1
    var y__1 int8 = 1
    var inline441 string = "int32: "
    var inline442 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline441)
    _goml_runtime_core_string_print(inline442)
    var inline438 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__0)
    _goml_runtime_core_string_println(inline438)
    var inline434 string = "int8: "
    var inline435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline434)
    _goml_runtime_core_string_print(inline435)
    var inline431 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(y__1)
    _goml_runtime_core_string_println(inline431)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t426 string = _goml_runtime_core_int32_to_string(self__154)
    return t426
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__152 int8) string {
    var t429 string = _goml_runtime_core_int8_to_string(self__152)
    return t429
}

func main() {
    main0()
}
