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
    var inline444 string = "int32: "
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline444)
    _goml_runtime_core_string_print(inline445)
    var inline441 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x__0)
    _goml_runtime_core_string_println(inline441)
    var inline437 string = "int8: "
    var inline438 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline437)
    _goml_runtime_core_string_print(inline438)
    var inline434 string = _goml_m_trait__impl_i_ToString_i_i8_i_to__string(y__1)
    _goml_runtime_core_string_println(inline434)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t429 string = _goml_runtime_core_int32_to_string(self__154)
    return t429
}

func _goml_m_trait__impl_i_ToString_i_i8_i_to__string(self__152 int8) string {
    var t432 string = _goml_runtime_core_int8_to_string(self__152)
    return t432
}

func main() {
    main0()
}
