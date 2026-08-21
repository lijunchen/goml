package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var a__0 int8 = -128
    var t414 string
    var inline436 string = _goml_runtime_core_int8_to_string(a__0)
    t414 = inline436
    var inline433 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t414)
    _goml_runtime_core_string_println(inline433)
    var b__1 int16 = -32768
    var t415 string
    var inline431 string = _goml_runtime_core_int16_to_string(b__1)
    t415 = inline431
    var inline428 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t415)
    _goml_runtime_core_string_println(inline428)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
