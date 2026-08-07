package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var value__0 int32 = 42
    var text__1 string
    var inline186 string = _goml_runtime_core_int32_to_string(value__0)
    text__1 = inline186
    var inline183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__1)
    _goml_runtime_core_string_println(inline183)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
