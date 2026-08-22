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

type Value struct {}

type Ordering int32

func main0() struct{} {
    var text__2 string
    text__2 = "isize"
    var number__3 int32
    number__3 = 7
    var inline435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__2)
    _goml_runtime_core_string_println(inline435)
    var inline432 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(number__3)
    _goml_runtime_core_string_println(inline432)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t430 string = _goml_runtime_core_int32_to_string(self__154)
    return t430
}

func main() {
    main0()
}
