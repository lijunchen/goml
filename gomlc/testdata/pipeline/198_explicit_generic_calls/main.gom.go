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

type Ordering int32

func main0() struct{} {
    var t410 string
    var inline437 string = "direct"
    t410 = inline437
    var inline434 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t410)
    _goml_runtime_core_string_println(inline434)
    var t411 int32
    var inline432 int32 = 42
    t411 = inline432
    var t412 string
    var inline430 string = _goml_runtime_core_int32_to_string(t411)
    t412 = inline430
    var inline427 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t412)
    _goml_runtime_core_string_println(inline427)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
