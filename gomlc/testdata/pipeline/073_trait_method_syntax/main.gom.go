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

type S struct {
    value int32
}

type Ordering int32

func main0() struct{} {
    var t419 string
    var inline434 int32 = 7
    var inline435 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline434)
    var inline436 string = "S(" + inline435
    var inline437 string = inline436 + ")"
    t419 = inline437
    var inline431 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline431)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t422 string = _goml_runtime_core_int32_to_string(self__33)
    return t422
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
