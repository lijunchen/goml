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

type Box__string struct {
    value string
}

type Box__int32 struct {
    value int32
}

type Ordering int32

func _goml_m_trait__impl_i_Mark_i_int32_i_mark(self__0 int32) string {
    var inline438 string = _goml_runtime_core_int32_to_string(self__0)
    return inline438
}

func main0() struct{} {
    var t422 string
    var inline450 string = "text"
    var inline451 string = "string:" + inline450
    t422 = inline451
    var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline447)
    var t423 string
    var inline443 int32 = 7
    var inline444 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline443)
    var inline445 string = "blanket:" + inline444
    t423 = inline445
    var inline440 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline440)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
