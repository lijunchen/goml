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

func main0() struct{} {
    var t163 string
    var inline178 int32 = 7
    var inline179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline178)
    var inline180 string = "S(" + inline179
    var inline181 string = inline180 + ")"
    t163 = inline181
    var inline175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline175)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t166 string = _goml_runtime_core_int32_to_string(self__6)
    return t166
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
