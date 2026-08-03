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
    var first__3 int32 = 1
    var t140 int32
    var inline161 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(first__3)
    t140 = inline161
    var inline158 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t140)
    _goml_runtime_core_string_println(inline158)
    var t141 int32
    t141 = 42
    var inline154 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t141)
    _goml_runtime_core_string_println(inline154)
    return struct{}{}
}

func _goml_m_trait__impl_i_Extra_i_int32_i_extra(self__1 int32) int32 {
    return 42
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t152 string = _goml_runtime_core_int32_to_string(self__72)
    return t152
}

func main() {
    main0()
}
