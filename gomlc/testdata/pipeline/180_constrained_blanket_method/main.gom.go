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
    var t176 int32
    var inline197 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(first__3)
    t176 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t176)
    _goml_runtime_core_string_println(inline194)
    var t177 int32
    t177 = 42
    var inline190 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t177)
    _goml_runtime_core_string_println(inline190)
    return struct{}{}
}

func _goml_m_trait__impl_i_Extra_i_int32_i_extra(self__1 int32) int32 {
    return 42
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t188 string = _goml_runtime_core_int32_to_string(self__70)
    return t188
}

func main() {
    main0()
}
