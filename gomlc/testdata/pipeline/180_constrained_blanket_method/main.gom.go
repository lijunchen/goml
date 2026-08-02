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
    var t159 int32
    var inline180 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(first__3)
    t159 = inline180
    var inline177 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t159)
    _goml_runtime_core_string_println(inline177)
    var t160 int32
    t160 = 42
    var inline173 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t160)
    _goml_runtime_core_string_println(inline173)
    return struct{}{}
}

func _goml_m_trait__impl_i_Extra_i_int32_i_extra(self__1 int32) int32 {
    return 42
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t171 string = _goml_runtime_core_int32_to_string(self__43)
    return t171
}

func main() {
    main0()
}
