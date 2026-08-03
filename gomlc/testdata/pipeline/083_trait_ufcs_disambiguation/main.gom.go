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

type S struct {}

func _goml_m_trait__impl_i_A_i_S_i_pick(self__0 S) int32 {
    return 10
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    return 20
}

func main0() struct{} {
    var t143 S = S{}
    var t144 int32
    var inline176 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(t143)
    t144 = inline176
    var t145 string
    var inline174 string = _goml_runtime_core_int32_to_string(t144)
    t145 = inline174
    var inline171 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t145)
    _goml_runtime_core_string_println(inline171)
    var t146 S = S{}
    var t147 int32
    var inline169 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(t146)
    t147 = inline169
    var t148 string
    var inline167 string = _goml_runtime_core_int32_to_string(t147)
    t148 = inline167
    var inline164 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
    _goml_runtime_core_string_println(inline164)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
