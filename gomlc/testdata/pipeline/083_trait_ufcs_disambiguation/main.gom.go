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
    var t194 S = S{}
    var t195 int32
    var inline227 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(t194)
    t195 = inline227
    var t196 string
    var inline225 string = _goml_runtime_core_int32_to_string(t195)
    t196 = inline225
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline222)
    var t197 S = S{}
    var t198 int32
    var inline220 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(t197)
    t198 = inline220
    var t199 string
    var inline218 string = _goml_runtime_core_int32_to_string(t198)
    t199 = inline218
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline215)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
