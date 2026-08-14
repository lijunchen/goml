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
    var t189 S = S{}
    var t190 int32
    var inline222 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(t189)
    t190 = inline222
    var t191 string
    var inline220 string = _goml_runtime_core_int32_to_string(t190)
    t191 = inline220
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline217)
    var t192 S = S{}
    var t193 int32
    var inline215 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(t192)
    t193 = inline215
    var t194 string
    var inline213 string = _goml_runtime_core_int32_to_string(t193)
    t194 = inline213
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
