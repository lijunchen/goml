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

func sum(n__2 int32) int32 {
    var t189 bool
    var inline203 int32 = 1
    var inline204 bool = n__2 < inline203
    var inline205 bool = !inline204
    if inline205 {
        var inline206 bool = inline203 < n__2
        var inline207 bool = !inline206
        t189 = inline207
    } else {
        t189 = false
    }
    if t189 {
        return 1
    } else {
        var t190 int32 = n__2 - 1
        var t191 int32 = sum(t190)
        var t192 int32 = n__2 + t191
        return t192
    }
}

func main0() struct{} {
    var t194 int32 = sum(100)
    var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t194)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t201 string = _goml_runtime_core_int32_to_string(self__72)
    return t201
}

func main() {
    main0()
}
