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
    var t194 bool
    var inline208 int32 = 1
    var inline209 bool = n__2 < inline208
    var inline210 bool = !inline209
    if inline210 {
        var inline211 bool = inline208 < n__2
        var inline212 bool = !inline211
        t194 = inline212
    } else {
        t194 = false
    }
    if t194 {
        return 1
    } else {
        var t195 int32 = n__2 - 1
        var t196 int32 = sum(t195)
        var t197 int32 = n__2 + t196
        return t197
    }
}

func main0() struct{} {
    var t199 int32 = sum(100)
    var inline214 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
    _goml_runtime_core_string_println(inline214)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t206 string = _goml_runtime_core_int32_to_string(self__70)
    return t206
}

func main() {
    main0()
}
