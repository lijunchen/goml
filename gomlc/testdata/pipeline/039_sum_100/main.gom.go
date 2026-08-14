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
    var t199 bool
    var inline213 int32 = 1
    var inline214 bool = n__2 < inline213
    var inline215 bool = !inline214
    if inline215 {
        var inline216 bool = inline213 < n__2
        var inline217 bool = !inline216
        t199 = inline217
    } else {
        t199 = false
    }
    if t199 {
        return 1
    } else {
        var t200 int32 = n__2 - 1
        var t201 int32 = sum(t200)
        var t202 int32 = n__2 + t201
        return t202
    }
}

func main0() struct{} {
    var t204 int32 = sum(100)
    var inline219 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t204)
    _goml_runtime_core_string_println(inline219)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t211 string = _goml_runtime_core_int32_to_string(self__70)
    return t211
}

func main() {
    main0()
}
