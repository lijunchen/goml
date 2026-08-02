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
    var t167 bool
    var inline181 int32 = 1
    var inline182 bool = n__2 < inline181
    var inline183 bool = !inline182
    if inline183 {
        var inline184 bool = inline181 < n__2
        var inline185 bool = !inline184
        t167 = inline185
    } else {
        t167 = false
    }
    if t167 {
        return 1
    } else {
        var t168 int32 = n__2 - 1
        var t169 int32 = sum(t168)
        var t170 int32 = n__2 + t169
        return t170
    }
}

func main0() struct{} {
    var t172 int32 = sum(100)
    var inline187 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t172)
    _goml_runtime_core_string_println(inline187)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t179 string = _goml_runtime_core_int32_to_string(self__43)
    return t179
}

func main() {
    main0()
}
