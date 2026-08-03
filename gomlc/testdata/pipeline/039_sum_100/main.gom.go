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
    var t148 bool
    var inline162 int32 = 1
    var inline163 bool = n__2 < inline162
    var inline164 bool = !inline163
    if inline164 {
        var inline165 bool = inline162 < n__2
        var inline166 bool = !inline165
        t148 = inline166
    } else {
        t148 = false
    }
    if t148 {
        return 1
    } else {
        var t149 int32 = n__2 - 1
        var t150 int32 = sum(t149)
        var t151 int32 = n__2 + t150
        return t151
    }
}

func main0() struct{} {
    var t153 int32 = sum(100)
    var inline168 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t153)
    _goml_runtime_core_string_println(inline168)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t160 string = _goml_runtime_core_int32_to_string(self__72)
    return t160
}

func main() {
    main0()
}
