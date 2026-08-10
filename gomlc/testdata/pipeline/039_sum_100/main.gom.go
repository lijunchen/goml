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
    var t184 bool
    var inline198 int32 = 1
    var inline199 bool = n__2 < inline198
    var inline200 bool = !inline199
    if inline200 {
        var inline201 bool = inline198 < n__2
        var inline202 bool = !inline201
        t184 = inline202
    } else {
        t184 = false
    }
    if t184 {
        return 1
    } else {
        var t185 int32 = n__2 - 1
        var t186 int32 = sum(t185)
        var t187 int32 = n__2 + t186
        return t187
    }
}

func main0() struct{} {
    var t189 int32 = sum(100)
    var inline204 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t189)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t196 string = _goml_runtime_core_int32_to_string(self__70)
    return t196
}

func main() {
    main0()
}
