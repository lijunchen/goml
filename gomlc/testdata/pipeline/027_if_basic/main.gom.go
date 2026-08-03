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
    var yes__3 int32
    var inline211 bool = true
    var inline212 int32 = 10
    var inline213 int32 = 99
    if inline211 {
        yes__3 = inline212
    } else {
        yes__3 = inline213
    }
    var no__4 int32
    var inline207 bool = false
    var inline208 int32 = 10
    var inline209 int32 = 99
    if inline207 {
        no__4 = inline208
    } else {
        no__4 = inline209
    }
    var t184 string
    var inline205 string = _goml_runtime_core_int32_to_string(yes__3)
    t184 = inline205
    var t185 string = "yes=" + t184
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline202)
    var t186 string
    var inline200 string = _goml_runtime_core_int32_to_string(no__4)
    t186 = inline200
    var t187 string = "no=" + t186
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline197)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
