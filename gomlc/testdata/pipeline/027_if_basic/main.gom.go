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
    var inline206 bool = true
    var inline207 int32 = 10
    var inline208 int32 = 99
    if inline206 {
        yes__3 = inline207
    } else {
        yes__3 = inline208
    }
    var no__4 int32
    var inline202 bool = false
    var inline203 int32 = 10
    var inline204 int32 = 99
    if inline202 {
        no__4 = inline203
    } else {
        no__4 = inline204
    }
    var t179 string
    var inline200 string = _goml_runtime_core_int32_to_string(yes__3)
    t179 = inline200
    var t180 string = "yes=" + t179
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline197)
    var t181 string
    var inline195 string = _goml_runtime_core_int32_to_string(no__4)
    t181 = inline195
    var t182 string = "no=" + t181
    var inline192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline192)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
