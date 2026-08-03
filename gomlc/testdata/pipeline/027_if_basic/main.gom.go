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
    var inline170 bool = true
    var inline171 int32 = 10
    var inline172 int32 = 99
    if inline170 {
        yes__3 = inline171
    } else {
        yes__3 = inline172
    }
    var no__4 int32
    var inline166 bool = false
    var inline167 int32 = 10
    var inline168 int32 = 99
    if inline166 {
        no__4 = inline167
    } else {
        no__4 = inline168
    }
    var t143 string
    var inline164 string = _goml_runtime_core_int32_to_string(yes__3)
    t143 = inline164
    var t144 string = "yes=" + t143
    var inline161 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t144)
    _goml_runtime_core_string_println(inline161)
    var t145 string
    var inline159 string = _goml_runtime_core_int32_to_string(no__4)
    t145 = inline159
    var t146 string = "no=" + t145
    var inline156 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
    _goml_runtime_core_string_println(inline156)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
