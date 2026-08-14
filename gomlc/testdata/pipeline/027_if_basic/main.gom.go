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
    var inline216 bool = true
    var inline217 int32 = 10
    var inline218 int32 = 99
    if inline216 {
        yes__3 = inline217
    } else {
        yes__3 = inline218
    }
    var no__4 int32
    var inline212 bool = false
    var inline213 int32 = 10
    var inline214 int32 = 99
    if inline212 {
        no__4 = inline213
    } else {
        no__4 = inline214
    }
    var t189 string
    var inline210 string = _goml_runtime_core_int32_to_string(yes__3)
    t189 = inline210
    var t190 string = "yes=" + t189
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline207)
    var t191 string
    var inline205 string = _goml_runtime_core_int32_to_string(no__4)
    t191 = inline205
    var t192 string = "no=" + t191
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
