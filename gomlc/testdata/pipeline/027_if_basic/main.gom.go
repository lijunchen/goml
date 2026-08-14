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
    var inline221 bool = true
    var inline222 int32 = 10
    var inline223 int32 = 99
    if inline221 {
        yes__3 = inline222
    } else {
        yes__3 = inline223
    }
    var no__4 int32
    var inline217 bool = false
    var inline218 int32 = 10
    var inline219 int32 = 99
    if inline217 {
        no__4 = inline218
    } else {
        no__4 = inline219
    }
    var t194 string
    var inline215 string = _goml_runtime_core_int32_to_string(yes__3)
    t194 = inline215
    var t195 string = "yes=" + t194
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline212)
    var t196 string
    var inline210 string = _goml_runtime_core_int32_to_string(no__4)
    t196 = inline210
    var t197 string = "no=" + t196
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
