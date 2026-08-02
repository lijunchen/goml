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

func array_get__Array_3_5int32(arr [3]int32, index int) int32 {
    return arr[index]
}

func array_set__Array_3_5int32(arr [3]int32, index int, value int32) [3]int32 {
    arr[index] = value
    return arr
}

func main0() struct{} {
    var arr__3 [3]int32 = [3]int32{1, 2, 3}
    var updated__4 [3]int32
    var inline186 [3]int32 = arr__3
    var inline187 [3]int32 = inline186
    var inline188 int = 1
    array_get__Array_3_5int32(inline187, inline188)
    var inline190 int32 = 42
    var inline191 [3]int32 = array_set__Array_3_5int32(inline187, inline188, inline190)
    inline186 = inline191
    updated__4 = inline186
    var value__5 int32
    var inline184 int32 = array_get__Array_3_5int32(updated__4, 1)
    value__5 = inline184
    var t169 string
    var inline182 string = _goml_runtime_core_int32_to_string(value__5)
    t169 = inline182
    var inline179 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
    _goml_runtime_core_string_println(inline179)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
