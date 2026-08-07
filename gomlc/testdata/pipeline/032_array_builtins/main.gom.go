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
    var inline203 [3]int32 = arr__3
    var inline204 [3]int32 = inline203
    var inline205 int = 1
    array_get__Array_3_5int32(inline204, inline205)
    var inline207 int32 = 42
    var inline208 [3]int32 = array_set__Array_3_5int32(inline204, inline205, inline207)
    inline203 = inline208
    updated__4 = inline203
    var value__5 int32
    var inline201 int32 = array_get__Array_3_5int32(updated__4, 1)
    value__5 = inline201
    var t186 string
    var inline199 string = _goml_runtime_core_int32_to_string(value__5)
    t186 = inline199
    var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline196)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
