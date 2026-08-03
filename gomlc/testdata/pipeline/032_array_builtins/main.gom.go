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
    var inline208 [3]int32 = arr__3
    var inline209 [3]int32 = inline208
    var inline210 int = 1
    array_get__Array_3_5int32(inline209, inline210)
    var inline212 int32 = 42
    var inline213 [3]int32 = array_set__Array_3_5int32(inline209, inline210, inline212)
    inline208 = inline213
    updated__4 = inline208
    var value__5 int32
    var inline206 int32 = array_get__Array_3_5int32(updated__4, 1)
    value__5 = inline206
    var t191 string
    var inline204 string = _goml_runtime_core_int32_to_string(value__5)
    t191 = inline204
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
