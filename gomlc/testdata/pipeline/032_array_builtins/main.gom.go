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
    var inline218 [3]int32 = arr__3
    var inline219 [3]int32 = inline218
    var inline220 int = 1
    array_get__Array_3_5int32(inline219, inline220)
    var inline222 int32 = 42
    var inline223 [3]int32 = array_set__Array_3_5int32(inline219, inline220, inline222)
    inline218 = inline223
    updated__4 = inline218
    var value__5 int32
    var inline216 int32 = array_get__Array_3_5int32(updated__4, 1)
    value__5 = inline216
    var t201 string
    var inline214 string = _goml_runtime_core_int32_to_string(value__5)
    t201 = inline214
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline211)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
