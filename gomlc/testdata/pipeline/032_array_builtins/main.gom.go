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
    var inline167 [3]int32 = arr__3
    var inline168 [3]int32 = inline167
    var inline169 int = 1
    array_get__Array_3_5int32(inline168, inline169)
    var inline171 int32 = 42
    var inline172 [3]int32 = array_set__Array_3_5int32(inline168, inline169, inline171)
    inline167 = inline172
    updated__4 = inline167
    var value__5 int32
    var inline165 int32 = array_get__Array_3_5int32(updated__4, 1)
    value__5 = inline165
    var t150 string
    var inline163 string = _goml_runtime_core_int32_to_string(value__5)
    t150 = inline163
    var inline160 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t150)
    _goml_runtime_core_string_println(inline160)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
