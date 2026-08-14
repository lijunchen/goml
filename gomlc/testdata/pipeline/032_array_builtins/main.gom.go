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

type Ordering int32

func main0() struct{} {
    var arr__3 [3]int32 = [3]int32{1, 2, 3}
    var updated__4 [3]int32
    var inline439 [3]int32 = arr__3
    var inline440 [3]int32 = inline439
    var inline441 int = 1
    array_get__Array_3_5int32(inline440, inline441)
    var inline443 int32 = 42
    var inline444 [3]int32 = array_set__Array_3_5int32(inline440, inline441, inline443)
    inline439 = inline444
    updated__4 = inline439
    var value__5 int32
    var inline437 int32 = array_get__Array_3_5int32(updated__4, 1)
    value__5 = inline437
    var t422 string
    var inline435 string = _goml_runtime_core_int32_to_string(value__5)
    t422 = inline435
    var inline432 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline432)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
