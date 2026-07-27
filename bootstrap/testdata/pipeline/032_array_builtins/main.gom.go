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

func update_array(arr__0 [3]int32) [3]int32 {
    var retv71 [3]int32
    var arr__1 [3]int32 = arr__0
    var place_root64 [3]int32 = arr__1
    var index65 int = 1
    array_get__Array_3_5int32(place_root64, index65)
    var value67 int32 = 42
    var t72 [3]int32 = array_set__Array_3_5int32(place_root64, index65, value67)
    arr__1 = t72
    retv71 = arr__1
    return retv71
}

func read_array(arr__2 [3]int32) int32 {
    var retv75 int32
    var t76 int32 = array_get__Array_3_5int32(arr__2, 1)
    retv75 = t76
    return retv75
}

func main0() struct{} {
    var arr__3 [3]int32 = [3]int32{1, 2, 3}
    var updated__4 [3]int32 = update_array(arr__3)
    var value__5 int32 = read_array(updated__4)
    var t78 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
    println__T_string(t78)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t80)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv83 string
    var t84 string = _goml_runtime_core_int32_to_string(self__6)
    retv83 = t84
    return retv83
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv86 string
    retv86 = self__38
    return retv86
}

func main() {
    main0()
}
