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

func array_get__Array_3_5int32(arr [3]int32, index int32) int32 {
    return arr[index]
}

func array_set__Array_3_5int32(arr [3]int32, index int32, value int32) [3]int32 {
    arr[index] = value
    return arr
}

func update_array(arr__0 [3]int32) [3]int32 {
    var retv68 [3]int32
    var arr__1 [3]int32 = arr__0
    var place_root61 [3]int32 = arr__1
    var index62 int32 = 1
    array_get__Array_3_5int32(place_root61, index62)
    var value64 int32 = 42
    var t69 [3]int32 = array_set__Array_3_5int32(place_root61, index62, value64)
    arr__1 = t69
    retv68 = arr__1
    return retv68
}

func read_array(arr__2 [3]int32) int32 {
    var retv72 int32
    var t73 int32 = array_get__Array_3_5int32(arr__2, 1)
    retv72 = t73
    return retv72
}

func main0() struct{} {
    var arr__3 [3]int32 = [3]int32{1, 2, 3}
    var updated__4 [3]int32 = update_array(arr__3)
    var value__5 int32 = read_array(updated__4)
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
    println__T_string(t75)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t77)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv80 string
    var t81 string = _goml_runtime_core_int32_to_string(self__5)
    retv80 = t81
    return retv80
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv83 string
    retv83 = self__37
    return retv83
}

func main() {
    main0()
}
