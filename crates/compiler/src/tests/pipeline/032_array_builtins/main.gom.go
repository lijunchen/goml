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
    var retv11 [3]int32
    var arr__1 [3]int32 = arr__0
    var place_root4 [3]int32 = arr__1
    var index5 int32 = 1
    array_get__Array_3_5int32(place_root4, index5)
    var value7 int32 = 42
    var t12 [3]int32 = array_set__Array_3_5int32(place_root4, index5, value7)
    arr__1 = t12
    retv11 = arr__1
    return retv11
}

func read_array(arr__2 [3]int32) int32 {
    var retv15 int32
    var t16 int32 = array_get__Array_3_5int32(arr__2, 1)
    retv15 = t16
    return retv15
}

func main0() struct{} {
    var arr__3 [3]int32 = [3]int32{1, 2, 3}
    var updated__4 [3]int32 = update_array(arr__3)
    var value__5 int32 = read_array(updated__4)
    var t18 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
    println__T_string(t18)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t20 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t20)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv23 string
    var t24 string = _goml_runtime_core_int32_to_string(self__2)
    retv23 = t24
    return retv23
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv26 string
    retv26 = self__9
    return retv26
}

func main() {
    main0()
}
