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
    var retv14 [3]int32
    var arr__1 [3]int32 = arr__0
    var place_root7 [3]int32 = arr__1
    var index8 int32 = 1
    array_get__Array_3_5int32(place_root7, index8)
    var value10 int32 = 42
    var t15 [3]int32 = array_set__Array_3_5int32(place_root7, index8, value10)
    arr__1 = t15
    retv14 = arr__1
    return retv14
}

func read_array(arr__2 [3]int32) int32 {
    var retv18 int32
    var t19 int32 = array_get__Array_3_5int32(arr__2, 1)
    retv18 = t19
    return retv18
}

func main0() struct{} {
    var arr__3 [3]int32 = [3]int32{1, 2, 3}
    var updated__4 [3]int32 = update_array(arr__3)
    var value__5 int32 = read_array(updated__4)
    var t21 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
    println__T_string(t21)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t23 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t23)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv26 string
    var t27 string = _goml_runtime_core_int32_to_string(self__2)
    retv26 = t27
    return retv26
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv29 string
    retv29 = self__9
    return retv29
}

func main() {
    main0()
}
