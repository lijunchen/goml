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
    var retv162 [3]int32
    var arr__1 [3]int32 = arr__0
    var place_root155 [3]int32 = arr__1
    var index156 int = 1
    array_get__Array_3_5int32(place_root155, index156)
    var value158 int32 = 42
    var t163 [3]int32 = array_set__Array_3_5int32(place_root155, index156, value158)
    arr__1 = t163
    retv162 = arr__1
    return retv162
}

func read_array(arr__2 [3]int32) int32 {
    var retv166 int32
    var t167 int32 = array_get__Array_3_5int32(arr__2, 1)
    retv166 = t167
    return retv166
}

func main0() struct{} {
    var arr__3 [3]int32 = [3]int32{1, 2, 3}
    var updated__4 [3]int32 = update_array(arr__3)
    var value__5 int32 = read_array(updated__4)
    var t169 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
    println__T_string(t169)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t171 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t171)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv174 string
    var t175 string = _goml_runtime_core_int32_to_string(self__6)
    retv174 = t175
    return retv174
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv177 string
    retv177 = self__38
    return retv177
}

func main() {
    main0()
}
