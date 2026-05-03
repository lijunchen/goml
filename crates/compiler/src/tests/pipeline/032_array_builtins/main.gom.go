package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
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
    var retv7 [3]int32
    var arr__1 [3]int32 = arr__0
    var place_root0 [3]int32 = arr__1
    var index1 int32 = 1
    array_get__Array_3_5int32(place_root0, index1)
    var value3 int32 = 42
    var t8 [3]int32 = array_set__Array_3_5int32(place_root0, index1, value3)
    arr__1 = t8
    retv7 = arr__1
    return retv7
}

func read_array(arr__2 [3]int32) int32 {
    var retv11 int32
    var t12 int32 = array_get__Array_3_5int32(arr__2, 1)
    retv11 = t12
    return retv11
}

func main0() struct{} {
    var arr__3 [3]int32 = [3]int32{1, 2, 3}
    var updated__4 [3]int32 = update_array(arr__3)
    var value__5 int32 = read_array(updated__4)
    var t14 string = int32_to_string(value__5)
    println__T_string(t14)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
