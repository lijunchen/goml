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

type GoError = error

func update_array(arr__0 [3]int32) [3]int32 {
    var retv2 [3]int32
    var t3 [3]int32 = array_set__Array_3_5int32(arr__0, 1, 42)
    retv2 = t3
    return retv2
}

func read_array(arr__1 [3]int32) int32 {
    var retv5 int32
    var t6 int32 = array_get__Array_3_5int32(arr__1, 1)
    retv5 = t6
    return retv5
}

func main0() struct{} {
    var arr__2 [3]int32 = [3]int32{1, 2, 3}
    var updated__3 [3]int32 = update_array(arr__2)
    var value__4 int32 = read_array(updated__3)
    var t8 string = int32_to_string(value__4)
    string_println(t8)
    return struct{}{}
}

func main() {
    main0()
}
