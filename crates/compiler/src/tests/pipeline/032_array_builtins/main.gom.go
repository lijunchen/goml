package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func array_get__Array_3_int32(arr [3]int32, index int32) int32 {
    return arr[index]
}

func array_set__Array_3_int32(arr [3]int32, index int32, value int32) [3]int32 {
    arr[index] = value
    return arr
}

func update_array(arr__0 [3]int32) [3]int32 {
    var t1 [3]int32 = array_set__Array_3_int32(arr__0, 1, 42)
    return t1
}

func read_array(arr__1 [3]int32) int32 {
    var t2 int32 = array_get__Array_3_int32(arr__1, 1)
    return t2
}

func main0() struct{} {
    var arr__2 [3]int32 = [3]int32{1, 2, 3}
    var updated__3 [3]int32 = update_array(arr__2)
    var value__4 int32 = read_array(updated__3)
    var t3 string = int32_to_string(value__4)
    string_println(t3)
    return struct{}{}
}

func main() {
    main0()
}
