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
    var t1 [3]int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t1 = array_set__Array_3_int32(arr__0, 1, 42)
            return t1
        default:
            panic("invalid pc")
        }
    }
}

func read_array(arr__1 [3]int32) int32 {
    var t2 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t2 = array_get__Array_3_int32(arr__1, 1)
            return t2
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var arr__2 [3]int32
    var updated__3 [3]int32
    var value__4 int32
    var t3 string
    var mtmp0 struct{}
    _ = mtmp0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            arr__2 = [3]int32{1, 2, 3}
            updated__3 = update_array(arr__2)
            value__4 = read_array(updated__3)
            t3 = int32_to_string(value__4)
            string_println(t3)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
