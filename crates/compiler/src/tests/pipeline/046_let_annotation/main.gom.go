package main

import (
    "fmt"
)

func int8_to_string(x int8) string {
    return fmt.Sprintf("%d", x)
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var ret6 struct{}
    var x__0 int32 = 1
    var y__1 int8 = 1
    string_print("int32: ")
    var t4 string = int32_to_string(x__0)
    string_println(t4)
    string_print("int8: ")
    var t5 string = int8_to_string(y__1)
    string_println(t5)
    ret6 = struct{}{}
    return ret6
}

func main() {
    main0()
}
