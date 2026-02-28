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

type S struct {
    value int32
}

func _goml_trait_impl_ToString_S_to_string(self__0 S) string {
    var t1 int32 = self__0.value
    var t2 string = int32_to_string(t1)
    var t3 string = "S(" + t2
    var t4 string = t3 + ")"
    return t4
}

func main0() struct{} {
    var s__1 S = S{
        value: 7,
    }
    var t5 string = _goml_trait_impl_ToString_S_to_string(s__1)
    println__T_string(t5)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t6 struct{} = string_println(value__1)
    return t6
}

func main() {
    main0()
}
