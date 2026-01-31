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
    var ret5 string
    var t3 int32 = self__0.value
    var t2 string = int32_to_string(t3)
    var t1 string = "S(" + t2
    ret5 = t1 + ")"
    return ret5
}

func main0() struct{} {
    var ret6 struct{}
    var s__1 S = S{
        value: 7,
    }
    var t4 string = _goml_trait_impl_ToString_S_to_string(s__1)
    println__T_string(t4)
    ret6 = struct{}{}
    return ret6
}

func println__T_string(value__1 string) struct{} {
    var ret7 struct{}
    ret7 = string_println(value__1)
    return ret7
}

func main() {
    main0()
}
