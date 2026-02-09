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

type Boxed struct {
    value int32
}

func _goml_inherent_Boxed_Boxed_format(self__0 Boxed) string {
    var ret7 string
    ret7 = "inherent"
    return ret7
}

func _goml_trait_impl_Render_Boxed_format(self__1 Boxed) string {
    var ret8 string
    var t2 int32 = self__1.value
    ret8 = int32_to_string(t2)
    return ret8
}

func main0() struct{} {
    var ret9 struct{}
    var t4 Boxed = Boxed{
        value: 9,
    }
    var t3 string = _goml_inherent_Boxed_Boxed_format(t4)
    println__T_string(t3)
    var t6 Boxed = Boxed{
        value: 9,
    }
    var t5 string = _goml_trait_impl_Render_Boxed_format(t6)
    println__T_string(t5)
    ret9 = struct{}{}
    return ret9
}

func println__T_string(value__1 string) struct{} {
    var ret10 struct{}
    ret10 = string_println(value__1)
    return ret10
}

func main() {
    main0()
}
