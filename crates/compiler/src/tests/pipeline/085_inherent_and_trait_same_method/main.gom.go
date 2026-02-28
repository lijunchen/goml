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
    return "inherent"
}

func _goml_trait_impl_Render_Boxed_format(self__1 Boxed) string {
    var t2 int32
    var t3 string
    t2 = self__1.value
    t3 = int32_to_string(t2)
    return t3
}

func main0() struct{} {
    var t4 Boxed
    var t5 string
    var t6 Boxed
    var t7 string
    t4 = Boxed{
        value: 9,
    }
    t5 = _goml_inherent_Boxed_Boxed_format(t4)
    println__T_string(t5)
    t6 = Boxed{
        value: 9,
    }
    t7 = _goml_trait_impl_Render_Boxed_format(t6)
    println__T_string(t7)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t8 struct{}
    t8 = string_println(value__1)
    return t8
}

func main() {
    main0()
}
