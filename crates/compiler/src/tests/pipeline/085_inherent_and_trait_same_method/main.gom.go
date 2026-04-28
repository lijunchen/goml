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

type Boxed struct {
    value int32
}

type GoError = error

func _goml_inherent_Boxed_Boxed_format(self__0 Boxed) string {
    var retv3 string
    retv3 = "inherent"
    return retv3
}

func _goml_trait_impl_Render_Boxed_format(self__1 Boxed) string {
    var retv5 string
    var t6 int32 = self__1.value
    var t7 string = int32_to_string(t6)
    retv5 = t7
    return retv5
}

func main0() struct{} {
    var t9 Boxed = Boxed{
        value: 9,
    }
    var t10 string = _goml_inherent_Boxed_Boxed_format(t9)
    println__T_string(t10)
    var t11 Boxed = Boxed{
        value: 9,
    }
    var t12 string = _goml_trait_impl_Render_Boxed_format(t11)
    println__T_string(t12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
