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

type Holder__dynDisplay struct {
    value dyn__Display
}

type GoError = error

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__int32__show(self any) string {
    return _goml_trait_impl_Display_int32_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_trait_impl_Display_int32_show(self__0 int32) string {
    var retv2 string
    var t3 string = int32_to_string(self__0)
    retv2 = t3
    return retv2
}

func render(x__1 dyn__Display) string {
    var retv5 string
    var t6 string = x__1.vtable.show(x__1.data)
    retv5 = t6
    return retv5
}

func main0() struct{} {
    var t8 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var holder__2 Holder__dynDisplay = Holder__dynDisplay{
        value: t8,
    }
    var t9 dyn__Display = holder__2.value
    var t10 string = render(t9)
    println__T_string(t10)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
