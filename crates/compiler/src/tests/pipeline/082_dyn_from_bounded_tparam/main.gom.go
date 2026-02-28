package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type S struct {}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__S__show(self any) string {
    return _goml_trait_impl_Display_S_show(self.(S))
}

func dyn__Display__vtable__S() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__S__show,
    }
}

func _goml_trait_impl_Display_S_show(self__0 S) string {
    return "ok"
}

func main0() struct{} {
    var t2 S = S{}
    to_dyn__T_S(t2)
    println__T_string("ok")
    return struct{}{}
}

func to_dyn__T_S(x__1 S) dyn__Display {
    var t3 dyn__Display = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__S(),
    }
    return t3
}

func println__T_string(value__1 string) struct{} {
    var t4 struct{} = string_println(value__1)
    return t4
}

func main() {
    main0()
}
