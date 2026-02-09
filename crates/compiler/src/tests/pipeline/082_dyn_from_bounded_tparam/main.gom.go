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
    var ret3 string
    ret3 = "ok"
    return ret3
}

func main0() struct{} {
    var ret4 struct{}
    var t2 S = S{}
    to_dyn__T_S(t2)
    println__T_string("ok")
    ret4 = struct{}{}
    return ret4
}

func to_dyn__T_S(x__1 S) dyn__Display {
    var ret5 dyn__Display
    ret5 = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__S(),
    }
    return ret5
}

func println__T_string(value__1 string) struct{} {
    var ret6 struct{}
    ret6 = string_println(value__1)
    return ret6
}

func main() {
    main0()
}
