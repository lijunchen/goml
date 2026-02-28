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
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return "ok"
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var t2 S
    var mtmp0 dyn__Display
    var _wild1 struct{}
    _ = mtmp0
    _ = _wild1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t2 = S{}
            to_dyn__T_S(t2)
            println__T_string("ok")
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func to_dyn__T_S(x__1 S) dyn__Display {
    var t3 dyn__Display
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t3 = dyn__Display{
                data: x__1,
                vtable: dyn__Display__vtable__S(),
            }
            return t3
        default:
            panic("invalid pc")
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t4 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t4 = string_println(value__1)
            return t4
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
