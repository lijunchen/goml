package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
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
    return _goml_m_trait__impl_i_Display_i_S_i_show(self.(S))
}

func dyn__Display__vtable__S() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__S__show,
    }
}

func _goml_m_trait__impl_i_Display_i_S_i_show(self__0 S) string {
    var retv25 string
    retv25 = "ok"
    return retv25
}

func main0() struct{} {
    var t27 S = S{}
    to_dyn__T_S(t27)
    println__T_string("ok")
    return struct{}{}
}

func to_dyn__T_S(x__1 S) dyn__Display {
    var retv29 dyn__Display
    var t30 dyn__Display = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__S(),
    }
    retv29 = t30
    return retv29
}

func println__T_string(value__1 string) struct{} {
    var t32 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t32)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv35 string
    retv35 = self__9
    return retv35
}

func main() {
    main0()
}
