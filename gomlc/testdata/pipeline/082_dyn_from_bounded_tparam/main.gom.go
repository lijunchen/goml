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
    var retv111 string
    retv111 = "ok"
    return retv111
}

func main0() struct{} {
    var t113 S = S{}
    to_dyn__T_S(t113)
    println__T_string("ok")
    return struct{}{}
}

func to_dyn__T_S(x__1 S) dyn__Display {
    var retv115 dyn__Display
    var t116 dyn__Display = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__S(),
    }
    retv115 = t116
    return retv115
}

func println__T_string(value__1 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t118)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv121 string
    retv121 = self__38
    return retv121
}

func main() {
    main0()
}
