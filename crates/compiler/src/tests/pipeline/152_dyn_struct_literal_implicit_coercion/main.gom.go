package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Holder__dynDisplay struct {
    value dyn__Display
}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_int32_i_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Display_i_int32_i_show(self__0 int32) string {
    var retv60 string
    var t61 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv60 = t61
    return retv60
}

func render(x__1 dyn__Display) string {
    var retv63 string
    var t64 string = x__1.vtable.show(x__1.data)
    retv63 = t64
    return retv63
}

func main0() struct{} {
    var t66 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var holder__2 Holder__dynDisplay = Holder__dynDisplay{
        value: t66,
    }
    var t67 dyn__Display = holder__2.value
    var t68 string = render(t67)
    println__T_string(t68)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv70 string
    var t71 string = _goml_runtime_core_int32_to_string(self__2)
    retv70 = t71
    return retv70
}

func println__T_string(value__1 string) struct{} {
    var t73 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t73)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv76 string
    retv76 = self__34
    return retv76
}

func main() {
    main0()
}
