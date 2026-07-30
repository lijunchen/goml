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
    var retv110 string
    var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv110 = t111
    return retv110
}

func render(x__1 dyn__Display) string {
    var retv113 string
    var t114 string = x__1.vtable.show(x__1.data)
    retv113 = t114
    return retv113
}

func main0() struct{} {
    var value__2 int32 = 42
    var t116 dyn__Display = dyn__Display{
        data: int32(value__2),
        vtable: dyn__Display__vtable__int32(),
    }
    var holder__3 Holder__dynDisplay = Holder__dynDisplay{
        value: t116,
    }
    var t117 dyn__Display = holder__3.value
    var t118 string = render(t117)
    println__T_string(t118)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv120 string
    var t121 string = _goml_runtime_core_int32_to_string(self__6)
    retv120 = t121
    return retv120
}

func println__T_string(value__1 string) struct{} {
    var t123 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t123)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv126 string
    retv126 = self__38
    return retv126
}

func main() {
    main0()
}
