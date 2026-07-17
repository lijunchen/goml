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
    var retv63 string
    var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv63 = t64
    return retv63
}

func render(x__1 dyn__Display) string {
    var retv66 string
    var t67 string = x__1.vtable.show(x__1.data)
    retv66 = t67
    return retv66
}

func main0() struct{} {
    var t69 dyn__Display = dyn__Display{
        data: int32(42),
        vtable: dyn__Display__vtable__int32(),
    }
    var holder__2 Holder__dynDisplay = Holder__dynDisplay{
        value: t69,
    }
    var t70 dyn__Display = holder__2.value
    var t71 string = render(t70)
    println__T_string(t71)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv73 string
    var t74 string = _goml_runtime_core_int32_to_string(self__5)
    retv73 = t74
    return retv73
}

func println__T_string(value__1 string) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv79 string
    retv79 = self__37
    return retv79
}

func main() {
    main0()
}
