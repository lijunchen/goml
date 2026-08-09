package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Number struct {
    value int
}

type dyn__Display_vtable struct {
    display func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

type dyn__Source_vtable struct {
    get func(any) int
}

type dyn__Source struct {
    data any
    vtable *dyn__Source_vtable
}

func dyn__Display__wrap__Number__display(self any) string {
    return _goml_m_trait__impl_i_Display_i_Number_i_display(self.(Number))
}

func dyn__Display__vtable__Number() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        display: dyn__Display__wrap__Number__display,
    }
}

func dyn__Source__wrap__Number__get(self any) int {
    return _goml_m_trait__impl_i_Source_i_Number_i_get(self.(Number))
}

func dyn__Source__vtable__Number() *dyn__Source_vtable {
    return &dyn__Source_vtable{
        get: dyn__Source__wrap__Number__get,
    }
}

func _goml_m_trait__impl_i_Display_i_Number_i_display(self__0 Number) string {
    var t177 int = self__0.value
    var inline210 string = _goml_runtime_core_int_to_string(t177)
    return inline210
}

func _goml_m_trait__impl_i_Source_i_Number_i_get(self__1 Number) int {
    var t181 int = self__1.value
    return t181
}

func main0() struct{} {
    var t183 Number = Number{
        value: 42,
    }
    var display__3 dyn__Display = dyn__Display{
        data: t183,
        vtable: dyn__Display__vtable__Number(),
    }
    var t184 string = display__3.vtable.display(display__3.data)
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline223)
    var t185 Number = Number{
        value: 7,
    }
    var erased__4 dyn__Display
    var inline221 dyn__Display = dyn__Display{
        data: t185,
        vtable: dyn__Display__vtable__Number(),
    }
    erased__4 = inline221
    var t186 string = erased__4.vtable.display(erased__4.data)
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline218)
    var t187 Number = Number{
        value: 11,
    }
    var source__5 dyn__Source = dyn__Source{
        data: t187,
        vtable: dyn__Source__vtable__Number(),
    }
    var t188 int = source__5.vtable.get(source__5.data)
    var inline215 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t188)
    _goml_runtime_core_string_println(inline215)
    var t189 Number = Number{
        value: 13,
    }
    var same__6 dyn__Display = dyn__Display{
        data: t189,
        vtable: dyn__Display__vtable__Number(),
    }
    var t190 string = same__6.vtable.display(same__6.data)
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline212)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t208 string = _goml_runtime_core_int_to_string(self__69)
    return t208
}

func main() {
    main0()
}
