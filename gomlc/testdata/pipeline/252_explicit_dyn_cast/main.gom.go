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
    var t192 int = self__0.value
    var inline225 string = _goml_runtime_core_int_to_string(t192)
    return inline225
}

func _goml_m_trait__impl_i_Source_i_Number_i_get(self__1 Number) int {
    var t196 int = self__1.value
    return t196
}

func main0() struct{} {
    var t198 Number = Number{
        value: 42,
    }
    var display__3 dyn__Display = dyn__Display{
        data: t198,
        vtable: dyn__Display__vtable__Number(),
    }
    var t199 string = display__3.vtable.display(display__3.data)
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline238)
    var t200 Number = Number{
        value: 7,
    }
    var erased__4 dyn__Display
    var inline236 dyn__Display = dyn__Display{
        data: t200,
        vtable: dyn__Display__vtable__Number(),
    }
    erased__4 = inline236
    var t201 string = erased__4.vtable.display(erased__4.data)
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline233)
    var t202 Number = Number{
        value: 11,
    }
    var source__5 dyn__Source = dyn__Source{
        data: t202,
        vtable: dyn__Source__vtable__Number(),
    }
    var t203 int = source__5.vtable.get(source__5.data)
    var inline230 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t203)
    _goml_runtime_core_string_println(inline230)
    var t204 Number = Number{
        value: 13,
    }
    var same__6 dyn__Display = dyn__Display{
        data: t204,
        vtable: dyn__Display__vtable__Number(),
    }
    var t205 string = same__6.vtable.display(same__6.data)
    var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline227)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t223 string = _goml_runtime_core_int_to_string(self__67)
    return t223
}

func main() {
    main0()
}
