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
    var t187 int = self__0.value
    var inline220 string = _goml_runtime_core_int_to_string(t187)
    return inline220
}

func _goml_m_trait__impl_i_Source_i_Number_i_get(self__1 Number) int {
    var t191 int = self__1.value
    return t191
}

func main0() struct{} {
    var t193 Number = Number{
        value: 42,
    }
    var display__3 dyn__Display = dyn__Display{
        data: t193,
        vtable: dyn__Display__vtable__Number(),
    }
    var t194 string = display__3.vtable.display(display__3.data)
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline233)
    var t195 Number = Number{
        value: 7,
    }
    var erased__4 dyn__Display
    var inline231 dyn__Display = dyn__Display{
        data: t195,
        vtable: dyn__Display__vtable__Number(),
    }
    erased__4 = inline231
    var t196 string = erased__4.vtable.display(erased__4.data)
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline228)
    var t197 Number = Number{
        value: 11,
    }
    var source__5 dyn__Source = dyn__Source{
        data: t197,
        vtable: dyn__Source__vtable__Number(),
    }
    var t198 int = source__5.vtable.get(source__5.data)
    var inline225 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t198)
    _goml_runtime_core_string_println(inline225)
    var t199 Number = Number{
        value: 13,
    }
    var same__6 dyn__Display = dyn__Display{
        data: t199,
        vtable: dyn__Display__vtable__Number(),
    }
    var t200 string = same__6.vtable.display(same__6.data)
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline222)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t218 string = _goml_runtime_core_int_to_string(self__67)
    return t218
}

func main() {
    main0()
}
