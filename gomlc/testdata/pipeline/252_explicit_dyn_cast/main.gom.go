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

type Ordering int32

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
    var t416 int = self__0.value
    var inline449 string = _goml_runtime_core_int_to_string(t416)
    return inline449
}

func _goml_m_trait__impl_i_Source_i_Number_i_get(self__1 Number) int {
    var t420 int = self__1.value
    return t420
}

func main0() struct{} {
    var t422 Number = Number{
        value: 42,
    }
    var display__3 dyn__Display = dyn__Display{
        data: t422,
        vtable: dyn__Display__vtable__Number(),
    }
    var t423 string = display__3.vtable.display(display__3.data)
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline462)
    var t424 Number = Number{
        value: 7,
    }
    var erased__4 dyn__Display
    var inline460 dyn__Display = dyn__Display{
        data: t424,
        vtable: dyn__Display__vtable__Number(),
    }
    erased__4 = inline460
    var t425 string = erased__4.vtable.display(erased__4.data)
    var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline457)
    var t426 Number = Number{
        value: 11,
    }
    var source__5 dyn__Source = dyn__Source{
        data: t426,
        vtable: dyn__Source__vtable__Number(),
    }
    var t427 int = source__5.vtable.get(source__5.data)
    var inline454 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t427)
    _goml_runtime_core_string_println(inline454)
    var t428 Number = Number{
        value: 13,
    }
    var same__6 dyn__Display = dyn__Display{
        data: t428,
        vtable: dyn__Display__vtable__Number(),
    }
    var t429 string = same__6.vtable.display(same__6.data)
    var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline451)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t447 string = _goml_runtime_core_int_to_string(self__151)
    return t447
}

func main() {
    main0()
}
