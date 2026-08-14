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
    var t413 int = self__0.value
    var inline446 string = _goml_runtime_core_int_to_string(t413)
    return inline446
}

func _goml_m_trait__impl_i_Source_i_Number_i_get(self__1 Number) int {
    var t417 int = self__1.value
    return t417
}

func main0() struct{} {
    var t419 Number = Number{
        value: 42,
    }
    var display__3 dyn__Display = dyn__Display{
        data: t419,
        vtable: dyn__Display__vtable__Number(),
    }
    var t420 string = display__3.vtable.display(display__3.data)
    var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline459)
    var t421 Number = Number{
        value: 7,
    }
    var erased__4 dyn__Display
    var inline457 dyn__Display = dyn__Display{
        data: t421,
        vtable: dyn__Display__vtable__Number(),
    }
    erased__4 = inline457
    var t422 string = erased__4.vtable.display(erased__4.data)
    var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline454)
    var t423 Number = Number{
        value: 11,
    }
    var source__5 dyn__Source = dyn__Source{
        data: t423,
        vtable: dyn__Source__vtable__Number(),
    }
    var t424 int = source__5.vtable.get(source__5.data)
    var inline451 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t424)
    _goml_runtime_core_string_println(inline451)
    var t425 Number = Number{
        value: 13,
    }
    var same__6 dyn__Display = dyn__Display{
        data: t425,
        vtable: dyn__Display__vtable__Number(),
    }
    var t426 string = same__6.vtable.display(same__6.data)
    var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t426)
    _goml_runtime_core_string_println(inline448)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t444 string = _goml_runtime_core_int_to_string(self__151)
    return t444
}

func main() {
    main0()
}
