package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type DefaultValue struct {
    name string
}

type OverrideValue struct {}

type Ordering int32

type dyn__Describe_vtable struct {
    name func(any) string
    describe func(any) string
}

type dyn__Describe struct {
    data any
    vtable *dyn__Describe_vtable
}

func dyn__Describe__wrap__DefaultValue__name(self any) string {
    return _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(self.(DefaultValue))
}

func dyn__Describe__wrap__DefaultValue__describe(self any) string {
    return _goml_m_trait__impl_i_Describe_i_DefaultValue_i_describe(self.(DefaultValue))
}

func dyn__Describe__vtable__DefaultValue() *dyn__Describe_vtable {
    return &dyn__Describe_vtable{
        name: dyn__Describe__wrap__DefaultValue__name,
        describe: dyn__Describe__wrap__DefaultValue__describe,
    }
}

func _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(self__3 DefaultValue) string {
    var t417 string = self__3.name
    return t417
}

func _goml_m_trait__impl_i_Describe_i_DefaultValue_i_describe(default_arg0 DefaultValue) string {
    var inline447 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(default_arg0)
    var inline448 string = "default:" + inline447
    return inline448
}

func main0() struct{} {
    var default_value__7 DefaultValue = DefaultValue{
        name: "value",
    }
    var t429 string
    var inline467 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(default_value__7)
    var inline468 string = "default:" + inline467
    t429 = inline468
    var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline464)
    var t430 string
    var inline462 string = "generic"
    t430 = inline462
    var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline459)
    var t432 string
    t432 = "override"
    var inline455 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline455)
    var t433 dyn__Describe = dyn__Describe{
        data: default_value__7,
        vtable: dyn__Describe__vtable__DefaultValue(),
    }
    var t434 string
    var inline453 string = t433.vtable.describe(t433.data)
    t434 = inline453
    var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline450)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
