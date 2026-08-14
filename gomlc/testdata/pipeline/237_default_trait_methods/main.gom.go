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
    var t188 string = self__3.name
    return t188
}

func _goml_m_trait__impl_i_Describe_i_DefaultValue_i_describe(default_arg0 DefaultValue) string {
    var inline218 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(default_arg0)
    var inline219 string = "default:" + inline218
    return inline219
}

func main0() struct{} {
    var default_value__7 DefaultValue = DefaultValue{
        name: "value",
    }
    var t200 string
    var inline238 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(default_value__7)
    var inline239 string = "default:" + inline238
    t200 = inline239
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline235)
    var t201 string
    var inline233 string = "generic"
    t201 = inline233
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline230)
    var t203 string
    t203 = "override"
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline226)
    var t204 dyn__Describe = dyn__Describe{
        data: default_value__7,
        vtable: dyn__Describe__vtable__DefaultValue(),
    }
    var t205 string
    var inline224 string = t204.vtable.describe(t204.data)
    t205 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
