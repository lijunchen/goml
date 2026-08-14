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
    var t193 string = self__3.name
    return t193
}

func _goml_m_trait__impl_i_Describe_i_DefaultValue_i_describe(default_arg0 DefaultValue) string {
    var inline223 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(default_arg0)
    var inline224 string = "default:" + inline223
    return inline224
}

func main0() struct{} {
    var default_value__7 DefaultValue = DefaultValue{
        name: "value",
    }
    var t205 string
    var inline243 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(default_value__7)
    var inline244 string = "default:" + inline243
    t205 = inline244
    var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline240)
    var t206 string
    var inline238 string = "generic"
    t206 = inline238
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline235)
    var t208 string
    t208 = "override"
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline231)
    var t209 dyn__Describe = dyn__Describe{
        data: default_value__7,
        vtable: dyn__Describe__vtable__DefaultValue(),
    }
    var t210 string
    var inline229 string = t209.vtable.describe(t209.data)
    t210 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline226)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
