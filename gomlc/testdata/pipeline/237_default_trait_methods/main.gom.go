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
    var t161 string = self__3.name
    return t161
}

func _goml_m_trait__impl_i_Describe_i_DefaultValue_i_describe(default_arg0 DefaultValue) string {
    var inline194 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(default_arg0)
    var inline195 string = "default:" + inline194
    return inline195
}

func main0() struct{} {
    var default_value__7 DefaultValue = DefaultValue{
        name: "value",
    }
    var t173 string
    var inline215 string = _goml_m_trait__default_i_Describe_i_describe____Self__DefaultValue(default_value__7)
    t173 = inline215
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t173)
    _goml_runtime_core_string_println(inline212)
    var t174 string
    var inline209 string = "generic"
    var inline210 string = _goml_m_trait__default_i_Keep_i_keep____Self__DefaultValue____T__string(default_value__7, inline209)
    t174 = inline210
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline206)
    var t176 string
    t176 = "override"
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
    _goml_runtime_core_string_println(inline202)
    var t177 dyn__Describe = dyn__Describe{
        data: default_value__7,
        vtable: dyn__Describe__vtable__DefaultValue(),
    }
    var t178 string
    var inline200 string = t177.vtable.describe(t177.data)
    t178 = inline200
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
    _goml_runtime_core_string_println(inline197)
    return struct{}{}
}

func _goml_m_trait__default_i_Describe_i_describe____Self__DefaultValue(self__0 DefaultValue) string {
    var t181 string
    var inline217 string = self__0.name
    t181 = inline217
    var t182 string = "default:" + t181
    return t182
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__default_i_Keep_i_keep____Self__DefaultValue____T__string(self__1 DefaultValue, value__2 string) string {
    return value__2
}

func main() {
    main0()
}
