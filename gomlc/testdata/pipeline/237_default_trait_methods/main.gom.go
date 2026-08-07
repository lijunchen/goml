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
    var t178 string = self__3.name
    return t178
}

func _goml_m_trait__impl_i_Describe_i_DefaultValue_i_describe(default_arg0 DefaultValue) string {
    var inline211 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(default_arg0)
    var inline212 string = "default:" + inline211
    return inline212
}

func main0() struct{} {
    var default_value__7 DefaultValue = DefaultValue{
        name: "value",
    }
    var t190 string
    var inline232 string = _goml_m_trait__default_i_Describe_i_describe____Self__DefaultValue(default_value__7)
    t190 = inline232
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline229)
    var t191 string
    var inline226 string = "generic"
    var inline227 string = _goml_m_trait__default_i_Keep_i_keep____Self__DefaultValue____T__string(default_value__7, inline226)
    t191 = inline227
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline223)
    var t193 string
    t193 = "override"
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline219)
    var t194 dyn__Describe = dyn__Describe{
        data: default_value__7,
        vtable: dyn__Describe__vtable__DefaultValue(),
    }
    var t195 string
    var inline217 string = t194.vtable.describe(t194.data)
    t195 = inline217
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline214)
    return struct{}{}
}

func _goml_m_trait__default_i_Describe_i_describe____Self__DefaultValue(self__0 DefaultValue) string {
    var t198 string
    var inline234 string = self__0.name
    t198 = inline234
    var t199 string = "default:" + t198
    return t199
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__default_i_Keep_i_keep____Self__DefaultValue____T__string(self__1 DefaultValue, value__2 string) string {
    return value__2
}

func main() {
    main0()
}
