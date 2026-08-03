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
    var t183 string = self__3.name
    return t183
}

func _goml_m_trait__impl_i_Describe_i_DefaultValue_i_describe(default_arg0 DefaultValue) string {
    var inline216 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(default_arg0)
    var inline217 string = "default:" + inline216
    return inline217
}

func main0() struct{} {
    var default_value__7 DefaultValue = DefaultValue{
        name: "value",
    }
    var t195 string
    var inline237 string = _goml_m_trait__default_i_Describe_i_describe____Self__DefaultValue(default_value__7)
    t195 = inline237
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline234)
    var t196 string
    var inline231 string = "generic"
    var inline232 string = _goml_m_trait__default_i_Keep_i_keep____Self__DefaultValue____T__string(default_value__7, inline231)
    t196 = inline232
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline228)
    var t198 string
    t198 = "override"
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline224)
    var t199 dyn__Describe = dyn__Describe{
        data: default_value__7,
        vtable: dyn__Describe__vtable__DefaultValue(),
    }
    var t200 string
    var inline222 string = t199.vtable.describe(t199.data)
    t200 = inline222
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline219)
    return struct{}{}
}

func _goml_m_trait__default_i_Describe_i_describe____Self__DefaultValue(self__0 DefaultValue) string {
    var t203 string
    var inline239 string = self__0.name
    t203 = inline239
    var t204 string = "default:" + t203
    return t204
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
