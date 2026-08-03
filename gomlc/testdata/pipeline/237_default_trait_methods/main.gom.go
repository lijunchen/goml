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
    var t142 string = self__3.name
    return t142
}

func _goml_m_trait__impl_i_Describe_i_DefaultValue_i_describe(default_arg0 DefaultValue) string {
    var inline175 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(default_arg0)
    var inline176 string = "default:" + inline175
    return inline176
}

func main0() struct{} {
    var default_value__7 DefaultValue = DefaultValue{
        name: "value",
    }
    var t154 string
    var inline196 string = _goml_m_trait__default_i_Describe_i_describe____Self__DefaultValue(default_value__7)
    t154 = inline196
    var inline193 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t154)
    _goml_runtime_core_string_println(inline193)
    var t155 string
    var inline190 string = "generic"
    var inline191 string = _goml_m_trait__default_i_Keep_i_keep____Self__DefaultValue____T__string(default_value__7, inline190)
    t155 = inline191
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t155)
    _goml_runtime_core_string_println(inline187)
    var t157 string
    t157 = "override"
    var inline183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t157)
    _goml_runtime_core_string_println(inline183)
    var t158 dyn__Describe = dyn__Describe{
        data: default_value__7,
        vtable: dyn__Describe__vtable__DefaultValue(),
    }
    var t159 string
    var inline181 string = t158.vtable.describe(t158.data)
    t159 = inline181
    var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
    _goml_runtime_core_string_println(inline178)
    return struct{}{}
}

func _goml_m_trait__default_i_Describe_i_describe____Self__DefaultValue(self__0 DefaultValue) string {
    var t162 string
    var inline198 string = self__0.name
    t162 = inline198
    var t163 string = "default:" + t162
    return t163
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
