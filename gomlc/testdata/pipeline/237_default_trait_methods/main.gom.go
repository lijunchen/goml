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
    var t164 string = _goml_m_trait__default_i_Describe_i_describe____Self__DefaultValue(default_arg0)
    return t164
}

func _goml_m_trait__impl_i_Describe_i_OverrideValue_i_describe(self__5 OverrideValue) string {
    return "override"
}

func describe_dyn(value__6 dyn__Describe) string {
    var t171 string = value__6.vtable.describe(value__6.data)
    return t171
}

func main0() struct{} {
    var default_value__7 DefaultValue = DefaultValue{
        name: "value",
    }
    var t173 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_describe(default_value__7)
    _goml_runtime_core_string_println(t173)
    var t174 string = _goml_m_trait__impl_i_Keep_i_DefaultValue_i_keep____mono1(default_value__7, "generic")
    _goml_runtime_core_string_println(t174)
    var t175 OverrideValue = OverrideValue{}
    var t176 string = _goml_m_trait__impl_i_Describe_i_OverrideValue_i_describe(t175)
    _goml_runtime_core_string_println(t176)
    var t177 dyn__Describe = dyn__Describe{
        data: default_value__7,
        vtable: dyn__Describe__vtable__DefaultValue(),
    }
    var t178 string = describe_dyn(t177)
    _goml_runtime_core_string_println(t178)
    return struct{}{}
}

func _goml_m_trait__default_i_Describe_i_describe____Self__DefaultValue(self__0 DefaultValue) string {
    var t181 string = _goml_m_trait__impl_i_Describe_i_DefaultValue_i_name(self__0)
    var t182 string = "default:" + t181
    return t182
}

func _goml_m_trait__impl_i_Keep_i_DefaultValue_i_keep____mono1(default_arg0 DefaultValue, default_arg1 string) string {
    var t185 string = _goml_m_trait__default_i_Keep_i_keep____Self__DefaultValue____T__string(default_arg0, default_arg1)
    return t185
}

func _goml_m_trait__default_i_Keep_i_keep____Self__DefaultValue____T__string(self__1 DefaultValue, value__2 string) string {
    return value__2
}

func main() {
    main0()
}
