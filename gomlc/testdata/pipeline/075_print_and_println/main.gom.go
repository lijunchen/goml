package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

type S struct {
    value int32
}

type dyn__ToString_vtable struct {
    to_string func(any) string
}

type dyn__ToString struct {
    data any
    vtable *dyn__ToString_vtable
}

func dyn__ToString__wrap__S__to_string(self any) string {
    return _goml_m_trait__impl_i_ToString_i_S_i_to__string(self.(S))
}

func dyn__ToString__vtable__S() *dyn__ToString_vtable {
    return &dyn__ToString_vtable{
        to_string: dyn__ToString__wrap__S__to_string,
    }
}

func _goml_m_trait__impl_i_ToString_i_S_i_to__string(self__0 S) string {
    var retv167 string
    var t168 int32 = self__0.value
    var t169 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t168)
    var t170 string = "S(" + t169
    var t171 string = t170 + ")"
    retv167 = t171
    return retv167
}

func main0() struct{} {
    println__T_int(1)
    println__T_bool(true)
    println__T_string("hi")
    println__T_unit(struct{}{})
    var t173 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(2)
    println__T_string(t173)
    var t174 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
    println__T_string(t174)
    var s__1 S = S{
        value: 9,
    }
    println__T_S(s__1)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    println__T_dynToString(d__2)
    var r__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(5)
    _goml_m_println____T__Ref_l_int_r_(r__3)
    print__T_string("no-newline")
    println__T_string("!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv176 string
    var t177 string = _goml_runtime_core_int32_to_string(self__6)
    retv176 = t177
    return retv176
}

func println__T_int(value__1 int) struct{} {
    var t179 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t179)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t182 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t182)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t185 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t185)
    return struct{}{}
}

func println__T_unit(value__1 struct{}) struct{} {
    var t188 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__1)
    _goml_runtime_core_string_println(t188)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv191 string
    var t192 string = _goml_runtime_core_int_to_string(self__40)
    retv191 = t192
    return retv191
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv194 string
    var t195 string = _goml_runtime_core_int_to_string(self__5)
    retv194 = t195
    return retv194
}

func println__T_S(value__1 S) struct{} {
    var t197 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(value__1)
    _goml_runtime_core_string_println(t197)
    return struct{}{}
}

func println__T_dynToString(value__1 dyn__ToString) struct{} {
    var t200 string = value__1.vtable.to_string(value__1.data)
    _goml_runtime_core_string_println(t200)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv203 *ref_int_x
    var t204 *ref_int_x = ref__Ref_3int(value__207)
    retv203 = t204
    return retv203
}

func _goml_m_println____T__Ref_l_int_r_(value__1 *ref_int_x) struct{} {
    var t206 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(value__1)
    _goml_runtime_core_string_println(t206)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv212 string
    var t213 string = _goml_runtime_core_bool_to_string(self__37)
    retv212 = t213
    return retv212
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv215 string
    retv215 = self__38
    return retv215
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv217 string
    var t218 string = _goml_runtime_core_unit_to_string(self__36)
    retv217 = t218
    return retv217
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__99 *ref_int_x) string {
    var retv220 string
    var v__100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__99)
    var t221 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(v__100)
    var t222 string = "ref(" + t221
    var t223 string = t222 + ")"
    retv220 = t223
    return retv220
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv225 int
    var t226 int = ref_get__Ref_3int(self__208)
    retv225 = t226
    return retv225
}

func main() {
    main0()
}
