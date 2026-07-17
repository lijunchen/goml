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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
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
    var retv73 string
    var t74 int32 = self__0.value
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t74)
    var t76 string = "S(" + t75
    var t77 string = t76 + ")"
    retv73 = t77
    return retv73
}

func main0() struct{} {
    println__T_int32(1)
    println__T_bool(true)
    println__T_string("hi")
    println__T_unit(struct{}{})
    var t79 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(2)
    println__T_string(t79)
    var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
    println__T_string(t80)
    var s__1 S = S{
        value: 9,
    }
    println__T_S(s__1)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    println__T_dynToString(d__2)
    var r__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(5)
    _goml_m_println____T__Ref_l_int32_r_(r__3)
    print__T_string("no-newline")
    println__T_string("!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv82 string
    var t83 string = _goml_runtime_core_int32_to_string(self__5)
    retv82 = t83
    return retv82
}

func println__T_int32(value__1 int32) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t91 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t91)
    return struct{}{}
}

func println__T_unit(value__1 struct{}) struct{} {
    var t94 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__1)
    _goml_runtime_core_string_println(t94)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv97 string
    var t98 string = _goml_runtime_core_int32_to_string(self__41)
    retv97 = t98
    return retv97
}

func println__T_S(value__1 S) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func println__T_dynToString(value__1 dyn__ToString) struct{} {
    var t103 string = value__1.vtable.to_string(value__1.data)
    _goml_runtime_core_string_println(t103)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv106 *ref_int32_x
    var t107 *ref_int32_x = ref__Ref_5int32(value__204)
    retv106 = t107
    return retv106
}

func _goml_m_println____T__Ref_l_int32_r_(value__1 *ref_int32_x) struct{} {
    var t109 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int32_r__i_to__string(value__1)
    _goml_runtime_core_string_println(t109)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t112 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t112)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv115 string
    var t116 string = _goml_runtime_core_bool_to_string(self__36)
    retv115 = t116
    return retv115
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv118 string
    retv118 = self__37
    return retv118
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__35 struct{}) string {
    var retv120 string
    var t121 string = _goml_runtime_core_unit_to_string(self__35)
    retv120 = t121
    return retv120
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int32_r__i_to__string(self__94 *ref_int32_x) string {
    var retv123 string
    var v__95 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__94)
    var t124 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(v__95)
    var t125 string = "ref(" + t124
    var t126 string = t125 + ")"
    retv123 = t126
    return retv123
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv128 int32
    var t129 int32 = ref_get__Ref_5int32(self__205)
    retv128 = t129
    return retv128
}

func main() {
    main0()
}
