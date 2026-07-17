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
    var retv70 string
    var t71 int32 = self__0.value
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t71)
    var t73 string = "S(" + t72
    var t74 string = t73 + ")"
    retv70 = t74
    return retv70
}

func main0() struct{} {
    println__T_int32(1)
    println__T_bool(true)
    println__T_string("hi")
    println__T_unit(struct{}{})
    var t76 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(2)
    println__T_string(t76)
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
    println__T_string(t77)
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

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__2)
    retv79 = t80
    return retv79
}

func println__T_int32(value__1 int32) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func println__T_unit(value__1 struct{}) struct{} {
    var t91 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__1)
    _goml_runtime_core_string_println(t91)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int32_to_string(self__38)
    retv94 = t95
    return retv94
}

func println__T_S(value__1 S) struct{} {
    var t97 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(value__1)
    _goml_runtime_core_string_println(t97)
    return struct{}{}
}

func println__T_dynToString(value__1 dyn__ToString) struct{} {
    var t100 string = value__1.vtable.to_string(value__1.data)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv103 *ref_int32_x
    var t104 *ref_int32_x = ref__Ref_5int32(value__200)
    retv103 = t104
    return retv103
}

func _goml_m_println____T__Ref_l_int32_r_(value__1 *ref_int32_x) struct{} {
    var t106 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int32_r__i_to__string(value__1)
    _goml_runtime_core_string_println(t106)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t109 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t109)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv112 string
    var t113 string = _goml_runtime_core_bool_to_string(self__33)
    retv112 = t113
    return retv112
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv115 string
    retv115 = self__34
    return retv115
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__32 struct{}) string {
    var retv117 string
    var t118 string = _goml_runtime_core_unit_to_string(self__32)
    retv117 = t118
    return retv117
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int32_r__i_to__string(self__91 *ref_int32_x) string {
    var retv120 string
    var v__92 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__91)
    var t121 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(v__92)
    var t122 string = "ref(" + t121
    var t123 string = t122 + ")"
    retv120 = t123
    return retv120
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv125 int32
    var t126 int32 = ref_get__Ref_5int32(self__201)
    retv125 = t126
    return retv125
}

func main() {
    main0()
}
