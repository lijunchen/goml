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
    var t149 int32 = self__0.value
    var t150 string
    var inline215 string = _goml_runtime_core_int32_to_string(t149)
    t150 = inline215
    var t151 string = "S(" + t150
    var t152 string = t151 + ")"
    return t152
}

func main0() struct{} {
    var inline261 int = 1
    var inline262 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline261)
    _goml_runtime_core_string_println(inline262)
    var inline257 bool = true
    var inline258 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline257)
    _goml_runtime_core_string_println(inline258)
    var inline253 string = "hi"
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline253)
    _goml_runtime_core_string_println(inline254)
    var inline249 struct{} = struct{}{}
    var inline250 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline249)
    _goml_runtime_core_string_println(inline250)
    var t154 string
    var inline246 int = 2
    var inline247 string = _goml_runtime_core_int_to_string(inline246)
    t154 = inline247
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t154)
    _goml_runtime_core_string_println(inline243)
    var t155 string
    var inline240 int = 2
    var inline241 string = _goml_runtime_core_int_to_string(inline240)
    t155 = inline241
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t155)
    _goml_runtime_core_string_println(inline237)
    var s__1 S = S{
        value: 9,
    }
    var inline234 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    _goml_runtime_core_string_println(inline234)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    var inline231 string = d__2.vtable.to_string(d__2.data)
    _goml_runtime_core_string_println(inline231)
    var r__3 *ref_int_x
    var inline228 int = 5
    var inline229 *ref_int_x = ref__Ref_3int(inline228)
    r__3 = inline229
    var inline225 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(r__3)
    _goml_runtime_core_string_println(inline225)
    var inline221 string = "no-newline"
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline221)
    _goml_runtime_core_string_print(inline222)
    var inline217 string = "!"
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline217)
    _goml_runtime_core_string_println(inline218)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t173 string = _goml_runtime_core_int_to_string(self__69)
    return t173
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t194 string = _goml_runtime_core_bool_to_string(self__66)
    return t194
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__65 struct{}) string {
    var t199 string = _goml_runtime_core_unit_to_string(self__65)
    return t199
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__128 *ref_int_x) string {
    var v__129 int
    var inline285 int = ref_get__Ref_3int(self__128)
    v__129 = inline285
    var t202 string
    var inline283 string = _goml_runtime_core_int_to_string(v__129)
    t202 = inline283
    var t203 string = "ref(" + t202
    var t204 string = t203 + ")"
    return t204
}

func main() {
    main0()
}
