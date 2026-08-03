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
    var t190 int32 = self__0.value
    var t191 string
    var inline256 string = _goml_runtime_core_int32_to_string(t190)
    t191 = inline256
    var t192 string = "S(" + t191
    var t193 string = t192 + ")"
    return t193
}

func main0() struct{} {
    var inline302 int = 1
    var inline303 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline302)
    _goml_runtime_core_string_println(inline303)
    var inline298 bool = true
    var inline299 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline298)
    _goml_runtime_core_string_println(inline299)
    var inline294 string = "hi"
    var inline295 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline294)
    _goml_runtime_core_string_println(inline295)
    var inline290 struct{} = struct{}{}
    var inline291 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline290)
    _goml_runtime_core_string_println(inline291)
    var t195 string
    var inline287 int = 2
    var inline288 string = _goml_runtime_core_int_to_string(inline287)
    t195 = inline288
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline284)
    var t196 string
    var inline281 int = 2
    var inline282 string = _goml_runtime_core_int_to_string(inline281)
    t196 = inline282
    var inline278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline278)
    var s__1 S = S{
        value: 9,
    }
    var inline275 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    _goml_runtime_core_string_println(inline275)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    var inline272 string = d__2.vtable.to_string(d__2.data)
    _goml_runtime_core_string_println(inline272)
    var r__3 *ref_int_x
    var inline269 int = 5
    var inline270 *ref_int_x = ref__Ref_3int(inline269)
    r__3 = inline270
    var inline266 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(r__3)
    _goml_runtime_core_string_println(inline266)
    var inline262 string = "no-newline"
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline262)
    _goml_runtime_core_string_print(inline263)
    var inline258 string = "!"
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline258)
    _goml_runtime_core_string_println(inline259)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t214 string = _goml_runtime_core_int_to_string(self__69)
    return t214
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t235 string = _goml_runtime_core_bool_to_string(self__66)
    return t235
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__65 struct{}) string {
    var t240 string = _goml_runtime_core_unit_to_string(self__65)
    return t240
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__128 *ref_int_x) string {
    var v__129 int
    var inline326 int = ref_get__Ref_3int(self__128)
    v__129 = inline326
    var t243 string
    var inline324 string = _goml_runtime_core_int_to_string(v__129)
    t243 = inline324
    var t244 string = "ref(" + t243
    var t245 string = t244 + ")"
    return t245
}

func main() {
    main0()
}
