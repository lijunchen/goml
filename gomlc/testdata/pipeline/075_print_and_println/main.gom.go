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
    var t185 int32 = self__0.value
    var t186 string
    var inline251 string = _goml_runtime_core_int32_to_string(t185)
    t186 = inline251
    var t187 string = "S(" + t186
    var t188 string = t187 + ")"
    return t188
}

func main0() struct{} {
    var inline297 int = 1
    var inline298 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline297)
    _goml_runtime_core_string_println(inline298)
    var inline293 bool = true
    var inline294 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline293)
    _goml_runtime_core_string_println(inline294)
    var inline289 string = "hi"
    var inline290 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline289)
    _goml_runtime_core_string_println(inline290)
    var inline285 struct{} = struct{}{}
    var inline286 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline285)
    _goml_runtime_core_string_println(inline286)
    var t190 string
    var inline282 int = 2
    var inline283 string = _goml_runtime_core_int_to_string(inline282)
    t190 = inline283
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline279)
    var t191 string
    var inline276 int = 2
    var inline277 string = _goml_runtime_core_int_to_string(inline276)
    t191 = inline277
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline273)
    var s__1 S = S{
        value: 9,
    }
    var inline270 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    _goml_runtime_core_string_println(inline270)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    var inline267 string = d__2.vtable.to_string(d__2.data)
    _goml_runtime_core_string_println(inline267)
    var r__3 *ref_int_x
    var inline264 int = 5
    var inline265 *ref_int_x = ref__Ref_3int(inline264)
    r__3 = inline265
    var inline261 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(r__3)
    _goml_runtime_core_string_println(inline261)
    var inline257 string = "no-newline"
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline257)
    _goml_runtime_core_string_print(inline258)
    var inline253 string = "!"
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline253)
    _goml_runtime_core_string_println(inline254)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t209 string = _goml_runtime_core_int_to_string(self__67)
    return t209
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t230 string = _goml_runtime_core_bool_to_string(self__64)
    return t230
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__63 struct{}) string {
    var t235 string = _goml_runtime_core_unit_to_string(self__63)
    return t235
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__166 *ref_int_x) string {
    var v__167 int
    var inline321 int = ref_get__Ref_3int(self__166)
    v__167 = inline321
    var t238 string
    var inline319 string = _goml_runtime_core_int_to_string(v__167)
    t238 = inline319
    var t239 string = "ref(" + t238
    var t240 string = t239 + ")"
    return t240
}

func main() {
    main0()
}
