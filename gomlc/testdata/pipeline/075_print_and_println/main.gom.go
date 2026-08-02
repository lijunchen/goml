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
    var t168 int32 = self__0.value
    var t169 string
    var inline234 string = _goml_runtime_core_int32_to_string(t168)
    t169 = inline234
    var t170 string = "S(" + t169
    var t171 string = t170 + ")"
    return t171
}

func main0() struct{} {
    var inline280 int = 1
    var inline281 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline280)
    _goml_runtime_core_string_println(inline281)
    var inline276 bool = true
    var inline277 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline276)
    _goml_runtime_core_string_println(inline277)
    var inline272 string = "hi"
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline272)
    _goml_runtime_core_string_println(inline273)
    var inline268 struct{} = struct{}{}
    var inline269 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline268)
    _goml_runtime_core_string_println(inline269)
    var t173 string
    var inline265 int = 2
    var inline266 string = _goml_runtime_core_int_to_string(inline265)
    t173 = inline266
    var inline262 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t173)
    _goml_runtime_core_string_println(inline262)
    var t174 string
    var inline259 int = 2
    var inline260 string = _goml_runtime_core_int_to_string(inline259)
    t174 = inline260
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline256)
    var s__1 S = S{
        value: 9,
    }
    var inline253 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    _goml_runtime_core_string_println(inline253)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    var inline250 string = d__2.vtable.to_string(d__2.data)
    _goml_runtime_core_string_println(inline250)
    var r__3 *ref_int_x
    var inline247 int = 5
    var inline248 *ref_int_x = ref__Ref_3int(inline247)
    r__3 = inline248
    var inline244 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(r__3)
    _goml_runtime_core_string_println(inline244)
    var inline240 string = "no-newline"
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline240)
    _goml_runtime_core_string_print(inline241)
    var inline236 string = "!"
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline236)
    _goml_runtime_core_string_println(inline237)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t192 string = _goml_runtime_core_int_to_string(self__40)
    return t192
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t213 string = _goml_runtime_core_bool_to_string(self__37)
    return t213
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var t218 string = _goml_runtime_core_unit_to_string(self__36)
    return t218
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__99 *ref_int_x) string {
    var v__100 int
    var inline304 int = ref_get__Ref_3int(self__99)
    v__100 = inline304
    var t221 string
    var inline302 string = _goml_runtime_core_int_to_string(v__100)
    t221 = inline302
    var t222 string = "ref(" + t221
    var t223 string = t222 + ")"
    return t223
}

func main() {
    main0()
}
