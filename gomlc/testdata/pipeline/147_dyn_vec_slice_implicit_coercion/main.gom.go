package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_Dyn_Display struct {
    items []dyn__Display
}

func vec_new__Vec_11Dyn_Display() *_goml_vec_Dyn_Display {
    return &_goml_vec_Dyn_Display{
        items: nil,
    }
}

func vec_push__Vec_11Dyn_Display(vec *_goml_vec_Dyn_Display, elem dyn__Display) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_int32_i_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Display_i_int32_i_show(self__0 int32) string {
    var inline209 string = _goml_runtime_core_int32_to_string(self__0)
    return inline209
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display
    var inline233 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__2 = inline233
    var first__3 int32 = 10
    var second__4 int32 = 20
    var t183 dyn__Display = dyn__Display{
        data: int32(first__3),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t183)
    var t184 dyn__Display = dyn__Display{
        data: int32(second__4),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t184)
    var s__5 []dyn__Display
    var inline225 int = 0
    var inline226 int = 2
    var inline227 []dyn__Display = v__2.items[inline225:inline226]
    s__5 = inline227
    var t185 dyn__Display = s__5[0]
    var t186 string
    var inline223 string = t185.vtable.show(t185.data)
    t186 = inline223
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline220)
    var t__6 []dyn__Display
    var inline216 int = 1
    var inline217 int = 2
    var inline218 []dyn__Display = s__5[inline216:inline217]
    t__6 = inline218
    var t187 dyn__Display = t__6[0]
    var t188 string
    var inline214 string = t187.vtable.show(t187.data)
    t188 = inline214
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline211)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
