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
    var inline192 string = _goml_runtime_core_int32_to_string(self__0)
    return inline192
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display
    var inline216 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__2 = inline216
    var first__3 int32 = 10
    var second__4 int32 = 20
    var t166 dyn__Display = dyn__Display{
        data: int32(first__3),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t166)
    var t167 dyn__Display = dyn__Display{
        data: int32(second__4),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t167)
    var s__5 []dyn__Display
    var inline208 int = 0
    var inline209 int = 2
    var inline210 []dyn__Display = v__2.items[inline208:inline209]
    s__5 = inline210
    var t168 dyn__Display = s__5[0]
    var t169 string
    var inline206 string = t168.vtable.show(t168.data)
    t169 = inline206
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
    _goml_runtime_core_string_println(inline203)
    var t__6 []dyn__Display
    var inline199 int = 1
    var inline200 int = 2
    var inline201 []dyn__Display = s__5[inline199:inline200]
    t__6 = inline201
    var t170 dyn__Display = t__6[0]
    var t171 string
    var inline197 string = t170.vtable.show(t170.data)
    t171 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
    _goml_runtime_core_string_println(inline194)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
