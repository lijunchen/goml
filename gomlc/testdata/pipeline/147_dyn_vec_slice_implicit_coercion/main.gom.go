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
    var inline173 string = _goml_runtime_core_int32_to_string(self__0)
    return inline173
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display
    var inline197 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__2 = inline197
    var first__3 int32 = 10
    var second__4 int32 = 20
    var t147 dyn__Display = dyn__Display{
        data: int32(first__3),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t147)
    var t148 dyn__Display = dyn__Display{
        data: int32(second__4),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t148)
    var s__5 []dyn__Display
    var inline189 int = 0
    var inline190 int = 2
    var inline191 []dyn__Display = v__2.items[inline189:inline190]
    s__5 = inline191
    var t149 dyn__Display = s__5[0]
    var t150 string
    var inline187 string = t149.vtable.show(t149.data)
    t150 = inline187
    var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t150)
    _goml_runtime_core_string_println(inline184)
    var t__6 []dyn__Display
    var inline180 int = 1
    var inline181 int = 2
    var inline182 []dyn__Display = s__5[inline180:inline181]
    t__6 = inline182
    var t151 dyn__Display = t__6[0]
    var t152 string
    var inline178 string = t151.vtable.show(t151.data)
    t152 = inline178
    var inline175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t152)
    _goml_runtime_core_string_println(inline175)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
