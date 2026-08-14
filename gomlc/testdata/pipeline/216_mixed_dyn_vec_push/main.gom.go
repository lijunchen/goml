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

type _goml_vec_Dyn_Show struct {
    items []dyn__Show
}

func vec_new__Vec_8Dyn_Show() *_goml_vec_Dyn_Show {
    return &_goml_vec_Dyn_Show{
        items: nil,
    }
}

func vec_push__Vec_8Dyn_Show(vec *_goml_vec_Dyn_Show, elem dyn__Show) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_8Dyn_Show(vec *_goml_vec_Dyn_Show, index int) dyn__Show {
    return vec.items[index]
}

type Wrap struct {
    value string
}

type dyn__Show_vtable struct {
    show func(any) string
}

type dyn__Show struct {
    data any
    vtable *dyn__Show_vtable
}

func dyn__Show__wrap__Wrap__show(self any) string {
    return _goml_m_trait__impl_i_Show_i_Wrap_i_show(self.(Wrap))
}

func dyn__Show__vtable__Wrap() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__Wrap__show,
    }
}

func dyn__Show__wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Show_i_int32_i_show(self.(int32))
}

func dyn__Show__vtable__int32() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Show_i_int32_i_show(self__0 int32) string {
    var inline219 string = _goml_runtime_core_int32_to_string(self__0)
    return inline219
}

func _goml_m_trait__impl_i_Show_i_Wrap_i_show(self__1 Wrap) string {
    var t195 string = self__1.value
    return t195
}

func main0() struct{} {
    var values__2 *_goml_vec_Dyn_Show
    var inline231 *_goml_vec_Dyn_Show = vec_new__Vec_8Dyn_Show()
    values__2 = inline231
    var value__3 int32 = 10
    var t197 dyn__Show = dyn__Show{
        data: int32(value__3),
        vtable: dyn__Show__vtable__int32(),
    }
    vec_push__Vec_8Dyn_Show(values__2, t197)
    var t198 Wrap = Wrap{
        value: "ok",
    }
    var t199 dyn__Show = dyn__Show{
        data: t198,
        vtable: dyn__Show__vtable__Wrap(),
    }
    vec_push__Vec_8Dyn_Show(values__2, t199)
    var t200 dyn__Show = vec_get__Vec_8Dyn_Show(values__2, 0)
    var t201 string = t200.vtable.show(t200.data)
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline224)
    var t202 dyn__Show = vec_get__Vec_8Dyn_Show(values__2, 1)
    var t203 string = t202.vtable.show(t202.data)
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
