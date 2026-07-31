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

type Wrap__int32 struct {
    value int32
}

type dyn__Show_vtable struct {
    show func(any) string
}

type dyn__Show struct {
    data any
    vtable *dyn__Show_vtable
}

func dyn__Show__wrap__Wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Show_i_Wrap____int32_i_show(self.(Wrap__int32))
}

func dyn__Show__vtable__Wrap__int32() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__Wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Show_i_Wrap____int32_i_show(self__0 Wrap__int32) string {
    var retv153 string
    var t154 int32 = self__0.value
    var t155 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t154)
    retv153 = t155
    return retv153
}

func render(value__2 dyn__Show) string {
    var retv157 string
    var t158 string = value__2.vtable.show(value__2.data)
    retv157 = t158
    return retv157
}

func main0() struct{} {
    var value__3 int32 = 42
    var t160 Wrap__int32 = make_wrap__T_int32(value__3)
    var t161 dyn__Show = dyn__Show{
        data: t160,
        vtable: dyn__Show__vtable__Wrap__int32(),
    }
    var t162 string = render(t161)
    println__T_string(t162)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv165 string
    var t166 string = _goml_runtime_core_int32_to_string(self__6)
    retv165 = t166
    return retv165
}

func println__T_string(value__1 string) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func make_wrap__T_int32(value__1 int32) Wrap__int32 {
    var retv171 Wrap__int32
    var t172 Wrap__int32 = Wrap__int32{
        value: value__1,
    }
    retv171 = t172
    return retv171
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv174 string
    retv174 = self__38
    return retv174
}

func main() {
    main0()
}
