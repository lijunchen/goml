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

type Boxed interface {
    isBoxed()
}

type One struct {
    _0 dyn__Display
}

func (_ One) isBoxed() {}

type Pair struct {
    _0 dyn__Display
    _1 dyn__Display
}

func (_ Pair) isBoxed() {}

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
    var inline170 string = _goml_runtime_core_int32_to_string(self__0)
    return inline170
}

func main0() struct{} {
    var one__5 int32 = 42
    var left__6 int32 = 7
    var right__7 int32 = 9
    var t154 dyn__Display = dyn__Display{
        data: int32(one__5),
        vtable: dyn__Display__vtable__int32(),
    }
    var t156 string
    var inline192 string = t154.vtable.show(t154.data)
    t156 = inline192
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t156)
    _goml_runtime_core_string_println(inline187)
    var t157 dyn__Display = dyn__Display{
        data: int32(left__6),
        vtable: dyn__Display__vtable__int32(),
    }
    var t158 dyn__Display = dyn__Display{
        data: int32(right__7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t160 string
    var inline182 string = t157.vtable.show(t157.data)
    var inline183 string = inline182 + "-"
    var inline184 string = t158.vtable.show(t158.data)
    var inline185 string = inline183 + inline184
    t160 = inline185
    var inline172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline172)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
