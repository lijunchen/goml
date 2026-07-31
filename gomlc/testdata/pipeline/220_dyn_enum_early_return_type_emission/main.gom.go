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

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func build() int32 {
    var retv157 int32
    retv157 = 9
    return retv157
}

func main0() struct{} {
    var t159 int32 = build()
    var t160 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t159)
    println__T_string(t160)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv163 string
    var t164 string = _goml_runtime_core_int32_to_string(self__6)
    retv163 = t164
    return retv163
}

func println__T_string(value__1 string) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv169 string
    retv169 = self__38
    return retv169
}

func main() {
    main0()
}
