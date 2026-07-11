package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Option__uint8 interface {
    isOption__uint8()
}

type Some struct {
    _0 uint8
}

func (_ Some) isOption__uint8() {}

type None struct {}

func (_ None) isOption__uint8() {}

func main0() struct{} {
    var x__0 Option__uint8 = Some{
        _0: 42,
    }
    switch x__0.(type) {
    case Some:
        var x4 uint8 = x__0.(Some)._0
        var v__1 uint8 = x4
        var t8 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(v__1)
        println__T_string(t8)
    case None:
        println__T_string("none")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t12 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t12)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__15 uint8) string {
    var retv15 string
    var t16 string = _goml_runtime_core_uint8_to_string(self__15)
    retv15 = t16
    return retv15
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv18 string
    retv18 = self__9
    return retv18
}

func main() {
    main0()
}
