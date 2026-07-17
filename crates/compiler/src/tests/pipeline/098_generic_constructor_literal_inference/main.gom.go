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
        var x61 uint8 = x__0.(Some)._0
        var v__1 uint8 = x61
        var t65 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(v__1)
        println__T_string(t65)
    case None:
        println__T_string("none")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t69)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__43 uint8) string {
    var retv72 string
    var t73 string = _goml_runtime_core_uint8_to_string(self__43)
    retv72 = t73
    return retv72
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv75 string
    retv75 = self__37
    return retv75
}

func main() {
    main0()
}
