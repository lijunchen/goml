package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type T interface {
    isT()
}

type A struct {}

func (_ A) isT() {}

type B struct {
    _0 bool
    _1 struct{}
}

func (_ B) isT() {}

func main0() struct{} {
    var x187 bool = true
    switch x187 {
    case true:
        var t195 string
        var inline211 int = 2
        var inline212 string = _goml_runtime_core_int_to_string(inline211)
        t195 = inline212
        var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
        _goml_runtime_core_string_println(inline208)
        return struct{}{}
    case false:
        var t197 string
        var inline217 int = 3
        var inline218 string = _goml_runtime_core_int_to_string(inline217)
        t197 = inline218
        var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
        _goml_runtime_core_string_println(inline214)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
