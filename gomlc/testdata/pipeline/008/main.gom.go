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
    var x177 bool = true
    switch x177 {
    case true:
        var t185 string
        var inline201 int = 2
        var inline202 string = _goml_runtime_core_int_to_string(inline201)
        t185 = inline202
        var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
        _goml_runtime_core_string_println(inline198)
        return struct{}{}
    case false:
        var t187 string
        var inline207 int = 3
        var inline208 string = _goml_runtime_core_int_to_string(inline207)
        t187 = inline208
        var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
        _goml_runtime_core_string_println(inline204)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
