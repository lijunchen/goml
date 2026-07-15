package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Person struct {
    name string
}

func main0() struct{} {
    var name__0 string = "Alice"
    var p__1 Person = Person{
        name: name__0,
    }
    var t24 string = p__1.name
    println__T_string(t24)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t26 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t26)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv29 string
    retv29 = self__9
    return retv29
}

func main() {
    main0()
}
