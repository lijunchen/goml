package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

func main0() struct{} {
    var x155 bool = true
    var x156 bool = false
    switch x155 {
    case true:
        print__T_bool(x156)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var x158 bool = true
    var x159 bool = true
    switch x158 {
    case true:
        print__T_bool(x159)
        return struct{}{}
    case false:
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func print__T_bool(value__0 bool) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t166)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t170 string = _goml_runtime_core_bool_to_string(self__37)
    return t170
}

func main() {
    main0()
}
