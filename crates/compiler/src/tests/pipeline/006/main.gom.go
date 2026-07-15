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
    var a__0 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var x22 bool = a__0._0
    var x23 bool = a__0._1
    switch x22 {
    case true:
        var b__1 bool = x23
        print__T_bool(b__1)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var c__2 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: true,
    }
    var x25 bool = c__2._0
    var x26 bool = c__2._1
    switch x25 {
    case true:
        var d__3 bool = x26
        print__T_bool(d__3)
    case false:
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t33)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv36 string
    var t37 string = _goml_runtime_core_bool_to_string(self__8)
    retv36 = t37
    return retv36
}

func main() {
    main0()
}
