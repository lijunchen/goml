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
    var x58 bool = a__0._0
    var x59 bool = a__0._1
    switch x58 {
    case true:
        var b__1 bool = x59
        print__T_bool(b__1)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var c__2 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: true,
    }
    var x61 bool = c__2._0
    var x62 bool = c__2._1
    switch x61 {
    case true:
        var d__3 bool = x62
        print__T_bool(d__3)
    case false:
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t69)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv72 string
    var t73 string = _goml_runtime_core_bool_to_string(self__33)
    retv72 = t73
    return retv72
}

func main() {
    main0()
}
