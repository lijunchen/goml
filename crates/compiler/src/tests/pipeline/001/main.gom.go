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

type Tuple3_4bool_4bool_18Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
    _2 Tuple2_4bool_4bool
}

func main0() struct{} {
    var t12 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var a__0 Tuple3_4bool_4bool_18Tuple2_4bool_4bool = Tuple3_4bool_4bool_18Tuple2_4bool_4bool{
        _0: true,
        _1: false,
        _2: t12,
    }
    var x7 Tuple2_4bool_4bool = a__0._2
    var x9 bool = x7._1
    var w__4 bool = x9
    var b__5 bool = w__4
    print__T_bool(b__5)
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t14 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t14)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv17 string
    var t18 string = _goml_runtime_core_bool_to_string(self__8)
    retv17 = t18
    return retv17
}

func main() {
    main0()
}
