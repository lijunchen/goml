package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
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
    var x4 bool = a__0._0
    var x5 bool = a__0._1
    var jp13 Tuple2_4bool_4bool
    switch x5 {
    case true:
        var jp28 Tuple2_4bool_4bool
        switch x4 {
        case true:
            var t29 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp28 = t29
        case false:
            var t30 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp28 = t30
        default:
            panic("non-exhaustive match")
        }
        jp13 = jp28
    case false:
        var jp32 Tuple2_4bool_4bool
        switch x4 {
        case true:
            var t33 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp32 = t33
        case false:
            var t34 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp32 = t34
        default:
            panic("non-exhaustive match")
        }
        jp13 = jp32
    default:
        panic("non-exhaustive match")
    }
    var b__1 Tuple2_4bool_4bool = jp13
    var x7 bool = b__1._1
    var w__2 bool = x7
    var b_1__3 bool = w__2
    var mtmp8 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: b_1__3,
    }
    var x9 bool = mtmp8._0
    var x10 bool = mtmp8._1
    switch x10 {
    case true:
        switch x9 {
        case true:
            var t18 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
            println__T_string(t18)
        case false:
            var t20 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
            println__T_string(t20)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x9 {
        case true:
            var t23 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
            println__T_string(t23)
        case false:
            var t25 string = _goml_m_inherent_i_int32_i_int32_i_to__string(0)
            println__T_string(t25)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t15 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(c__4)
    println__T_string(t15)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv39 string
    var t40 string = _goml_runtime_core_int32_to_string(self__2)
    retv39 = t40
    return retv39
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__7 struct{}) string {
    var retv42 string
    var t43 string = _goml_runtime_core_unit_to_string(self__7)
    retv42 = t43
    return retv42
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv45 string
    retv45 = self__9
    return retv45
}

func main() {
    main0()
}
