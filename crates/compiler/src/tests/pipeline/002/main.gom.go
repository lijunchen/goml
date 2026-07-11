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
    var x7 bool = a__0._0
    var x8 bool = a__0._1
    var jp16 Tuple2_4bool_4bool
    switch x8 {
    case true:
        var jp31 Tuple2_4bool_4bool
        switch x7 {
        case true:
            var t32 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp31 = t32
        case false:
            var t33 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp31 = t33
        default:
            panic("non-exhaustive match")
        }
        jp16 = jp31
    case false:
        var jp35 Tuple2_4bool_4bool
        switch x7 {
        case true:
            var t36 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp35 = t36
        case false:
            var t37 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp35 = t37
        default:
            panic("non-exhaustive match")
        }
        jp16 = jp35
    default:
        panic("non-exhaustive match")
    }
    var b__1 Tuple2_4bool_4bool = jp16
    var x10 bool = b__1._1
    var w__2 bool = x10
    var b_1__3 bool = w__2
    var mtmp11 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: b_1__3,
    }
    var x12 bool = mtmp11._0
    var x13 bool = mtmp11._1
    switch x13 {
    case true:
        switch x12 {
        case true:
            var t21 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
            println__T_string(t21)
        case false:
            var t23 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
            println__T_string(t23)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x12 {
        case true:
            var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
            println__T_string(t26)
        case false:
            var t28 string = _goml_m_inherent_i_int32_i_int32_i_to__string(0)
            println__T_string(t28)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t18 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(c__4)
    println__T_string(t18)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t39 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t39)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv42 string
    var t43 string = _goml_runtime_core_int32_to_string(self__2)
    retv42 = t43
    return retv42
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__7 struct{}) string {
    var retv45 string
    var t46 string = _goml_runtime_core_unit_to_string(self__7)
    retv45 = t46
    return retv45
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv48 string
    retv48 = self__9
    return retv48
}

func main() {
    main0()
}
