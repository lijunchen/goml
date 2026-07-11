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
    var x22 bool = a__0._0
    var x23 bool = a__0._1
    var jp31 Tuple2_4bool_4bool
    switch x23 {
    case true:
        var jp46 Tuple2_4bool_4bool
        switch x22 {
        case true:
            var t47 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp46 = t47
        case false:
            var t48 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp46 = t48
        default:
            panic("non-exhaustive match")
        }
        jp31 = jp46
    case false:
        var jp50 Tuple2_4bool_4bool
        switch x22 {
        case true:
            var t51 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp50 = t51
        case false:
            var t52 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp50 = t52
        default:
            panic("non-exhaustive match")
        }
        jp31 = jp50
    default:
        panic("non-exhaustive match")
    }
    var b__1 Tuple2_4bool_4bool = jp31
    var x25 bool = b__1._1
    var w__2 bool = x25
    var b_1__3 bool = w__2
    var mtmp26 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: b_1__3,
    }
    var x27 bool = mtmp26._0
    var x28 bool = mtmp26._1
    switch x28 {
    case true:
        switch x27 {
        case true:
            var t36 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
            println__T_string(t36)
        case false:
            var t38 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
            println__T_string(t38)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x27 {
        case true:
            var t41 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
            println__T_string(t41)
        case false:
            var t43 string = _goml_m_inherent_i_int32_i_int32_i_to__string(0)
            println__T_string(t43)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t33 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(c__4)
    println__T_string(t33)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t54 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t54)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv57 string
    var t58 string = _goml_runtime_core_int32_to_string(self__2)
    retv57 = t58
    return retv57
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__7 struct{}) string {
    var retv60 string
    var t61 string = _goml_runtime_core_unit_to_string(self__7)
    retv60 = t61
    return retv60
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv63 string
    retv63 = self__9
    return retv63
}

func main() {
    main0()
}
