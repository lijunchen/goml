package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

func test_nested_match(x__0 Tuple2_4bool_4bool, y__1 Tuple2_4bool_4bool) struct{} {
    var x22 bool = x__0._0
    var x23 bool = x__0._1
    switch x23 {
    case true:
        var x24 bool = y__1._0
        var x25 bool = y__1._1
        switch x25 {
        case true:
            switch x24 {
            case true:
                println__T_string("case4")
            case false:
                println__T_string("case3")
            default:
                panic("non-exhaustive match")
            }
        case false:
            println__T_string("case4")
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x22 {
        case true:
            var x26 bool = y__1._0
            var x27 bool = y__1._1
            switch x27 {
            case true:
                switch x26 {
                case true:
                    println__T_string("case2")
                case false:
                    println__T_string("case1")
                default:
                    panic("non-exhaustive match")
                }
            case false:
                println__T_string("case2")
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x28 bool = y__1._0
            var x29 bool = y__1._1
            switch x29 {
            case true:
                switch x28 {
                case true:
                    println__T_string("case4")
                case false:
                    println__T_string("case3")
                default:
                    panic("non-exhaustive match")
                }
            case false:
                println__T_string("case4")
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t53 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t54 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t53, t54)
    var t55 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t56 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t55, t56)
    var t57 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t58 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t57, t58)
    var t59 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t60 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t59, t60)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t62 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t62)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv65 string
    retv65 = self__9
    return retv65
}

func main() {
    main0()
}
