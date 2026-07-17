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
    var x58 bool = x__0._0
    var x59 bool = x__0._1
    switch x59 {
    case true:
        var x60 bool = y__1._0
        var x61 bool = y__1._1
        switch x61 {
        case true:
            switch x60 {
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
        switch x58 {
        case true:
            var x62 bool = y__1._0
            var x63 bool = y__1._1
            switch x63 {
            case true:
                switch x62 {
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
            var x64 bool = y__1._0
            var x65 bool = y__1._1
            switch x65 {
            case true:
                switch x64 {
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
    var t89 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t90 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t89, t90)
    var t91 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t92 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t91, t92)
    var t93 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t94 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t93, t94)
    var t95 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t96 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t95, t96)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv101 string
    retv101 = self__34
    return retv101
}

func main() {
    main0()
}
