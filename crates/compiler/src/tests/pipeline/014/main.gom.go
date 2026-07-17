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
    var x61 bool = x__0._0
    var x62 bool = x__0._1
    switch x62 {
    case true:
        var x63 bool = y__1._0
        var x64 bool = y__1._1
        switch x64 {
        case true:
            switch x63 {
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
        switch x61 {
        case true:
            var x65 bool = y__1._0
            var x66 bool = y__1._1
            switch x66 {
            case true:
                switch x65 {
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
            var x67 bool = y__1._0
            var x68 bool = y__1._1
            switch x68 {
            case true:
                switch x67 {
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
    var t92 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t93 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t92, t93)
    var t94 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t95 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t94, t95)
    var t96 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t97 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t96, t97)
    var t98 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t99 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t98, t99)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv104 string
    retv104 = self__37
    return retv104
}

func main() {
    main0()
}
