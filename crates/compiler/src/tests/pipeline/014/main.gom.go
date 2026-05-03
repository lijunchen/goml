package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

func test_nested_match(x__0 Tuple2_4bool_4bool, y__1 Tuple2_4bool_4bool) struct{} {
    var x0 bool = x__0._0
    var x1 bool = x__0._1
    switch x1 {
    case true:
        var x2 bool = y__1._0
        var x3 bool = y__1._1
        switch x3 {
        case true:
            switch x2 {
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
        switch x0 {
        case true:
            var x4 bool = y__1._0
            var x5 bool = y__1._1
            switch x5 {
            case true:
                switch x4 {
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
            var x6 bool = y__1._0
            var x7 bool = y__1._1
            switch x7 {
            case true:
                switch x6 {
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
    var t31 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t32 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t31, t32)
    var t33 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t34 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t33, t34)
    var t35 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t36 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t35, t36)
    var t37 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t38 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t37, t38)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
