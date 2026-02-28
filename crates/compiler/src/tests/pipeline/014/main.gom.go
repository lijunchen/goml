package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Tuple2_bool_bool struct {
    _0 bool
    _1 bool
}

func test_nested_match(x__0 Tuple2_bool_bool, y__1 Tuple2_bool_bool) struct{} {
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
                string_println("case4")
            case false:
                string_println("case3")
            default:
                panic("non-exhaustive match")
            }
            return struct{}{}
        case false:
            string_println("case4")
            return struct{}{}
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
                    string_println("case2")
                case false:
                    string_println("case1")
                default:
                    panic("non-exhaustive match")
                }
                return struct{}{}
            case false:
                string_println("case2")
                return struct{}{}
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
                    string_println("case4")
                case false:
                    string_println("case3")
                default:
                    panic("non-exhaustive match")
                }
                return struct{}{}
            case false:
                string_println("case4")
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t29 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    var t30 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t29, t30)
    var t31 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    var t32 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t31, t32)
    var t33 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    var t34 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t33, t34)
    var t35 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    var t36 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t35, t36)
    return struct{}{}
}

func main() {
    main0()
}
