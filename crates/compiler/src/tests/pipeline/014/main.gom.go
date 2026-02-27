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
    var ret31 struct{}
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
                ret31 = string_println("case4")
            case false:
                ret31 = string_println("case3")
            }
        case false:
            ret31 = string_println("case4")
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
                    ret31 = string_println("case2")
                case false:
                    ret31 = string_println("case1")
                }
            case false:
                ret31 = string_println("case2")
            }
        case false:
            var x6 bool = y__1._0
            var x7 bool = y__1._1
            switch x7 {
            case true:
                switch x6 {
                case true:
                    ret31 = string_println("case4")
                case false:
                    ret31 = string_println("case3")
                }
            case false:
                ret31 = string_println("case4")
            }
        }
    }
    return ret31
}

func main0() struct{} {
    var ret32 struct{}
    var t23 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    var t24 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t23, t24)
    var t25 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    var t26 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t25, t26)
    var t27 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    var t28 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t27, t28)
    var t29 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    var t30 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t29, t30)
    ret32 = struct{}{}
    return ret32
}

func main() {
    main0()
}
