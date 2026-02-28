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
    var x0 bool
    var x1 bool
    var x2 bool
    var x3 bool
    var t15 struct{}
    var t16 struct{}
    var t17 struct{}
    var x4 bool
    var x5 bool
    var t21 struct{}
    var t22 struct{}
    var t23 struct{}
    var x6 bool
    var x7 bool
    var t26 struct{}
    var t27 struct{}
    var t28 struct{}
    _ = t15
    _ = t16
    _ = t17
    _ = t21
    _ = t22
    _ = t23
    _ = t26
    _ = t27
    _ = t28
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            x0 = x__0._0
            x1 = x__0._1
            switch x1 {
            case true:
                pc = 2
            case false:
                pc = 9
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return struct{}{}
        case 2:
            x2 = y__1._0
            x3 = y__1._1
            switch x3 {
            case true:
                pc = 4
            case false:
                pc = 8
            default:
                panic("non-exhaustive match")
            }
        case 3:
            pc = 1
        case 4:
            switch x2 {
            case true:
                pc = 6
            case false:
                pc = 7
            default:
                panic("non-exhaustive match")
            }
        case 5:
            pc = 3
        case 6:
            string_println("case4")
            pc = 5
        case 7:
            string_println("case3")
            pc = 5
        case 8:
            string_println("case4")
            pc = 3
        case 9:
            switch x0 {
            case true:
                pc = 11
            case false:
                pc = 18
            default:
                panic("non-exhaustive match")
            }
        case 10:
            pc = 1
        case 11:
            x4 = y__1._0
            x5 = y__1._1
            switch x5 {
            case true:
                pc = 13
            case false:
                pc = 17
            default:
                panic("non-exhaustive match")
            }
        case 12:
            pc = 10
        case 13:
            switch x4 {
            case true:
                pc = 15
            case false:
                pc = 16
            default:
                panic("non-exhaustive match")
            }
        case 14:
            pc = 12
        case 15:
            string_println("case2")
            pc = 14
        case 16:
            string_println("case1")
            pc = 14
        case 17:
            string_println("case2")
            pc = 12
        case 18:
            x6 = y__1._0
            x7 = y__1._1
            switch x7 {
            case true:
                pc = 20
            case false:
                pc = 24
            default:
                panic("non-exhaustive match")
            }
        case 19:
            pc = 10
        case 20:
            switch x6 {
            case true:
                pc = 22
            case false:
                pc = 23
            default:
                panic("non-exhaustive match")
            }
        case 21:
            pc = 19
        case 22:
            string_println("case4")
            pc = 21
        case 23:
            string_println("case3")
            pc = 21
        case 24:
            string_println("case4")
            pc = 19
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var t29 Tuple2_bool_bool
    var t30 Tuple2_bool_bool
    var mtmp8 struct{}
    var t31 Tuple2_bool_bool
    var t32 Tuple2_bool_bool
    var mtmp9 struct{}
    var t33 Tuple2_bool_bool
    var t34 Tuple2_bool_bool
    var mtmp10 struct{}
    var t35 Tuple2_bool_bool
    var t36 Tuple2_bool_bool
    var mtmp11 struct{}
    _ = mtmp8
    _ = mtmp9
    _ = mtmp10
    _ = mtmp11
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t29 = Tuple2_bool_bool{
                _0: true,
                _1: false,
            }
            t30 = Tuple2_bool_bool{
                _0: false,
                _1: true,
            }
            test_nested_match(t29, t30)
            t31 = Tuple2_bool_bool{
                _0: true,
                _1: false,
            }
            t32 = Tuple2_bool_bool{
                _0: true,
                _1: false,
            }
            test_nested_match(t31, t32)
            t33 = Tuple2_bool_bool{
                _0: false,
                _1: true,
            }
            t34 = Tuple2_bool_bool{
                _0: false,
                _1: true,
            }
            test_nested_match(t33, t34)
            t35 = Tuple2_bool_bool{
                _0: false,
                _1: true,
            }
            t36 = Tuple2_bool_bool{
                _0: true,
                _1: false,
            }
            test_nested_match(t35, t36)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
