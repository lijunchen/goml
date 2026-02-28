package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

type T interface {
    isT()
}

type A struct {}

func (_ A) isT() {}

type B struct {
    _0 bool
    _1 bool
}

func (_ B) isT() {}

func test(t__0 T) struct{} {
    var t7 string
    var t8 struct{}
    var x0 bool
    var x1 bool
    var t11 string
    var t12 struct{}
    var t13 string
    var t14 struct{}
    var t16 string
    var t17 struct{}
    var t18 string
    var t19 struct{}
    _ = t8
    _ = t12
    _ = t14
    _ = t17
    _ = t19
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch t__0.(type) {
            case A:
                pc = 2
            case B:
                pc = 3
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return struct{}{}
        case 2:
            t7 = int32_to_string(1)
            string_print(t7)
            pc = 1
        case 3:
            x0 = t__0.(B)._0
            x1 = t__0.(B)._1
            switch x1 {
            case true:
                pc = 5
            case false:
                pc = 9
            default:
                panic("non-exhaustive match")
            }
        case 4:
            pc = 1
        case 5:
            switch x0 {
            case true:
                pc = 7
            case false:
                pc = 8
            default:
                panic("non-exhaustive match")
            }
        case 6:
            pc = 4
        case 7:
            t11 = int32_to_string(4)
            string_print(t11)
            pc = 6
        case 8:
            t13 = int32_to_string(3)
            string_print(t13)
            pc = 6
        case 9:
            switch x0 {
            case true:
                pc = 11
            case false:
                pc = 12
            default:
                panic("non-exhaustive match")
            }
        case 10:
            pc = 4
        case 11:
            t16 = int32_to_string(4)
            string_print(t16)
            pc = 10
        case 12:
            t18 = int32_to_string(2)
            string_print(t18)
            pc = 10
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var t20 T
    var mtmp2 struct{}
    var t21 T
    var mtmp3 struct{}
    var t22 T
    var mtmp4 struct{}
    var mtmp5 struct{}
    _ = mtmp2
    _ = mtmp3
    _ = mtmp4
    _ = mtmp5
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t20 = B{
                _0: true,
                _1: true,
            }
            test(t20)
            t21 = B{
                _0: false,
                _1: true,
            }
            test(t21)
            t22 = B{
                _0: false,
                _1: false,
            }
            test(t22)
            test(A{})
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
