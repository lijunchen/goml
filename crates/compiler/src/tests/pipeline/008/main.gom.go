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
    _1 struct{}
}

func (_ B) isT() {}

func main0() struct{} {
    var t__0 T
    var t3 string
    var t4 struct{}
    var x0 bool
    var t7 string
    var t8 struct{}
    var t9 string
    var t10 struct{}
    _ = t4
    _ = t8
    _ = t10
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t__0 = B{
                _0: true,
                _1: struct{}{},
            }
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
            t3 = int32_to_string(1)
            string_print(t3)
            pc = 1
        case 3:
            x0 = t__0.(B)._0
            pc = 5
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
            t7 = int32_to_string(2)
            string_print(t7)
            pc = 6
        case 8:
            t9 = int32_to_string(3)
            string_print(t9)
            pc = 6
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
