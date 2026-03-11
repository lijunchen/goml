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

type Tuple2_bool_bool struct {
    _0 bool
    _1 bool
}

type GoError = error

func main0() struct{} {
    var a__0 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: true,
    }
    var x0 bool = a__0._0
    var x1 bool = a__0._1
    switch x1 {
    case true:
        switch x0 {
        case true:
            var t5 string = int32_to_string(789)
            string_print(t5)
        case false:
            var t7 string = int32_to_string(456)
            string_print(t7)
        default:
            panic("non-exhaustive match")
        }
        return struct{}{}
    case false:
        switch x0 {
        case true:
            var t10 string = int32_to_string(123)
            string_print(t10)
        case false:
            var t12 string = int32_to_string(789)
            string_print(t12)
        default:
            panic("non-exhaustive match")
        }
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
