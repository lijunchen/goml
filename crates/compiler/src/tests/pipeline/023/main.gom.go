package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Tuple2_int32_string struct {
    _0 int32
    _1 string
}

type Mixed interface {
    isMixed()
}

type OnlyInt struct {
    _0 int32
}

func (_ OnlyInt) isMixed() {}

type OnlyStr struct {
    _0 string
}

func (_ OnlyStr) isMixed() {}

type Both struct {
    _0 int32
    _1 string
}

func (_ Both) isMixed() {}

func match_mixed_pair(pair__0 Tuple2_int32_string) int32 {
    var x0 int32 = pair__0._0
    var x1 string = pair__0._1
    var jp19 int32
    switch x1 {
    case "zero":
        var jp21 int32
        switch x0 {
        case 0:
            jp21 = 1
        default:
            jp21 = 4
        }
        jp19 = jp21
        return jp19
    case "one":
        var jp23 int32
        switch x0 {
        case 0:
            jp23 = 2
        case 1:
            jp23 = 3
        default:
            jp23 = 5
        }
        jp19 = jp23
        return jp19
    default:
        var jp25 int32
        switch x0 {
        case 0:
            jp25 = 2
        default:
            jp25 = 5
        }
        jp19 = jp25
        return jp19
    }
}

func match_mixed_enum(value__1 Mixed) int32 {
    var jp27 int32
    switch value__1.(type) {
    case OnlyInt:
        var x2 int32 = value__1.(OnlyInt)._0
        var jp29 int32
        switch x2 {
        case 0:
            jp29 = 6
        default:
            jp29 = 7
        }
        jp27 = jp29
        return jp27
    case OnlyStr:
        var x3 string = value__1.(OnlyStr)._0
        var jp31 int32
        switch x3 {
        case "zero":
            jp31 = 8
        default:
            jp31 = 9
        }
        jp27 = jp31
        return jp27
    case Both:
        var x4 int32 = value__1.(Both)._0
        var x5 string = value__1.(Both)._1
        var jp33 int32
        switch x5 {
        case "zero":
            var jp35 int32
            switch x4 {
            case 0:
                jp35 = 10
            default:
                jp35 = 12
            }
            jp33 = jp35
            jp27 = jp33
            return jp27
        default:
            var jp37 int32
            switch x4 {
            case 0:
                jp37 = 11
            default:
                jp37 = 13
            }
            jp33 = jp37
            jp27 = jp33
            return jp27
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t38 Tuple2_int32_string = Tuple2_int32_string{
        _0: 0,
        _1: "zero",
    }
    var t39 int32 = match_mixed_pair(t38)
    var t40 string = int32_to_string(t39)
    string_println(t40)
    var t41 Tuple2_int32_string = Tuple2_int32_string{
        _0: 0,
        _1: "other",
    }
    var t42 int32 = match_mixed_pair(t41)
    var t43 string = int32_to_string(t42)
    string_println(t43)
    var t44 Tuple2_int32_string = Tuple2_int32_string{
        _0: 1,
        _1: "one",
    }
    var t45 int32 = match_mixed_pair(t44)
    var t46 string = int32_to_string(t45)
    string_println(t46)
    var t47 Tuple2_int32_string = Tuple2_int32_string{
        _0: 2,
        _1: "zero",
    }
    var t48 int32 = match_mixed_pair(t47)
    var t49 string = int32_to_string(t48)
    string_println(t49)
    var t50 Tuple2_int32_string = Tuple2_int32_string{
        _0: 2,
        _1: "two",
    }
    var t51 int32 = match_mixed_pair(t50)
    var t52 string = int32_to_string(t51)
    string_println(t52)
    var t53 Mixed = OnlyInt{
        _0: 0,
    }
    var t54 int32 = match_mixed_enum(t53)
    var t55 string = int32_to_string(t54)
    string_println(t55)
    var t56 Mixed = OnlyInt{
        _0: 5,
    }
    var t57 int32 = match_mixed_enum(t56)
    var t58 string = int32_to_string(t57)
    string_println(t58)
    var t59 Mixed = OnlyStr{
        _0: "zero",
    }
    var t60 int32 = match_mixed_enum(t59)
    var t61 string = int32_to_string(t60)
    string_println(t61)
    var t62 Mixed = OnlyStr{
        _0: "hello",
    }
    var t63 int32 = match_mixed_enum(t62)
    var t64 string = int32_to_string(t63)
    string_println(t64)
    var t65 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t66 int32 = match_mixed_enum(t65)
    var t67 string = int32_to_string(t66)
    string_println(t67)
    var t68 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t69 int32 = match_mixed_enum(t68)
    var t70 string = int32_to_string(t69)
    string_println(t70)
    var t71 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t72 int32 = match_mixed_enum(t71)
    var t73 string = int32_to_string(t72)
    string_println(t73)
    var t74 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t75 int32 = match_mixed_enum(t74)
    var t76 string = int32_to_string(t75)
    var t77 struct{} = string_println(t76)
    return t77
}

func main() {
    main0()
}
