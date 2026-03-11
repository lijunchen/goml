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

type GoError = error

func match_mixed_pair(pair__0 Tuple2_int32_string) int32 {
    var retv19 int32
    var x0 int32 = pair__0._0
    var x1 string = pair__0._1
    var jp21 int32
    switch x1 {
    case "zero":
        var jp23 int32
        switch x0 {
        case 0:
            jp23 = 1
        default:
            jp23 = 4
        }
        jp21 = jp23
        retv19 = jp21
        return retv19
    case "one":
        var jp25 int32
        switch x0 {
        case 0:
            jp25 = 2
        case 1:
            jp25 = 3
        default:
            jp25 = 5
        }
        jp21 = jp25
        retv19 = jp21
        return retv19
    default:
        var jp27 int32
        switch x0 {
        case 0:
            jp27 = 2
        default:
            jp27 = 5
        }
        jp21 = jp27
        retv19 = jp21
        return retv19
    }
}

func match_mixed_enum(value__1 Mixed) int32 {
    var retv29 int32
    var jp31 int32
    switch value__1.(type) {
    case OnlyInt:
        var x2 int32 = value__1.(OnlyInt)._0
        var jp33 int32
        switch x2 {
        case 0:
            jp33 = 6
        default:
            jp33 = 7
        }
        jp31 = jp33
        retv29 = jp31
        return retv29
    case OnlyStr:
        var x3 string = value__1.(OnlyStr)._0
        var jp35 int32
        switch x3 {
        case "zero":
            jp35 = 8
        default:
            jp35 = 9
        }
        jp31 = jp35
        retv29 = jp31
        return retv29
    case Both:
        var x4 int32 = value__1.(Both)._0
        var x5 string = value__1.(Both)._1
        var jp37 int32
        switch x5 {
        case "zero":
            var jp39 int32
            switch x4 {
            case 0:
                jp39 = 10
            default:
                jp39 = 12
            }
            jp37 = jp39
            jp31 = jp37
            retv29 = jp31
            return retv29
        default:
            var jp41 int32
            switch x4 {
            case 0:
                jp41 = 11
            default:
                jp41 = 13
            }
            jp37 = jp41
            jp31 = jp37
            retv29 = jp31
            return retv29
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t43 Tuple2_int32_string = Tuple2_int32_string{
        _0: 0,
        _1: "zero",
    }
    var t44 int32 = match_mixed_pair(t43)
    var t45 string = int32_to_string(t44)
    string_println(t45)
    var t46 Tuple2_int32_string = Tuple2_int32_string{
        _0: 0,
        _1: "other",
    }
    var t47 int32 = match_mixed_pair(t46)
    var t48 string = int32_to_string(t47)
    string_println(t48)
    var t49 Tuple2_int32_string = Tuple2_int32_string{
        _0: 1,
        _1: "one",
    }
    var t50 int32 = match_mixed_pair(t49)
    var t51 string = int32_to_string(t50)
    string_println(t51)
    var t52 Tuple2_int32_string = Tuple2_int32_string{
        _0: 2,
        _1: "zero",
    }
    var t53 int32 = match_mixed_pair(t52)
    var t54 string = int32_to_string(t53)
    string_println(t54)
    var t55 Tuple2_int32_string = Tuple2_int32_string{
        _0: 2,
        _1: "two",
    }
    var t56 int32 = match_mixed_pair(t55)
    var t57 string = int32_to_string(t56)
    string_println(t57)
    var t58 Mixed = OnlyInt{
        _0: 0,
    }
    var t59 int32 = match_mixed_enum(t58)
    var t60 string = int32_to_string(t59)
    string_println(t60)
    var t61 Mixed = OnlyInt{
        _0: 5,
    }
    var t62 int32 = match_mixed_enum(t61)
    var t63 string = int32_to_string(t62)
    string_println(t63)
    var t64 Mixed = OnlyStr{
        _0: "zero",
    }
    var t65 int32 = match_mixed_enum(t64)
    var t66 string = int32_to_string(t65)
    string_println(t66)
    var t67 Mixed = OnlyStr{
        _0: "hello",
    }
    var t68 int32 = match_mixed_enum(t67)
    var t69 string = int32_to_string(t68)
    string_println(t69)
    var t70 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t71 int32 = match_mixed_enum(t70)
    var t72 string = int32_to_string(t71)
    string_println(t72)
    var t73 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t74 int32 = match_mixed_enum(t73)
    var t75 string = int32_to_string(t74)
    string_println(t75)
    var t76 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t77 int32 = match_mixed_enum(t76)
    var t78 string = int32_to_string(t77)
    string_println(t78)
    var t79 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t80 int32 = match_mixed_enum(t79)
    var t81 string = int32_to_string(t80)
    string_println(t81)
    return struct{}{}
}

func main() {
    main0()
}
