package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_5int32_6string struct {
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

func match_mixed_pair(pair__0 Tuple2_5int32_6string) int32 {
    var retv23 int32
    var x4 int32 = pair__0._0
    var x5 string = pair__0._1
    var jp25 int32
    switch x5 {
    case "zero":
        var jp27 int32
        switch x4 {
        case 0:
            jp27 = 1
        default:
            jp27 = 4
        }
        jp25 = jp27
    case "one":
        var jp29 int32
        switch x4 {
        case 0:
            jp29 = 2
        case 1:
            jp29 = 3
        default:
            jp29 = 5
        }
        jp25 = jp29
    default:
        var jp31 int32
        switch x4 {
        case 0:
            jp31 = 2
        default:
            jp31 = 5
        }
        jp25 = jp31
    }
    retv23 = jp25
    return retv23
}

func match_mixed_enum(value__1 Mixed) int32 {
    var retv33 int32
    var jp35 int32
    switch value__1.(type) {
    case OnlyInt:
        var x6 int32 = value__1.(OnlyInt)._0
        var jp37 int32
        switch x6 {
        case 0:
            jp37 = 6
        default:
            jp37 = 7
        }
        jp35 = jp37
    case OnlyStr:
        var x7 string = value__1.(OnlyStr)._0
        var jp39 int32
        switch x7 {
        case "zero":
            jp39 = 8
        default:
            jp39 = 9
        }
        jp35 = jp39
    case Both:
        var x8 int32 = value__1.(Both)._0
        var x9 string = value__1.(Both)._1
        var jp41 int32
        switch x9 {
        case "zero":
            var jp43 int32
            switch x8 {
            case 0:
                jp43 = 10
            default:
                jp43 = 12
            }
            jp41 = jp43
        default:
            var jp45 int32
            switch x8 {
            case 0:
                jp45 = 11
            default:
                jp45 = 13
            }
            jp41 = jp45
        }
        jp35 = jp41
    default:
        panic("non-exhaustive match")
    }
    retv33 = jp35
    return retv33
}

func main0() struct{} {
    var t47 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t48 int32 = match_mixed_pair(t47)
    var t49 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t48)
    println__T_string(t49)
    var t50 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t51 int32 = match_mixed_pair(t50)
    var t52 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t51)
    println__T_string(t52)
    var t53 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t54 int32 = match_mixed_pair(t53)
    var t55 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t54)
    println__T_string(t55)
    var t56 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t57 int32 = match_mixed_pair(t56)
    var t58 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t57)
    println__T_string(t58)
    var t59 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t60 int32 = match_mixed_pair(t59)
    var t61 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t60)
    println__T_string(t61)
    var t62 Mixed = OnlyInt{
        _0: 0,
    }
    var t63 int32 = match_mixed_enum(t62)
    var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t63)
    println__T_string(t64)
    var t65 Mixed = OnlyInt{
        _0: 5,
    }
    var t66 int32 = match_mixed_enum(t65)
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t66)
    println__T_string(t67)
    var t68 Mixed = OnlyStr{
        _0: "zero",
    }
    var t69 int32 = match_mixed_enum(t68)
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t69)
    println__T_string(t70)
    var t71 Mixed = OnlyStr{
        _0: "hello",
    }
    var t72 int32 = match_mixed_enum(t71)
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t72)
    println__T_string(t73)
    var t74 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t75 int32 = match_mixed_enum(t74)
    var t76 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t75)
    println__T_string(t76)
    var t77 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t78 int32 = match_mixed_enum(t77)
    var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t78)
    println__T_string(t79)
    var t80 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t81 int32 = match_mixed_enum(t80)
    var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t81)
    println__T_string(t82)
    var t83 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t84 int32 = match_mixed_enum(t83)
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t84)
    println__T_string(t85)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv91 string
    var t92 string = _goml_runtime_core_int32_to_string(self__2)
    retv91 = t92
    return retv91
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv94 string
    retv94 = self__9
    return retv94
}

func main() {
    main0()
}
