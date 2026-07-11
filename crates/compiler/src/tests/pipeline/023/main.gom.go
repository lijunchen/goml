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
    var retv26 int32
    var x7 int32 = pair__0._0
    var x8 string = pair__0._1
    var jp28 int32
    switch x8 {
    case "zero":
        var jp30 int32
        switch x7 {
        case 0:
            jp30 = 1
        default:
            jp30 = 4
        }
        jp28 = jp30
    case "one":
        var jp32 int32
        switch x7 {
        case 0:
            jp32 = 2
        case 1:
            jp32 = 3
        default:
            jp32 = 5
        }
        jp28 = jp32
    default:
        var jp34 int32
        switch x7 {
        case 0:
            jp34 = 2
        default:
            jp34 = 5
        }
        jp28 = jp34
    }
    retv26 = jp28
    return retv26
}

func match_mixed_enum(value__1 Mixed) int32 {
    var retv36 int32
    var jp38 int32
    switch value__1.(type) {
    case OnlyInt:
        var x9 int32 = value__1.(OnlyInt)._0
        var jp40 int32
        switch x9 {
        case 0:
            jp40 = 6
        default:
            jp40 = 7
        }
        jp38 = jp40
    case OnlyStr:
        var x10 string = value__1.(OnlyStr)._0
        var jp42 int32
        switch x10 {
        case "zero":
            jp42 = 8
        default:
            jp42 = 9
        }
        jp38 = jp42
    case Both:
        var x11 int32 = value__1.(Both)._0
        var x12 string = value__1.(Both)._1
        var jp44 int32
        switch x12 {
        case "zero":
            var jp46 int32
            switch x11 {
            case 0:
                jp46 = 10
            default:
                jp46 = 12
            }
            jp44 = jp46
        default:
            var jp48 int32
            switch x11 {
            case 0:
                jp48 = 11
            default:
                jp48 = 13
            }
            jp44 = jp48
        }
        jp38 = jp44
    default:
        panic("non-exhaustive match")
    }
    retv36 = jp38
    return retv36
}

func main0() struct{} {
    var t50 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t51 int32 = match_mixed_pair(t50)
    var t52 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t51)
    println__T_string(t52)
    var t53 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t54 int32 = match_mixed_pair(t53)
    var t55 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t54)
    println__T_string(t55)
    var t56 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t57 int32 = match_mixed_pair(t56)
    var t58 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t57)
    println__T_string(t58)
    var t59 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t60 int32 = match_mixed_pair(t59)
    var t61 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t60)
    println__T_string(t61)
    var t62 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t63 int32 = match_mixed_pair(t62)
    var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t63)
    println__T_string(t64)
    var t65 Mixed = OnlyInt{
        _0: 0,
    }
    var t66 int32 = match_mixed_enum(t65)
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t66)
    println__T_string(t67)
    var t68 Mixed = OnlyInt{
        _0: 5,
    }
    var t69 int32 = match_mixed_enum(t68)
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t69)
    println__T_string(t70)
    var t71 Mixed = OnlyStr{
        _0: "zero",
    }
    var t72 int32 = match_mixed_enum(t71)
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t72)
    println__T_string(t73)
    var t74 Mixed = OnlyStr{
        _0: "hello",
    }
    var t75 int32 = match_mixed_enum(t74)
    var t76 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t75)
    println__T_string(t76)
    var t77 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t78 int32 = match_mixed_enum(t77)
    var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t78)
    println__T_string(t79)
    var t80 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t81 int32 = match_mixed_enum(t80)
    var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t81)
    println__T_string(t82)
    var t83 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t84 int32 = match_mixed_enum(t83)
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t84)
    println__T_string(t85)
    var t86 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t87 int32 = match_mixed_enum(t86)
    var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t87)
    println__T_string(t88)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t91 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t91)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int32_to_string(self__2)
    retv94 = t95
    return retv94
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv97 string
    retv97 = self__9
    return retv97
}

func main() {
    main0()
}
