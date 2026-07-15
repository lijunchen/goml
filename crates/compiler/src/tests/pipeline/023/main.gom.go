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
    var retv41 int32
    var x22 int32 = pair__0._0
    var x23 string = pair__0._1
    var jp43 int32
    switch x23 {
    case "zero":
        var jp45 int32
        switch x22 {
        case 0:
            jp45 = 1
        default:
            jp45 = 4
        }
        jp43 = jp45
    case "one":
        var jp47 int32
        switch x22 {
        case 0:
            jp47 = 2
        case 1:
            jp47 = 3
        default:
            jp47 = 5
        }
        jp43 = jp47
    default:
        var jp49 int32
        switch x22 {
        case 0:
            jp49 = 2
        default:
            jp49 = 5
        }
        jp43 = jp49
    }
    retv41 = jp43
    return retv41
}

func match_mixed_enum(value__1 Mixed) int32 {
    var retv51 int32
    var jp53 int32
    switch value__1.(type) {
    case OnlyInt:
        var x24 int32 = value__1.(OnlyInt)._0
        var jp55 int32
        switch x24 {
        case 0:
            jp55 = 6
        default:
            jp55 = 7
        }
        jp53 = jp55
    case OnlyStr:
        var x25 string = value__1.(OnlyStr)._0
        var jp57 int32
        switch x25 {
        case "zero":
            jp57 = 8
        default:
            jp57 = 9
        }
        jp53 = jp57
    case Both:
        var x26 int32 = value__1.(Both)._0
        var x27 string = value__1.(Both)._1
        var jp59 int32
        switch x27 {
        case "zero":
            var jp61 int32
            switch x26 {
            case 0:
                jp61 = 10
            default:
                jp61 = 12
            }
            jp59 = jp61
        default:
            var jp63 int32
            switch x26 {
            case 0:
                jp63 = 11
            default:
                jp63 = 13
            }
            jp59 = jp63
        }
        jp53 = jp59
    default:
        panic("non-exhaustive match")
    }
    retv51 = jp53
    return retv51
}

func main0() struct{} {
    var t65 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t66 int32 = match_mixed_pair(t65)
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t66)
    println__T_string(t67)
    var t68 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t69 int32 = match_mixed_pair(t68)
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t69)
    println__T_string(t70)
    var t71 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t72 int32 = match_mixed_pair(t71)
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t72)
    println__T_string(t73)
    var t74 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t75 int32 = match_mixed_pair(t74)
    var t76 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t75)
    println__T_string(t76)
    var t77 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t78 int32 = match_mixed_pair(t77)
    var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t78)
    println__T_string(t79)
    var t80 Mixed = OnlyInt{
        _0: 0,
    }
    var t81 int32 = match_mixed_enum(t80)
    var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t81)
    println__T_string(t82)
    var t83 Mixed = OnlyInt{
        _0: 5,
    }
    var t84 int32 = match_mixed_enum(t83)
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t84)
    println__T_string(t85)
    var t86 Mixed = OnlyStr{
        _0: "zero",
    }
    var t87 int32 = match_mixed_enum(t86)
    var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t87)
    println__T_string(t88)
    var t89 Mixed = OnlyStr{
        _0: "hello",
    }
    var t90 int32 = match_mixed_enum(t89)
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t90)
    println__T_string(t91)
    var t92 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t93 int32 = match_mixed_enum(t92)
    var t94 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t93)
    println__T_string(t94)
    var t95 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t96 int32 = match_mixed_enum(t95)
    var t97 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t96)
    println__T_string(t97)
    var t98 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t99 int32 = match_mixed_enum(t98)
    var t100 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t99)
    println__T_string(t100)
    var t101 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t102 int32 = match_mixed_enum(t101)
    var t103 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t102)
    println__T_string(t103)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t106 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t106)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv109 string
    var t110 string = _goml_runtime_core_int32_to_string(self__2)
    retv109 = t110
    return retv109
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv112 string
    retv112 = self__9
    return retv112
}

func main() {
    main0()
}
