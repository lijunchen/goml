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
    var retv77 int32
    var x58 int32 = pair__0._0
    var x59 string = pair__0._1
    var jp79 int32
    switch x59 {
    case "zero":
        var jp81 int32
        switch x58 {
        case 0:
            jp81 = 1
        default:
            jp81 = 4
        }
        jp79 = jp81
    case "one":
        var jp83 int32
        switch x58 {
        case 0:
            jp83 = 2
        case 1:
            jp83 = 3
        default:
            jp83 = 5
        }
        jp79 = jp83
    default:
        var jp85 int32
        switch x58 {
        case 0:
            jp85 = 2
        default:
            jp85 = 5
        }
        jp79 = jp85
    }
    retv77 = jp79
    return retv77
}

func match_mixed_enum(value__1 Mixed) int32 {
    var retv87 int32
    var jp89 int32
    switch value__1.(type) {
    case OnlyInt:
        var x60 int32 = value__1.(OnlyInt)._0
        var jp91 int32
        switch x60 {
        case 0:
            jp91 = 6
        default:
            jp91 = 7
        }
        jp89 = jp91
    case OnlyStr:
        var x61 string = value__1.(OnlyStr)._0
        var jp93 int32
        switch x61 {
        case "zero":
            jp93 = 8
        default:
            jp93 = 9
        }
        jp89 = jp93
    case Both:
        var x62 int32 = value__1.(Both)._0
        var x63 string = value__1.(Both)._1
        var jp95 int32
        switch x63 {
        case "zero":
            var jp97 int32
            switch x62 {
            case 0:
                jp97 = 10
            default:
                jp97 = 12
            }
            jp95 = jp97
        default:
            var jp99 int32
            switch x62 {
            case 0:
                jp99 = 11
            default:
                jp99 = 13
            }
            jp95 = jp99
        }
        jp89 = jp95
    default:
        panic("non-exhaustive match")
    }
    retv87 = jp89
    return retv87
}

func main0() struct{} {
    var t101 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t102 int32 = match_mixed_pair(t101)
    var t103 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t102)
    println__T_string(t103)
    var t104 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t105 int32 = match_mixed_pair(t104)
    var t106 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t105)
    println__T_string(t106)
    var t107 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t108 int32 = match_mixed_pair(t107)
    var t109 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t108)
    println__T_string(t109)
    var t110 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t111 int32 = match_mixed_pair(t110)
    var t112 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t111)
    println__T_string(t112)
    var t113 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t114 int32 = match_mixed_pair(t113)
    var t115 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t114)
    println__T_string(t115)
    var t116 Mixed = OnlyInt{
        _0: 0,
    }
    var t117 int32 = match_mixed_enum(t116)
    var t118 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t117)
    println__T_string(t118)
    var t119 Mixed = OnlyInt{
        _0: 5,
    }
    var t120 int32 = match_mixed_enum(t119)
    var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t120)
    println__T_string(t121)
    var t122 Mixed = OnlyStr{
        _0: "zero",
    }
    var t123 int32 = match_mixed_enum(t122)
    var t124 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t123)
    println__T_string(t124)
    var t125 Mixed = OnlyStr{
        _0: "hello",
    }
    var t126 int32 = match_mixed_enum(t125)
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t126)
    println__T_string(t127)
    var t128 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t129 int32 = match_mixed_enum(t128)
    var t130 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t129)
    println__T_string(t130)
    var t131 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t132 int32 = match_mixed_enum(t131)
    var t133 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t132)
    println__T_string(t133)
    var t134 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t135 int32 = match_mixed_enum(t134)
    var t136 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t135)
    println__T_string(t136)
    var t137 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t138 int32 = match_mixed_enum(t137)
    var t139 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t138)
    println__T_string(t139)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t142 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t142)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv145 string
    var t146 string = _goml_runtime_core_int32_to_string(self__2)
    retv145 = t146
    return retv145
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv148 string
    retv148 = self__34
    return retv148
}

func main() {
    main0()
}
