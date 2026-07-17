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
    var retv80 int32
    var x61 int32 = pair__0._0
    var x62 string = pair__0._1
    var jp82 int32
    switch x62 {
    case "zero":
        var jp84 int32
        switch x61 {
        case 0:
            jp84 = 1
        default:
            jp84 = 4
        }
        jp82 = jp84
    case "one":
        var jp86 int32
        switch x61 {
        case 0:
            jp86 = 2
        case 1:
            jp86 = 3
        default:
            jp86 = 5
        }
        jp82 = jp86
    default:
        var jp88 int32
        switch x61 {
        case 0:
            jp88 = 2
        default:
            jp88 = 5
        }
        jp82 = jp88
    }
    retv80 = jp82
    return retv80
}

func match_mixed_enum(value__1 Mixed) int32 {
    var retv90 int32
    var jp92 int32
    switch value__1.(type) {
    case OnlyInt:
        var x63 int32 = value__1.(OnlyInt)._0
        var jp94 int32
        switch x63 {
        case 0:
            jp94 = 6
        default:
            jp94 = 7
        }
        jp92 = jp94
    case OnlyStr:
        var x64 string = value__1.(OnlyStr)._0
        var jp96 int32
        switch x64 {
        case "zero":
            jp96 = 8
        default:
            jp96 = 9
        }
        jp92 = jp96
    case Both:
        var x65 int32 = value__1.(Both)._0
        var x66 string = value__1.(Both)._1
        var jp98 int32
        switch x66 {
        case "zero":
            var jp100 int32
            switch x65 {
            case 0:
                jp100 = 10
            default:
                jp100 = 12
            }
            jp98 = jp100
        default:
            var jp102 int32
            switch x65 {
            case 0:
                jp102 = 11
            default:
                jp102 = 13
            }
            jp98 = jp102
        }
        jp92 = jp98
    default:
        panic("non-exhaustive match")
    }
    retv90 = jp92
    return retv90
}

func main0() struct{} {
    var t104 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t105 int32 = match_mixed_pair(t104)
    var t106 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t105)
    println__T_string(t106)
    var t107 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t108 int32 = match_mixed_pair(t107)
    var t109 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t108)
    println__T_string(t109)
    var t110 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t111 int32 = match_mixed_pair(t110)
    var t112 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t111)
    println__T_string(t112)
    var t113 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t114 int32 = match_mixed_pair(t113)
    var t115 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t114)
    println__T_string(t115)
    var t116 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t117 int32 = match_mixed_pair(t116)
    var t118 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t117)
    println__T_string(t118)
    var t119 Mixed = OnlyInt{
        _0: 0,
    }
    var t120 int32 = match_mixed_enum(t119)
    var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t120)
    println__T_string(t121)
    var t122 Mixed = OnlyInt{
        _0: 5,
    }
    var t123 int32 = match_mixed_enum(t122)
    var t124 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t123)
    println__T_string(t124)
    var t125 Mixed = OnlyStr{
        _0: "zero",
    }
    var t126 int32 = match_mixed_enum(t125)
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t126)
    println__T_string(t127)
    var t128 Mixed = OnlyStr{
        _0: "hello",
    }
    var t129 int32 = match_mixed_enum(t128)
    var t130 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t129)
    println__T_string(t130)
    var t131 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t132 int32 = match_mixed_enum(t131)
    var t133 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t132)
    println__T_string(t133)
    var t134 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t135 int32 = match_mixed_enum(t134)
    var t136 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t135)
    println__T_string(t136)
    var t137 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t138 int32 = match_mixed_enum(t137)
    var t139 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t138)
    println__T_string(t139)
    var t140 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t141 int32 = match_mixed_enum(t140)
    var t142 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t141)
    println__T_string(t142)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t145 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t145)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv148 string
    var t149 string = _goml_runtime_core_int32_to_string(self__5)
    retv148 = t149
    return retv148
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv151 string
    retv151 = self__37
    return retv151
}

func main() {
    main0()
}
