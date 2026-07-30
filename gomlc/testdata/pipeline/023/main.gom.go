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
    var retv87 int32
    var x68 int32 = pair__0._0
    var x69 string = pair__0._1
    var jp89 int32
    switch x69 {
    case "zero":
        var jp91 int32
        switch x68 {
        case 0:
            jp91 = 1
        default:
            jp91 = 4
        }
        jp89 = jp91
    case "one":
        var jp93 int32
        switch x68 {
        case 0:
            jp93 = 2
        case 1:
            jp93 = 3
        default:
            jp93 = 5
        }
        jp89 = jp93
    default:
        var jp95 int32
        switch x68 {
        case 0:
            jp95 = 2
        default:
            jp95 = 5
        }
        jp89 = jp95
    }
    retv87 = jp89
    return retv87
}

func match_mixed_enum(value__1 Mixed) int32 {
    var retv97 int32
    var jp99 int32
    switch value__1.(type) {
    case OnlyInt:
        var x70 int32 = value__1.(OnlyInt)._0
        var jp101 int32
        switch x70 {
        case 0:
            jp101 = 6
        default:
            jp101 = 7
        }
        jp99 = jp101
    case OnlyStr:
        var x71 string = value__1.(OnlyStr)._0
        var jp103 int32
        switch x71 {
        case "zero":
            jp103 = 8
        default:
            jp103 = 9
        }
        jp99 = jp103
    case Both:
        var x72 int32 = value__1.(Both)._0
        var x73 string = value__1.(Both)._1
        var jp105 int32
        switch x73 {
        case "zero":
            var jp107 int32
            switch x72 {
            case 0:
                jp107 = 10
            default:
                jp107 = 12
            }
            jp105 = jp107
        default:
            var jp109 int32
            switch x72 {
            case 0:
                jp109 = 11
            default:
                jp109 = 13
            }
            jp105 = jp109
        }
        jp99 = jp105
    default:
        panic("non-exhaustive match")
    }
    retv97 = jp99
    return retv97
}

func main0() struct{} {
    var t111 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t112 int32 = match_mixed_pair(t111)
    var t113 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t112)
    println__T_string(t113)
    var t114 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t115 int32 = match_mixed_pair(t114)
    var t116 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t115)
    println__T_string(t116)
    var t117 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t118 int32 = match_mixed_pair(t117)
    var t119 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t118)
    println__T_string(t119)
    var t120 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t121 int32 = match_mixed_pair(t120)
    var t122 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t121)
    println__T_string(t122)
    var t123 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t124 int32 = match_mixed_pair(t123)
    var t125 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t124)
    println__T_string(t125)
    var t126 Mixed = OnlyInt{
        _0: 0,
    }
    var t127 int32 = match_mixed_enum(t126)
    var t128 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t127)
    println__T_string(t128)
    var t129 Mixed = OnlyInt{
        _0: 5,
    }
    var t130 int32 = match_mixed_enum(t129)
    var t131 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t130)
    println__T_string(t131)
    var t132 Mixed = OnlyStr{
        _0: "zero",
    }
    var t133 int32 = match_mixed_enum(t132)
    var t134 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t133)
    println__T_string(t134)
    var t135 Mixed = OnlyStr{
        _0: "hello",
    }
    var t136 int32 = match_mixed_enum(t135)
    var t137 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t136)
    println__T_string(t137)
    var t138 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t139 int32 = match_mixed_enum(t138)
    var t140 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t139)
    println__T_string(t140)
    var t141 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t142 int32 = match_mixed_enum(t141)
    var t143 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t142)
    println__T_string(t143)
    var t144 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t145 int32 = match_mixed_enum(t144)
    var t146 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t145)
    println__T_string(t146)
    var t147 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t148 int32 = match_mixed_enum(t147)
    var t149 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t148)
    println__T_string(t149)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t152 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t152)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv155 string
    var t156 string = _goml_runtime_core_int32_to_string(self__6)
    retv155 = t156
    return retv155
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv158 string
    retv158 = self__38
    return retv158
}

func main() {
    main0()
}
