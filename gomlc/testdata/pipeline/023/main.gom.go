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
    var retv83 int32
    var x64 int32 = pair__0._0
    var x65 string = pair__0._1
    var jp85 int32
    switch x65 {
    case "zero":
        var jp87 int32
        switch x64 {
        case 0:
            jp87 = 1
        default:
            jp87 = 4
        }
        jp85 = jp87
    case "one":
        var jp89 int32
        switch x64 {
        case 0:
            jp89 = 2
        case 1:
            jp89 = 3
        default:
            jp89 = 5
        }
        jp85 = jp89
    default:
        var jp91 int32
        switch x64 {
        case 0:
            jp91 = 2
        default:
            jp91 = 5
        }
        jp85 = jp91
    }
    retv83 = jp85
    return retv83
}

func match_mixed_enum(value__1 Mixed) int32 {
    var retv93 int32
    var jp95 int32
    switch value__1.(type) {
    case OnlyInt:
        var x66 int32 = value__1.(OnlyInt)._0
        var jp97 int32
        switch x66 {
        case 0:
            jp97 = 6
        default:
            jp97 = 7
        }
        jp95 = jp97
    case OnlyStr:
        var x67 string = value__1.(OnlyStr)._0
        var jp99 int32
        switch x67 {
        case "zero":
            jp99 = 8
        default:
            jp99 = 9
        }
        jp95 = jp99
    case Both:
        var x68 int32 = value__1.(Both)._0
        var x69 string = value__1.(Both)._1
        var jp101 int32
        switch x69 {
        case "zero":
            var jp103 int32
            switch x68 {
            case 0:
                jp103 = 10
            default:
                jp103 = 12
            }
            jp101 = jp103
        default:
            var jp105 int32
            switch x68 {
            case 0:
                jp105 = 11
            default:
                jp105 = 13
            }
            jp101 = jp105
        }
        jp95 = jp101
    default:
        panic("non-exhaustive match")
    }
    retv93 = jp95
    return retv93
}

func main0() struct{} {
    var t107 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t108 int32 = match_mixed_pair(t107)
    var t109 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t108)
    println__T_string(t109)
    var t110 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t111 int32 = match_mixed_pair(t110)
    var t112 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t111)
    println__T_string(t112)
    var t113 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t114 int32 = match_mixed_pair(t113)
    var t115 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t114)
    println__T_string(t115)
    var t116 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t117 int32 = match_mixed_pair(t116)
    var t118 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t117)
    println__T_string(t118)
    var t119 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t120 int32 = match_mixed_pair(t119)
    var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t120)
    println__T_string(t121)
    var t122 Mixed = OnlyInt{
        _0: 0,
    }
    var t123 int32 = match_mixed_enum(t122)
    var t124 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t123)
    println__T_string(t124)
    var t125 Mixed = OnlyInt{
        _0: 5,
    }
    var t126 int32 = match_mixed_enum(t125)
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t126)
    println__T_string(t127)
    var t128 Mixed = OnlyStr{
        _0: "zero",
    }
    var t129 int32 = match_mixed_enum(t128)
    var t130 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t129)
    println__T_string(t130)
    var t131 Mixed = OnlyStr{
        _0: "hello",
    }
    var t132 int32 = match_mixed_enum(t131)
    var t133 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t132)
    println__T_string(t133)
    var t134 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t135 int32 = match_mixed_enum(t134)
    var t136 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t135)
    println__T_string(t136)
    var t137 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t138 int32 = match_mixed_enum(t137)
    var t139 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t138)
    println__T_string(t139)
    var t140 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t141 int32 = match_mixed_enum(t140)
    var t142 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t141)
    println__T_string(t142)
    var t143 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t144 int32 = match_mixed_enum(t143)
    var t145 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t144)
    println__T_string(t145)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t148 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t148)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv151 string
    var t152 string = _goml_runtime_core_int32_to_string(self__6)
    retv151 = t152
    return retv151
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv154 string
    retv154 = self__38
    return retv154
}

func main() {
    main0()
}
