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
    var retv127 int32
    var x108 int32 = pair__0._0
    var x109 string = pair__0._1
    var jp129 int32
    switch x109 {
    case "zero":
        var jp131 int32
        switch x108 {
        case 0:
            jp131 = 1
        default:
            jp131 = 4
        }
        jp129 = jp131
    case "one":
        var jp133 int32
        switch x108 {
        case 0:
            jp133 = 2
        case 1:
            jp133 = 3
        default:
            jp133 = 5
        }
        jp129 = jp133
    default:
        var jp135 int32
        switch x108 {
        case 0:
            jp135 = 2
        default:
            jp135 = 5
        }
        jp129 = jp135
    }
    retv127 = jp129
    return retv127
}

func match_mixed_enum(value__1 Mixed) int32 {
    var retv137 int32
    var jp139 int32
    switch value__1.(type) {
    case OnlyInt:
        var x110 int32 = value__1.(OnlyInt)._0
        var jp141 int32
        switch x110 {
        case 0:
            jp141 = 6
        default:
            jp141 = 7
        }
        jp139 = jp141
    case OnlyStr:
        var x111 string = value__1.(OnlyStr)._0
        var jp143 int32
        switch x111 {
        case "zero":
            jp143 = 8
        default:
            jp143 = 9
        }
        jp139 = jp143
    case Both:
        var x112 int32 = value__1.(Both)._0
        var x113 string = value__1.(Both)._1
        var jp145 int32
        switch x113 {
        case "zero":
            var jp147 int32
            switch x112 {
            case 0:
                jp147 = 10
            default:
                jp147 = 12
            }
            jp145 = jp147
        default:
            var jp149 int32
            switch x112 {
            case 0:
                jp149 = 11
            default:
                jp149 = 13
            }
            jp145 = jp149
        }
        jp139 = jp145
    default:
        panic("non-exhaustive match")
    }
    retv137 = jp139
    return retv137
}

func main0() struct{} {
    var t151 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t152 int32 = match_mixed_pair(t151)
    var t153 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t152)
    println__T_string(t153)
    var t154 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t155 int32 = match_mixed_pair(t154)
    var t156 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t155)
    println__T_string(t156)
    var t157 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t158 int32 = match_mixed_pair(t157)
    var t159 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t158)
    println__T_string(t159)
    var t160 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t161 int32 = match_mixed_pair(t160)
    var t162 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t161)
    println__T_string(t162)
    var t163 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t164 int32 = match_mixed_pair(t163)
    var t165 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t164)
    println__T_string(t165)
    var t166 Mixed = OnlyInt{
        _0: 0,
    }
    var t167 int32 = match_mixed_enum(t166)
    var t168 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t167)
    println__T_string(t168)
    var t169 Mixed = OnlyInt{
        _0: 5,
    }
    var t170 int32 = match_mixed_enum(t169)
    var t171 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t170)
    println__T_string(t171)
    var t172 Mixed = OnlyStr{
        _0: "zero",
    }
    var t173 int32 = match_mixed_enum(t172)
    var t174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t173)
    println__T_string(t174)
    var t175 Mixed = OnlyStr{
        _0: "hello",
    }
    var t176 int32 = match_mixed_enum(t175)
    var t177 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t176)
    println__T_string(t177)
    var t178 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t179 int32 = match_mixed_enum(t178)
    var t180 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t179)
    println__T_string(t180)
    var t181 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t182 int32 = match_mixed_enum(t181)
    var t183 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t182)
    println__T_string(t183)
    var t184 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t185 int32 = match_mixed_enum(t184)
    var t186 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t185)
    println__T_string(t186)
    var t187 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t188 int32 = match_mixed_enum(t187)
    var t189 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t188)
    println__T_string(t189)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t192)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv195 string
    var t196 string = _goml_runtime_core_int32_to_string(self__6)
    retv195 = t196
    return retv195
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv198 string
    retv198 = self__38
    return retv198
}

func main() {
    main0()
}
