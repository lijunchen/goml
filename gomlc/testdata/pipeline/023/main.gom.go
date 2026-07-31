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
    var retv171 int32
    var x152 int32 = pair__0._0
    var x153 string = pair__0._1
    var jp173 int32
    switch x153 {
    case "zero":
        var jp175 int32
        switch x152 {
        case 0:
            jp175 = 1
        default:
            jp175 = 4
        }
        jp173 = jp175
    case "one":
        var jp177 int32
        switch x152 {
        case 0:
            jp177 = 2
        case 1:
            jp177 = 3
        default:
            jp177 = 5
        }
        jp173 = jp177
    default:
        var jp179 int32
        switch x152 {
        case 0:
            jp179 = 2
        default:
            jp179 = 5
        }
        jp173 = jp179
    }
    retv171 = jp173
    return retv171
}

func match_mixed_enum(value__1 Mixed) int32 {
    var retv181 int32
    var jp183 int32
    switch value__1.(type) {
    case OnlyInt:
        var x154 int32 = value__1.(OnlyInt)._0
        var jp185 int32
        switch x154 {
        case 0:
            jp185 = 6
        default:
            jp185 = 7
        }
        jp183 = jp185
    case OnlyStr:
        var x155 string = value__1.(OnlyStr)._0
        var jp187 int32
        switch x155 {
        case "zero":
            jp187 = 8
        default:
            jp187 = 9
        }
        jp183 = jp187
    case Both:
        var x156 int32 = value__1.(Both)._0
        var x157 string = value__1.(Both)._1
        var jp189 int32
        switch x157 {
        case "zero":
            var jp191 int32
            switch x156 {
            case 0:
                jp191 = 10
            default:
                jp191 = 12
            }
            jp189 = jp191
        default:
            var jp193 int32
            switch x156 {
            case 0:
                jp193 = 11
            default:
                jp193 = 13
            }
            jp189 = jp193
        }
        jp183 = jp189
    default:
        panic("non-exhaustive match")
    }
    retv181 = jp183
    return retv181
}

func main0() struct{} {
    var t195 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t196 int32 = match_mixed_pair(t195)
    var t197 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t196)
    println__T_string(t197)
    var t198 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t199 int32 = match_mixed_pair(t198)
    var t200 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t199)
    println__T_string(t200)
    var t201 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t202 int32 = match_mixed_pair(t201)
    var t203 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t202)
    println__T_string(t203)
    var t204 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t205 int32 = match_mixed_pair(t204)
    var t206 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t205)
    println__T_string(t206)
    var t207 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t208 int32 = match_mixed_pair(t207)
    var t209 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t208)
    println__T_string(t209)
    var t210 Mixed = OnlyInt{
        _0: 0,
    }
    var t211 int32 = match_mixed_enum(t210)
    var t212 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t211)
    println__T_string(t212)
    var t213 Mixed = OnlyInt{
        _0: 5,
    }
    var t214 int32 = match_mixed_enum(t213)
    var t215 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t214)
    println__T_string(t215)
    var t216 Mixed = OnlyStr{
        _0: "zero",
    }
    var t217 int32 = match_mixed_enum(t216)
    var t218 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t217)
    println__T_string(t218)
    var t219 Mixed = OnlyStr{
        _0: "hello",
    }
    var t220 int32 = match_mixed_enum(t219)
    var t221 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t220)
    println__T_string(t221)
    var t222 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t223 int32 = match_mixed_enum(t222)
    var t224 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t223)
    println__T_string(t224)
    var t225 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t226 int32 = match_mixed_enum(t225)
    var t227 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t226)
    println__T_string(t227)
    var t228 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t229 int32 = match_mixed_enum(t228)
    var t230 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t229)
    println__T_string(t230)
    var t231 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t232 int32 = match_mixed_enum(t231)
    var t233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t232)
    println__T_string(t233)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t236)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv239 string
    var t240 string = _goml_runtime_core_int32_to_string(self__6)
    retv239 = t240
    return retv239
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv242 string
    retv242 = self__38
    return retv242
}

func main() {
    main0()
}
