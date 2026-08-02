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
    var x155 int32 = pair__0._0
    var x156 string = pair__0._1
    switch x156 {
    case "zero":
        switch x155 {
        case 0:
            return 1
        default:
            return 4
        }
    case "one":
        switch x155 {
        case 0:
            return 2
        case 1:
            return 3
        default:
            return 5
        }
    default:
        switch x155 {
        case 0:
            return 2
        default:
            return 5
        }
    }
}

func match_mixed_enum(value__1 Mixed) int32 {
    switch value__1.(type) {
    case OnlyInt:
        var x157 int32 = value__1.(OnlyInt)._0
        switch x157 {
        case 0:
            return 6
        default:
            return 7
        }
    case OnlyStr:
        var x158 string = value__1.(OnlyStr)._0
        switch x158 {
        case "zero":
            return 8
        default:
            return 9
        }
    case Both:
        var x159 int32 = value__1.(Both)._0
        var x160 string = value__1.(Both)._1
        switch x160 {
        case "zero":
            switch x159 {
            case 0:
                return 10
            default:
                return 12
            }
        default:
            switch x159 {
            case 0:
                return 11
            default:
                return 13
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t198 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t199 int32 = match_mixed_pair(t198)
    var t200 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t199)
    println__T_string(t200)
    var t201 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t202 int32 = match_mixed_pair(t201)
    var t203 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t202)
    println__T_string(t203)
    var t204 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t205 int32 = match_mixed_pair(t204)
    var t206 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t205)
    println__T_string(t206)
    var t207 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t208 int32 = match_mixed_pair(t207)
    var t209 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t208)
    println__T_string(t209)
    var t210 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t211 int32 = match_mixed_pair(t210)
    var t212 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t211)
    println__T_string(t212)
    var t213 Mixed = OnlyInt{
        _0: 0,
    }
    var t214 int32 = match_mixed_enum(t213)
    var t215 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t214)
    println__T_string(t215)
    var t216 Mixed = OnlyInt{
        _0: 5,
    }
    var t217 int32 = match_mixed_enum(t216)
    var t218 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t217)
    println__T_string(t218)
    var t219 Mixed = OnlyStr{
        _0: "zero",
    }
    var t220 int32 = match_mixed_enum(t219)
    var t221 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t220)
    println__T_string(t221)
    var t222 Mixed = OnlyStr{
        _0: "hello",
    }
    var t223 int32 = match_mixed_enum(t222)
    var t224 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t223)
    println__T_string(t224)
    var t225 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t226 int32 = match_mixed_enum(t225)
    var t227 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t226)
    println__T_string(t227)
    var t228 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t229 int32 = match_mixed_enum(t228)
    var t230 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t229)
    println__T_string(t230)
    var t231 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t232 int32 = match_mixed_enum(t231)
    var t233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t232)
    println__T_string(t233)
    var t234 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t235 int32 = match_mixed_enum(t234)
    var t236 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t235)
    println__T_string(t236)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t239)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t243 string = _goml_runtime_core_int32_to_string(self__6)
    return t243
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
