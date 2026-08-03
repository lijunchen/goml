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
    var x136 int32 = pair__0._0
    var x137 string = pair__0._1
    switch x137 {
    case "zero":
        switch x136 {
        case 0:
            return 1
        default:
            return 4
        }
    case "one":
        switch x136 {
        case 0:
            return 2
        case 1:
            return 3
        default:
            return 5
        }
    default:
        switch x136 {
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
        var x138 int32 = value__1.(OnlyInt)._0
        switch x138 {
        case 0:
            return 6
        default:
            return 7
        }
    case OnlyStr:
        var x139 string = value__1.(OnlyStr)._0
        switch x139 {
        case "zero":
            return 8
        default:
            return 9
        }
    case Both:
        var x140 int32 = value__1.(Both)._0
        var x141 string = value__1.(Both)._1
        switch x141 {
        case "zero":
            switch x140 {
            case 0:
                return 10
            default:
                return 12
            }
        default:
            switch x140 {
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
    var t179 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t180 int32 = match_mixed_pair(t179)
    var t181 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t180)
    println__T_string(t181)
    var t182 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t183 int32 = match_mixed_pair(t182)
    var t184 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t183)
    println__T_string(t184)
    var t185 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t186 int32 = match_mixed_pair(t185)
    var t187 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t186)
    println__T_string(t187)
    var t188 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t189 int32 = match_mixed_pair(t188)
    var t190 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t189)
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline273)
    var t191 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t192 int32 = match_mixed_pair(t191)
    var t193 string
    var inline271 string = _goml_runtime_core_int32_to_string(t192)
    t193 = inline271
    var inline268 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline268)
    var t194 Mixed = OnlyInt{
        _0: 0,
    }
    var t195 int32 = match_mixed_enum(t194)
    var t196 string
    var inline266 string = _goml_runtime_core_int32_to_string(t195)
    t196 = inline266
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline263)
    var t197 Mixed = OnlyInt{
        _0: 5,
    }
    var t198 int32 = match_mixed_enum(t197)
    var t199 string
    var inline261 string = _goml_runtime_core_int32_to_string(t198)
    t199 = inline261
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline258)
    var t200 Mixed = OnlyStr{
        _0: "zero",
    }
    var t201 int32 = match_mixed_enum(t200)
    var t202 string
    var inline256 string = _goml_runtime_core_int32_to_string(t201)
    t202 = inline256
    var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline253)
    var t203 Mixed = OnlyStr{
        _0: "hello",
    }
    var t204 int32 = match_mixed_enum(t203)
    var t205 string
    var inline251 string = _goml_runtime_core_int32_to_string(t204)
    t205 = inline251
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline248)
    var t206 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t207 int32 = match_mixed_enum(t206)
    var t208 string
    var inline246 string = _goml_runtime_core_int32_to_string(t207)
    t208 = inline246
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline243)
    var t209 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t210 int32 = match_mixed_enum(t209)
    var t211 string
    var inline241 string = _goml_runtime_core_int32_to_string(t210)
    t211 = inline241
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline238)
    var t212 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t213 int32 = match_mixed_enum(t212)
    var t214 string
    var inline236 string = _goml_runtime_core_int32_to_string(t213)
    t214 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline233)
    var t215 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t216 int32 = match_mixed_enum(t215)
    var t217 string
    var inline231 string = _goml_runtime_core_int32_to_string(t216)
    t217 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline228)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t220 string
    t220 = value__31
    _goml_runtime_core_string_println(t220)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t224 string = _goml_runtime_core_int32_to_string(self__35)
    return t224
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
