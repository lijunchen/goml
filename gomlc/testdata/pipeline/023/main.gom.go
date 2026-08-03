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
    var x177 int32 = pair__0._0
    var x178 string = pair__0._1
    switch x178 {
    case "zero":
        switch x177 {
        case 0:
            return 1
        default:
            return 4
        }
    case "one":
        switch x177 {
        case 0:
            return 2
        case 1:
            return 3
        default:
            return 5
        }
    default:
        switch x177 {
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
        var x179 int32 = value__1.(OnlyInt)._0
        switch x179 {
        case 0:
            return 6
        default:
            return 7
        }
    case OnlyStr:
        var x180 string = value__1.(OnlyStr)._0
        switch x180 {
        case "zero":
            return 8
        default:
            return 9
        }
    case Both:
        var x181 int32 = value__1.(Both)._0
        var x182 string = value__1.(Both)._1
        switch x182 {
        case "zero":
            switch x181 {
            case 0:
                return 10
            default:
                return 12
            }
        default:
            switch x181 {
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
    var t220 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t221 int32 = match_mixed_pair(t220)
    var t222 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t221)
    println__T_string(t222)
    var t223 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t224 int32 = match_mixed_pair(t223)
    var t225 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t224)
    println__T_string(t225)
    var t226 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t227 int32 = match_mixed_pair(t226)
    var t228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t227)
    println__T_string(t228)
    var t229 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t230 int32 = match_mixed_pair(t229)
    var t231 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t230)
    var inline314 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t231)
    _goml_runtime_core_string_println(inline314)
    var t232 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t233 int32 = match_mixed_pair(t232)
    var t234 string
    var inline312 string = _goml_runtime_core_int32_to_string(t233)
    t234 = inline312
    var inline309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
    _goml_runtime_core_string_println(inline309)
    var t235 Mixed = OnlyInt{
        _0: 0,
    }
    var t236 int32 = match_mixed_enum(t235)
    var t237 string
    var inline307 string = _goml_runtime_core_int32_to_string(t236)
    t237 = inline307
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t237)
    _goml_runtime_core_string_println(inline304)
    var t238 Mixed = OnlyInt{
        _0: 5,
    }
    var t239 int32 = match_mixed_enum(t238)
    var t240 string
    var inline302 string = _goml_runtime_core_int32_to_string(t239)
    t240 = inline302
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t240)
    _goml_runtime_core_string_println(inline299)
    var t241 Mixed = OnlyStr{
        _0: "zero",
    }
    var t242 int32 = match_mixed_enum(t241)
    var t243 string
    var inline297 string = _goml_runtime_core_int32_to_string(t242)
    t243 = inline297
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t243)
    _goml_runtime_core_string_println(inline294)
    var t244 Mixed = OnlyStr{
        _0: "hello",
    }
    var t245 int32 = match_mixed_enum(t244)
    var t246 string
    var inline292 string = _goml_runtime_core_int32_to_string(t245)
    t246 = inline292
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t246)
    _goml_runtime_core_string_println(inline289)
    var t247 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t248 int32 = match_mixed_enum(t247)
    var t249 string
    var inline287 string = _goml_runtime_core_int32_to_string(t248)
    t249 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t249)
    _goml_runtime_core_string_println(inline284)
    var t250 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t251 int32 = match_mixed_enum(t250)
    var t252 string
    var inline282 string = _goml_runtime_core_int32_to_string(t251)
    t252 = inline282
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t252)
    _goml_runtime_core_string_println(inline279)
    var t253 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t254 int32 = match_mixed_enum(t253)
    var t255 string
    var inline277 string = _goml_runtime_core_int32_to_string(t254)
    t255 = inline277
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t255)
    _goml_runtime_core_string_println(inline274)
    var t256 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t257 int32 = match_mixed_enum(t256)
    var t258 string
    var inline272 string = _goml_runtime_core_int32_to_string(t257)
    t258 = inline272
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t258)
    _goml_runtime_core_string_println(inline269)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t261 string
    t261 = value__31
    _goml_runtime_core_string_println(t261)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t265 string = _goml_runtime_core_int32_to_string(self__35)
    return t265
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
