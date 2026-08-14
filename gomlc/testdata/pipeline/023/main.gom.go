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
    var x182 int32 = pair__0._0
    var x183 string = pair__0._1
    switch x183 {
    case "zero":
        switch x182 {
        case 0:
            return 1
        default:
            return 4
        }
    case "one":
        switch x182 {
        case 0:
            return 2
        case 1:
            return 3
        default:
            return 5
        }
    default:
        switch x182 {
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
        var x184 int32 = value__1.(OnlyInt)._0
        switch x184 {
        case 0:
            return 6
        default:
            return 7
        }
    case OnlyStr:
        var x185 string = value__1.(OnlyStr)._0
        switch x185 {
        case "zero":
            return 8
        default:
            return 9
        }
    case Both:
        var x186 int32 = value__1.(Both)._0
        var x187 string = value__1.(Both)._1
        switch x187 {
        case "zero":
            switch x186 {
            case 0:
                return 10
            default:
                return 12
            }
        default:
            switch x186 {
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
    var t225 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t226 int32 = match_mixed_pair(t225)
    var t227 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t226)
    println__T_string(t227)
    var t228 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t229 int32 = match_mixed_pair(t228)
    var t230 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t229)
    println__T_string(t230)
    var t231 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t232 int32 = match_mixed_pair(t231)
    var t233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t232)
    println__T_string(t233)
    var t234 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t235 int32 = match_mixed_pair(t234)
    var t236 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t235)
    var inline319 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
    _goml_runtime_core_string_println(inline319)
    var t237 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t238 int32 = match_mixed_pair(t237)
    var t239 string
    var inline317 string = _goml_runtime_core_int32_to_string(t238)
    t239 = inline317
    var inline314 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
    _goml_runtime_core_string_println(inline314)
    var t240 Mixed = OnlyInt{
        _0: 0,
    }
    var t241 int32 = match_mixed_enum(t240)
    var t242 string
    var inline312 string = _goml_runtime_core_int32_to_string(t241)
    t242 = inline312
    var inline309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t242)
    _goml_runtime_core_string_println(inline309)
    var t243 Mixed = OnlyInt{
        _0: 5,
    }
    var t244 int32 = match_mixed_enum(t243)
    var t245 string
    var inline307 string = _goml_runtime_core_int32_to_string(t244)
    t245 = inline307
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t245)
    _goml_runtime_core_string_println(inline304)
    var t246 Mixed = OnlyStr{
        _0: "zero",
    }
    var t247 int32 = match_mixed_enum(t246)
    var t248 string
    var inline302 string = _goml_runtime_core_int32_to_string(t247)
    t248 = inline302
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t248)
    _goml_runtime_core_string_println(inline299)
    var t249 Mixed = OnlyStr{
        _0: "hello",
    }
    var t250 int32 = match_mixed_enum(t249)
    var t251 string
    var inline297 string = _goml_runtime_core_int32_to_string(t250)
    t251 = inline297
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t251)
    _goml_runtime_core_string_println(inline294)
    var t252 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t253 int32 = match_mixed_enum(t252)
    var t254 string
    var inline292 string = _goml_runtime_core_int32_to_string(t253)
    t254 = inline292
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t254)
    _goml_runtime_core_string_println(inline289)
    var t255 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t256 int32 = match_mixed_enum(t255)
    var t257 string
    var inline287 string = _goml_runtime_core_int32_to_string(t256)
    t257 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t257)
    _goml_runtime_core_string_println(inline284)
    var t258 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t259 int32 = match_mixed_enum(t258)
    var t260 string
    var inline282 string = _goml_runtime_core_int32_to_string(t259)
    t260 = inline282
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t260)
    _goml_runtime_core_string_println(inline279)
    var t261 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t262 int32 = match_mixed_enum(t261)
    var t263 string
    var inline277 string = _goml_runtime_core_int32_to_string(t262)
    t263 = inline277
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t263)
    _goml_runtime_core_string_println(inline274)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t266 string
    t266 = value__1
    _goml_runtime_core_string_println(t266)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t270 string = _goml_runtime_core_int32_to_string(self__33)
    return t270
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
