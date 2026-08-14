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
    var x187 int32 = pair__0._0
    var x188 string = pair__0._1
    switch x188 {
    case "zero":
        switch x187 {
        case 0:
            return 1
        default:
            return 4
        }
    case "one":
        switch x187 {
        case 0:
            return 2
        case 1:
            return 3
        default:
            return 5
        }
    default:
        switch x187 {
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
        var x189 int32 = value__1.(OnlyInt)._0
        switch x189 {
        case 0:
            return 6
        default:
            return 7
        }
    case OnlyStr:
        var x190 string = value__1.(OnlyStr)._0
        switch x190 {
        case "zero":
            return 8
        default:
            return 9
        }
    case Both:
        var x191 int32 = value__1.(Both)._0
        var x192 string = value__1.(Both)._1
        switch x192 {
        case "zero":
            switch x191 {
            case 0:
                return 10
            default:
                return 12
            }
        default:
            switch x191 {
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
    var t230 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t231 int32 = match_mixed_pair(t230)
    var t232 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t231)
    println__T_string(t232)
    var t233 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t234 int32 = match_mixed_pair(t233)
    var t235 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t234)
    println__T_string(t235)
    var t236 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t237 int32 = match_mixed_pair(t236)
    var t238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t237)
    println__T_string(t238)
    var t239 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t240 int32 = match_mixed_pair(t239)
    var t241 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t240)
    var inline324 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t241)
    _goml_runtime_core_string_println(inline324)
    var t242 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t243 int32 = match_mixed_pair(t242)
    var t244 string
    var inline322 string = _goml_runtime_core_int32_to_string(t243)
    t244 = inline322
    var inline319 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
    _goml_runtime_core_string_println(inline319)
    var t245 Mixed = OnlyInt{
        _0: 0,
    }
    var t246 int32 = match_mixed_enum(t245)
    var t247 string
    var inline317 string = _goml_runtime_core_int32_to_string(t246)
    t247 = inline317
    var inline314 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t247)
    _goml_runtime_core_string_println(inline314)
    var t248 Mixed = OnlyInt{
        _0: 5,
    }
    var t249 int32 = match_mixed_enum(t248)
    var t250 string
    var inline312 string = _goml_runtime_core_int32_to_string(t249)
    t250 = inline312
    var inline309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t250)
    _goml_runtime_core_string_println(inline309)
    var t251 Mixed = OnlyStr{
        _0: "zero",
    }
    var t252 int32 = match_mixed_enum(t251)
    var t253 string
    var inline307 string = _goml_runtime_core_int32_to_string(t252)
    t253 = inline307
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t253)
    _goml_runtime_core_string_println(inline304)
    var t254 Mixed = OnlyStr{
        _0: "hello",
    }
    var t255 int32 = match_mixed_enum(t254)
    var t256 string
    var inline302 string = _goml_runtime_core_int32_to_string(t255)
    t256 = inline302
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t256)
    _goml_runtime_core_string_println(inline299)
    var t257 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t258 int32 = match_mixed_enum(t257)
    var t259 string
    var inline297 string = _goml_runtime_core_int32_to_string(t258)
    t259 = inline297
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t259)
    _goml_runtime_core_string_println(inline294)
    var t260 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t261 int32 = match_mixed_enum(t260)
    var t262 string
    var inline292 string = _goml_runtime_core_int32_to_string(t261)
    t262 = inline292
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t262)
    _goml_runtime_core_string_println(inline289)
    var t263 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t264 int32 = match_mixed_enum(t263)
    var t265 string
    var inline287 string = _goml_runtime_core_int32_to_string(t264)
    t265 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t265)
    _goml_runtime_core_string_println(inline284)
    var t266 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t267 int32 = match_mixed_enum(t266)
    var t268 string
    var inline282 string = _goml_runtime_core_int32_to_string(t267)
    t268 = inline282
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t268)
    _goml_runtime_core_string_println(inline279)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t271 string
    t271 = value__1
    _goml_runtime_core_string_println(t271)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t275 string = _goml_runtime_core_int32_to_string(self__33)
    return t275
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
