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
    var x172 int32 = pair__0._0
    var x173 string = pair__0._1
    switch x173 {
    case "zero":
        switch x172 {
        case 0:
            return 1
        default:
            return 4
        }
    case "one":
        switch x172 {
        case 0:
            return 2
        case 1:
            return 3
        default:
            return 5
        }
    default:
        switch x172 {
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
        var x174 int32 = value__1.(OnlyInt)._0
        switch x174 {
        case 0:
            return 6
        default:
            return 7
        }
    case OnlyStr:
        var x175 string = value__1.(OnlyStr)._0
        switch x175 {
        case "zero":
            return 8
        default:
            return 9
        }
    case Both:
        var x176 int32 = value__1.(Both)._0
        var x177 string = value__1.(Both)._1
        switch x177 {
        case "zero":
            switch x176 {
            case 0:
                return 10
            default:
                return 12
            }
        default:
            switch x176 {
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
    var t215 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t216 int32 = match_mixed_pair(t215)
    var t217 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t216)
    println__T_string(t217)
    var t218 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t219 int32 = match_mixed_pair(t218)
    var t220 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t219)
    println__T_string(t220)
    var t221 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t222 int32 = match_mixed_pair(t221)
    var t223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t222)
    println__T_string(t223)
    var t224 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t225 int32 = match_mixed_pair(t224)
    var t226 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t225)
    var inline309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline309)
    var t227 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t228 int32 = match_mixed_pair(t227)
    var t229 string
    var inline307 string = _goml_runtime_core_int32_to_string(t228)
    t229 = inline307
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t229)
    _goml_runtime_core_string_println(inline304)
    var t230 Mixed = OnlyInt{
        _0: 0,
    }
    var t231 int32 = match_mixed_enum(t230)
    var t232 string
    var inline302 string = _goml_runtime_core_int32_to_string(t231)
    t232 = inline302
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t232)
    _goml_runtime_core_string_println(inline299)
    var t233 Mixed = OnlyInt{
        _0: 5,
    }
    var t234 int32 = match_mixed_enum(t233)
    var t235 string
    var inline297 string = _goml_runtime_core_int32_to_string(t234)
    t235 = inline297
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t235)
    _goml_runtime_core_string_println(inline294)
    var t236 Mixed = OnlyStr{
        _0: "zero",
    }
    var t237 int32 = match_mixed_enum(t236)
    var t238 string
    var inline292 string = _goml_runtime_core_int32_to_string(t237)
    t238 = inline292
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t238)
    _goml_runtime_core_string_println(inline289)
    var t239 Mixed = OnlyStr{
        _0: "hello",
    }
    var t240 int32 = match_mixed_enum(t239)
    var t241 string
    var inline287 string = _goml_runtime_core_int32_to_string(t240)
    t241 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t241)
    _goml_runtime_core_string_println(inline284)
    var t242 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t243 int32 = match_mixed_enum(t242)
    var t244 string
    var inline282 string = _goml_runtime_core_int32_to_string(t243)
    t244 = inline282
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
    _goml_runtime_core_string_println(inline279)
    var t245 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t246 int32 = match_mixed_enum(t245)
    var t247 string
    var inline277 string = _goml_runtime_core_int32_to_string(t246)
    t247 = inline277
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t247)
    _goml_runtime_core_string_println(inline274)
    var t248 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t249 int32 = match_mixed_enum(t248)
    var t250 string
    var inline272 string = _goml_runtime_core_int32_to_string(t249)
    t250 = inline272
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t250)
    _goml_runtime_core_string_println(inline269)
    var t251 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t252 int32 = match_mixed_enum(t251)
    var t253 string
    var inline267 string = _goml_runtime_core_int32_to_string(t252)
    t253 = inline267
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t253)
    _goml_runtime_core_string_println(inline264)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t256 string
    t256 = value__31
    _goml_runtime_core_string_println(t256)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t260 string = _goml_runtime_core_int32_to_string(self__35)
    return t260
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
