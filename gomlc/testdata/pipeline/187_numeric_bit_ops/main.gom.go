package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint16_to_string(x uint16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint32_to_string(x uint32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint64_to_string(x uint64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Option__char interface {
    isOption__char()
}

type None struct {}

func (_ None) isOption__char() {}

type Some struct {
    _0 rune
}

func (_ Some) isOption__char() {}

func show_u8(value__0 uint8) struct{} {
    var inline404 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__0)
    _goml_runtime_core_string_println(inline404)
    return struct{}{}
}

func unsigned_ops() struct{} {
    var t221_lhs uint8 = 13
    var t221_rhs uint8 = 5
    var t221 uint8 = t221_lhs % t221_rhs
    show_u8(t221)
    var t222_lhs uint8 = 12
    var t222_rhs uint8 = 10
    var t222 uint8 = t222_lhs & t222_rhs
    show_u8(t222)
    var t223_lhs uint8 = 12
    var t223_rhs uint8 = 3
    var t223 uint8 = t223_lhs | t223_rhs
    show_u8(t223)
    var t224_lhs uint8 = 12
    var t224_rhs uint8 = 10
    var t224 uint8 = t224_lhs ^ t224_rhs
    show_u8(t224)
    var t225_lhs uint8 = 1
    var t225_rhs int = 7
    var t225 uint8 = t225_lhs << t225_rhs
    show_u8(t225)
    var t226_lhs uint8 = 128
    var t226_rhs int = 7
    var t226 uint8 = t226_lhs >> t226_rhs
    println__T_uint8(t226)
    var t227_operand uint8 = 0
    var t227 uint8 = ^t227_operand
    println__T_uint8(t227)
    var t228_lhs uint8 = 1
    var t228_rhs int = 8
    var t228 uint8 = t228_lhs << t228_rhs
    println__T_uint8(t228)
    var t229_lhs uint16 = 513
    var t229_rhs uint16 = 256
    var t229 uint16 = t229_lhs % t229_rhs
    println__T_uint16(t229)
    var t230_lhs uint16 = 3855
    var t230_rhs uint16 = 255
    var t230 uint16 = t230_lhs & t230_rhs
    println__T_uint16(t230)
    var t231_lhs uint16 = 3840
    var t231_rhs uint16 = 15
    var t231 uint16 = t231_lhs | t231_rhs
    println__T_uint16(t231)
    var t232_lhs uint16 = 43690
    var t232_rhs uint16 = 3855
    var t232 uint16 = t232_lhs ^ t232_rhs
    println__T_uint16(t232)
    var t233_lhs uint16 = 1
    var t233_rhs int = 15
    var t233 uint16 = t233_lhs << t233_rhs
    println__T_uint16(t233)
    var t234_lhs uint16 = 32768
    var t234_rhs int = 15
    var t234 uint16 = t234_lhs >> t234_rhs
    println__T_uint16(t234)
    var t235_operand uint16 = 0
    var t235 uint16 = ^t235_operand
    println__T_uint16(t235)
    var t236_lhs uint32 = 1000000001
    var t236_rhs uint32 = 1000
    var t236 uint32 = t236_lhs % t236_rhs
    println__T_uint32(t236)
    var t237_lhs uint32 = 4042322160
    var t237_rhs uint32 = 252645135
    var t237 uint32 = t237_lhs & t237_rhs
    println__T_uint32(t237)
    var t238_lhs uint32 = 4042322160
    var t238_rhs uint32 = 252645135
    var t238 uint32 = t238_lhs | t238_rhs
    println__T_uint32(t238)
    var t239_lhs uint32 = 4042322160
    var t239_rhs uint32 = 252645135
    var t239 uint32 = t239_lhs ^ t239_rhs
    println__T_uint32(t239)
    var t240_lhs uint32 = 1
    var t240_rhs int = 31
    var t240 uint32 = t240_lhs << t240_rhs
    println__T_uint32(t240)
    var t241_lhs uint32 = 2147483648
    var t241_rhs int = 31
    var t241 uint32 = t241_lhs >> t241_rhs
    println__T_uint32(t241)
    var t242_operand uint32 = 0
    var t242 uint32 = ^t242_operand
    println__T_uint32(t242)
    var t243_lhs uint64 = 1000000000001
    var t243_rhs uint64 = 1000
    var t243 uint64 = t243_lhs % t243_rhs
    println__T_uint64(t243)
    var t244_lhs uint64 = 17361641481138401520
    var t244_rhs uint64 = 1085102592571150095
    var t244 uint64 = t244_lhs & t244_rhs
    println__T_uint64(t244)
    var t245_lhs uint64 = 17361641481138401520
    var t245_rhs uint64 = 1085102592571150095
    var t245 uint64 = t245_lhs | t245_rhs
    println__T_uint64(t245)
    var t246_lhs uint64 = 17361641481138401520
    var t246_rhs uint64 = 1085102592571150095
    var t246 uint64 = t246_lhs ^ t246_rhs
    println__T_uint64(t246)
    var t247_lhs uint64 = 1
    var t247_rhs int = 63
    var t247 uint64 = t247_lhs << t247_rhs
    println__T_uint64(t247)
    var t248_lhs uint64 = 9223372036854775808
    var t248_rhs int = 63
    var t248 uint64 = t248_lhs >> t248_rhs
    println__T_uint64(t248)
    var t249_operand uint64 = 0
    var t249 uint64 = ^t249_operand
    println__T_uint64(t249)
    return struct{}{}
}

func signed_ops() struct{} {
    var t252_lhs int8 = -13
    var t252_rhs int8 = 5
    var t252 int8 = t252_lhs % t252_rhs
    println__T_int8(t252)
    var t253_lhs int8 = -8
    var t253_rhs int = 2
    var t253 int8 = t253_lhs >> t253_rhs
    println__T_int8(t253)
    var t254_lhs int8 = 1
    var t254_rhs int = 6
    var t254 int8 = t254_lhs << t254_rhs
    println__T_int8(t254)
    var t255_operand int8 = 0
    var t255 int8 = ^t255_operand
    println__T_int8(t255)
    var t256_lhs int8 = -1
    var t256_rhs int = 7
    var t256 int8 = t256_lhs >> t256_rhs
    println__T_int8(t256)
    var t257_lhs int16 = -513
    var t257_rhs int16 = 256
    var t257 int16 = t257_lhs % t257_rhs
    println__T_int16(t257)
    var t258 int16 = -32767 - 1
    var t259_rhs int = 15
    var t259 int16 = t258 >> t259_rhs
    println__T_int16(t259)
    var t260_lhs int16 = 1
    var t260_rhs int = 14
    var t260 int16 = t260_lhs << t260_rhs
    println__T_int16(t260)
    var t261_operand int16 = 255
    var t261 int16 = ^t261_operand
    println__T_int16(t261)
    var t262_lhs int32 = -1000000001
    var t262_rhs int32 = 1000
    var t262 int32 = t262_lhs % t262_rhs
    println__T_int32(t262)
    var t263 int32 = -2147483647 - 1
    var t264_rhs int = 31
    var t264 int32 = t263 >> t264_rhs
    println__T_int32(t264)
    var t265_lhs int32 = 1
    var t265_rhs int = 30
    var t265 int32 = t265_lhs << t265_rhs
    println__T_int32(t265)
    var t266_operand int32 = 65535
    var t266 int32 = ^t266_operand
    println__T_int32(t266)
    var t267_lhs int64 = -1000000000001
    var t267_rhs int64 = 1000
    var t267 int64 = t267_lhs % t267_rhs
    println__T_int64(t267)
    var t268_lhs int64 = -9223372036854775807
    var t268_rhs int = 62
    var t268 int64 = t268_lhs >> t268_rhs
    println__T_int64(t268)
    var t269_lhs int64 = 1
    var t269_rhs int = 62
    var t269 int64 = t269_lhs << t269_rhs
    println__T_int64(t269)
    var t270_operand int64 = 4294967295
    var t270 int64 = ^t270_operand
    println__T_int64(t270)
    return struct{}{}
}

func precedence() struct{} {
    var t273_lhs uint8 = 3
    var t273_rhs uint8 = 1
    var t273 uint8 = t273_lhs & t273_rhs
    var t274_lhs uint8 = 2
    var t274 uint8 = t274_lhs ^ t273
    var t275_lhs uint8 = 1
    var t275 uint8 = t275_lhs | t274
    println__T_uint8(t275)
    var t276 int = 2 + 1
    var t277_lhs uint8 = 1
    var t277 uint8 = t277_lhs << t276
    println__T_uint8(t277)
    var t278_lhs int = 1
    var t278_rhs int = 2
    var t278 int = t278_lhs | t278_rhs
    var t279 bool
    var inline522 int = 3
    var inline523 bool = t278 == inline522
    t279 = inline523
    var t280 string
    var inline520 string = _goml_runtime_core_bool_to_string(t279)
    t280 = inline520
    var inline517 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t280)
    _goml_runtime_core_string_println(inline517)
    var t281_lhs int = 8
    var t281_rhs int = 1
    var t281 int = t281_lhs >> t281_rhs
    var t282 bool = t281 < 5
    var t283 string
    var inline515 string = _goml_runtime_core_bool_to_string(t282)
    t283 = inline515
    var inline512 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t283)
    _goml_runtime_core_string_println(inline512)
    var t284_operand uint8 = 1
    var t284 uint8 = ^t284_operand
    var t285_rhs uint8 = 15
    var t285 uint8 = t284 & t285_rhs
    println__T_uint8(t285)
    return struct{}{}
}

func casts() struct{} {
    var five_eleven__8 uint16 = 511
    var two_fifty_six__9 uint16 = 256
    var negative_one_i16__10 int16 = -1
    var two_fifty_five__11 uint8 = 255
    var one_twenty_eight__12 uint8 = 128
    var negative_one_twenty_nine__13 int16 = -129
    var max_u16__14 uint16 = 65535
    var negative_one_i32__15 int32 = -1
    var negative_one_i8__16 int8 = -1
    var max_u64__17 uint64 = 18446744073709551615
    var sixty_five__18 uint8 = 65
    var max_u32__19 uint32 = 4294967295
    var three_hundred__20 uint16 = 300
    var t288 uint8 = uint8(uint16(five_eleven__8))
    println__T_uint8(t288)
    var t289 uint8 = uint8(uint16(two_fifty_six__9))
    println__T_uint8(t289)
    var t290 uint8 = uint8(int16(negative_one_i16__10))
    println__T_uint8(t290)
    var t291 int8 = int8(uint8(two_fifty_five__11))
    println__T_int8(t291)
    var t292 int8 = int8(uint8(one_twenty_eight__12))
    println__T_int8(t292)
    var t293 int8 = int8(int16(negative_one_twenty_nine__13))
    println__T_int8(t293)
    var t294 int16 = int16(uint16(max_u16__14))
    println__T_int16(t294)
    var t295 uint16 = uint16(int32(negative_one_i32__15))
    println__T_uint16(t295)
    var t296 uint64 = uint64(int8(negative_one_i8__16))
    println__T_uint64(t296)
    var t297 int32 = int32(uint64(max_u64__17))
    println__T_int32(t297)
    var t298 uint32 = uint32(uint8(sixty_five__18))
    println__T_uint32(t298)
    var t299 int64 = int64(uint32(max_u32__19))
    println__T_int64(t299)
    var t300_source rune = 65
    var t300 uint32 = uint32(rune(t300_source))
    println__T_uint32(t300)
    var mtmp197 Option__char
    var inline540 uint32 = 128512
    var inline541 bool = utf8_valid_scalar(inline540)
    if inline541 {
        var inline542 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(inline540)
        var inline544 rune = inline542._1
        var inline546 Option__char = Some{
            _0: inline544,
        }
        mtmp197 = inline546
    } else {
        mtmp197 = None{}
    }
    switch mtmp197.(type) {
    case None:
        var inline529 string = "invalid"
        var inline530 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline529)
        _goml_runtime_core_string_println(inline530)
    case Some:
        var x198 rune = mtmp197.(Some)._0
        var t306 string
        var inline536 string = char_to_string(x198)
        t306 = inline536
        var inline533 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t306)
        _goml_runtime_core_string_println(inline533)
    default:
        panic("non-exhaustive match")
    }
    var t302 uint8 = uint8(uint16(three_hundred__20))
    var t303 uint32 = uint32(uint8(t302))
    println__T_uint32(t303)
    return struct{}{}
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t316 uint8
    var inline576 uint8 = 10
    var inline577 uint8 = ^inline576
    var inline578_rhs uint8 = 15
    var inline578 uint8 = inline577 & inline578_rhs
    var inline579_lhs uint8 = 1
    var inline579_rhs int = 4
    var inline579 uint8 = inline579_lhs << inline579_rhs
    var inline580_rhs uint8 = 31
    var inline580 uint8 = inline579 % inline580_rhs
    var inline581 uint8 = inline578 | inline580
    t316 = inline581
    println__T_uint8(t316)
    return struct{}{}
}

func println__T_uint8(value__31 uint8) struct{} {
    var t319 string
    var inline583 string = _goml_runtime_core_uint8_to_string(value__31)
    t319 = inline583
    _goml_runtime_core_string_println(t319)
    return struct{}{}
}

func println__T_uint16(value__31 uint16) struct{} {
    var t322 string
    var inline585 string = _goml_runtime_core_uint16_to_string(value__31)
    t322 = inline585
    _goml_runtime_core_string_println(t322)
    return struct{}{}
}

func println__T_uint32(value__31 uint32) struct{} {
    var t325 string
    var inline587 string = _goml_runtime_core_uint32_to_string(value__31)
    t325 = inline587
    _goml_runtime_core_string_println(t325)
    return struct{}{}
}

func println__T_uint64(value__31 uint64) struct{} {
    var t328 string
    var inline589 string = _goml_runtime_core_uint64_to_string(value__31)
    t328 = inline589
    _goml_runtime_core_string_println(t328)
    return struct{}{}
}

func println__T_int8(value__31 int8) struct{} {
    var t331 string
    var inline591 string = _goml_runtime_core_int8_to_string(value__31)
    t331 = inline591
    _goml_runtime_core_string_println(t331)
    return struct{}{}
}

func println__T_int16(value__31 int16) struct{} {
    var t334 string
    var inline593 string = _goml_runtime_core_int16_to_string(value__31)
    t334 = inline593
    _goml_runtime_core_string_println(t334)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t337 string
    var inline595 string = _goml_runtime_core_int32_to_string(value__31)
    t337 = inline595
    _goml_runtime_core_string_println(t337)
    return struct{}{}
}

func println__T_int64(value__31 int64) struct{} {
    var t340 string
    var inline597 string = _goml_runtime_core_int64_to_string(value__31)
    t340 = inline597
    _goml_runtime_core_string_println(t340)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__74 uint8) string {
    var t362 string = _goml_runtime_core_uint8_to_string(self__74)
    return t362
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t390 bool = value__4 <= 1114111
    if t390 {
        var t394 bool = value__4 >= 55296
        var jp392 bool
        if t394 {
            var t395 bool = value__4 <= 57343
            jp392 = t395
        } else {
            jp392 = false
        }
        var t393 bool = !jp392
        return t393
    } else {
        return false
    }
}

func char_to_string(value__29 rune) string {
    var t400 uint32 = uint32(rune(value__29))
    var t401 bool
    var inline612 bool = t400 <= 1114111
    if inline612 {
        var inline613 bool = t400 >= 55296
        var inline615 bool
        if inline613 {
            var inline617 bool = t400 <= 57343
            inline615 = inline617
        } else {
            inline615 = false
        }
        var inline616 bool = !inline615
        t401 = inline616
    } else {
        t401 = false
    }
    if t401 {
        var t402 string = _goml_runtime_core_char_to_string(value__29)
        return t402
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
