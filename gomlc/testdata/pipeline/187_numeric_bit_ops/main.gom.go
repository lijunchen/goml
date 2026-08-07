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
    var inline440 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__0)
    _goml_runtime_core_string_println(inline440)
    return struct{}{}
}

func unsigned_ops() struct{} {
    var t257_lhs uint8 = 13
    var t257_rhs uint8 = 5
    var t257 uint8 = t257_lhs % t257_rhs
    show_u8(t257)
    var t258_lhs uint8 = 12
    var t258_rhs uint8 = 10
    var t258 uint8 = t258_lhs & t258_rhs
    show_u8(t258)
    var t259_lhs uint8 = 12
    var t259_rhs uint8 = 3
    var t259 uint8 = t259_lhs | t259_rhs
    show_u8(t259)
    var t260_lhs uint8 = 12
    var t260_rhs uint8 = 10
    var t260 uint8 = t260_lhs ^ t260_rhs
    show_u8(t260)
    var t261_lhs uint8 = 1
    var t261_rhs int = 7
    var t261 uint8 = t261_lhs << t261_rhs
    show_u8(t261)
    var t262_lhs uint8 = 128
    var t262_rhs int = 7
    var t262 uint8 = t262_lhs >> t262_rhs
    println__T_uint8(t262)
    var t263_operand uint8 = 0
    var t263 uint8 = ^t263_operand
    println__T_uint8(t263)
    var t264_lhs uint8 = 1
    var t264_rhs int = 8
    var t264 uint8 = t264_lhs << t264_rhs
    println__T_uint8(t264)
    var t265_lhs uint16 = 513
    var t265_rhs uint16 = 256
    var t265 uint16 = t265_lhs % t265_rhs
    println__T_uint16(t265)
    var t266_lhs uint16 = 3855
    var t266_rhs uint16 = 255
    var t266 uint16 = t266_lhs & t266_rhs
    println__T_uint16(t266)
    var t267_lhs uint16 = 3840
    var t267_rhs uint16 = 15
    var t267 uint16 = t267_lhs | t267_rhs
    println__T_uint16(t267)
    var t268_lhs uint16 = 43690
    var t268_rhs uint16 = 3855
    var t268 uint16 = t268_lhs ^ t268_rhs
    println__T_uint16(t268)
    var t269_lhs uint16 = 1
    var t269_rhs int = 15
    var t269 uint16 = t269_lhs << t269_rhs
    println__T_uint16(t269)
    var t270_lhs uint16 = 32768
    var t270_rhs int = 15
    var t270 uint16 = t270_lhs >> t270_rhs
    println__T_uint16(t270)
    var t271_operand uint16 = 0
    var t271 uint16 = ^t271_operand
    println__T_uint16(t271)
    var t272_lhs uint32 = 1000000001
    var t272_rhs uint32 = 1000
    var t272 uint32 = t272_lhs % t272_rhs
    println__T_uint32(t272)
    var t273_lhs uint32 = 4042322160
    var t273_rhs uint32 = 252645135
    var t273 uint32 = t273_lhs & t273_rhs
    println__T_uint32(t273)
    var t274_lhs uint32 = 4042322160
    var t274_rhs uint32 = 252645135
    var t274 uint32 = t274_lhs | t274_rhs
    println__T_uint32(t274)
    var t275_lhs uint32 = 4042322160
    var t275_rhs uint32 = 252645135
    var t275 uint32 = t275_lhs ^ t275_rhs
    println__T_uint32(t275)
    var t276_lhs uint32 = 1
    var t276_rhs int = 31
    var t276 uint32 = t276_lhs << t276_rhs
    println__T_uint32(t276)
    var t277_lhs uint32 = 2147483648
    var t277_rhs int = 31
    var t277 uint32 = t277_lhs >> t277_rhs
    println__T_uint32(t277)
    var t278_operand uint32 = 0
    var t278 uint32 = ^t278_operand
    println__T_uint32(t278)
    var t279_lhs uint64 = 1000000000001
    var t279_rhs uint64 = 1000
    var t279 uint64 = t279_lhs % t279_rhs
    println__T_uint64(t279)
    var t280_lhs uint64 = 17361641481138401520
    var t280_rhs uint64 = 1085102592571150095
    var t280 uint64 = t280_lhs & t280_rhs
    println__T_uint64(t280)
    var t281_lhs uint64 = 17361641481138401520
    var t281_rhs uint64 = 1085102592571150095
    var t281 uint64 = t281_lhs | t281_rhs
    println__T_uint64(t281)
    var t282_lhs uint64 = 17361641481138401520
    var t282_rhs uint64 = 1085102592571150095
    var t282 uint64 = t282_lhs ^ t282_rhs
    println__T_uint64(t282)
    var t283_lhs uint64 = 1
    var t283_rhs int = 63
    var t283 uint64 = t283_lhs << t283_rhs
    println__T_uint64(t283)
    var t284_lhs uint64 = 9223372036854775808
    var t284_rhs int = 63
    var t284 uint64 = t284_lhs >> t284_rhs
    println__T_uint64(t284)
    var t285_operand uint64 = 0
    var t285 uint64 = ^t285_operand
    println__T_uint64(t285)
    return struct{}{}
}

func signed_ops() struct{} {
    var t288_lhs int8 = -13
    var t288_rhs int8 = 5
    var t288 int8 = t288_lhs % t288_rhs
    println__T_int8(t288)
    var t289_lhs int8 = -8
    var t289_rhs int = 2
    var t289 int8 = t289_lhs >> t289_rhs
    println__T_int8(t289)
    var t290_lhs int8 = 1
    var t290_rhs int = 6
    var t290 int8 = t290_lhs << t290_rhs
    println__T_int8(t290)
    var t291_operand int8 = 0
    var t291 int8 = ^t291_operand
    println__T_int8(t291)
    var t292_lhs int8 = -1
    var t292_rhs int = 7
    var t292 int8 = t292_lhs >> t292_rhs
    println__T_int8(t292)
    var t293_lhs int16 = -513
    var t293_rhs int16 = 256
    var t293 int16 = t293_lhs % t293_rhs
    println__T_int16(t293)
    var t294 int16 = -32767 - 1
    var t295_rhs int = 15
    var t295 int16 = t294 >> t295_rhs
    println__T_int16(t295)
    var t296_lhs int16 = 1
    var t296_rhs int = 14
    var t296 int16 = t296_lhs << t296_rhs
    println__T_int16(t296)
    var t297_operand int16 = 255
    var t297 int16 = ^t297_operand
    println__T_int16(t297)
    var t298_lhs int32 = -1000000001
    var t298_rhs int32 = 1000
    var t298 int32 = t298_lhs % t298_rhs
    println__T_int32(t298)
    var t299 int32 = -2147483647 - 1
    var t300_rhs int = 31
    var t300 int32 = t299 >> t300_rhs
    println__T_int32(t300)
    var t301_lhs int32 = 1
    var t301_rhs int = 30
    var t301 int32 = t301_lhs << t301_rhs
    println__T_int32(t301)
    var t302_operand int32 = 65535
    var t302 int32 = ^t302_operand
    println__T_int32(t302)
    var t303_lhs int64 = -1000000000001
    var t303_rhs int64 = 1000
    var t303 int64 = t303_lhs % t303_rhs
    println__T_int64(t303)
    var t304_lhs int64 = -9223372036854775807
    var t304_rhs int = 62
    var t304 int64 = t304_lhs >> t304_rhs
    println__T_int64(t304)
    var t305_lhs int64 = 1
    var t305_rhs int = 62
    var t305 int64 = t305_lhs << t305_rhs
    println__T_int64(t305)
    var t306_operand int64 = 4294967295
    var t306 int64 = ^t306_operand
    println__T_int64(t306)
    return struct{}{}
}

func precedence() struct{} {
    var t309_lhs uint8 = 3
    var t309_rhs uint8 = 1
    var t309 uint8 = t309_lhs & t309_rhs
    var t310_lhs uint8 = 2
    var t310 uint8 = t310_lhs ^ t309
    var t311_lhs uint8 = 1
    var t311 uint8 = t311_lhs | t310
    println__T_uint8(t311)
    var t312 int = 2 + 1
    var t313_lhs uint8 = 1
    var t313 uint8 = t313_lhs << t312
    println__T_uint8(t313)
    var t314_lhs int = 1
    var t314_rhs int = 2
    var t314 int = t314_lhs | t314_rhs
    var t315 bool
    var inline558 int = 3
    var inline559 bool = t314 == inline558
    t315 = inline559
    var t316 string
    var inline556 string = _goml_runtime_core_bool_to_string(t315)
    t316 = inline556
    var inline553 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t316)
    _goml_runtime_core_string_println(inline553)
    var t317_lhs int = 8
    var t317_rhs int = 1
    var t317 int = t317_lhs >> t317_rhs
    var t318 bool = t317 < 5
    var t319 string
    var inline551 string = _goml_runtime_core_bool_to_string(t318)
    t319 = inline551
    var inline548 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t319)
    _goml_runtime_core_string_println(inline548)
    var t320_operand uint8 = 1
    var t320 uint8 = ^t320_operand
    var t321_rhs uint8 = 15
    var t321 uint8 = t320 & t321_rhs
    println__T_uint8(t321)
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
    var t324 uint8 = uint8(uint16(five_eleven__8))
    println__T_uint8(t324)
    var t325 uint8 = uint8(uint16(two_fifty_six__9))
    println__T_uint8(t325)
    var t326 uint8 = uint8(int16(negative_one_i16__10))
    println__T_uint8(t326)
    var t327 int8 = int8(uint8(two_fifty_five__11))
    println__T_int8(t327)
    var t328 int8 = int8(uint8(one_twenty_eight__12))
    println__T_int8(t328)
    var t329 int8 = int8(int16(negative_one_twenty_nine__13))
    println__T_int8(t329)
    var t330 int16 = int16(uint16(max_u16__14))
    println__T_int16(t330)
    var t331 uint16 = uint16(int32(negative_one_i32__15))
    println__T_uint16(t331)
    var t332 uint64 = uint64(int8(negative_one_i8__16))
    println__T_uint64(t332)
    var t333 int32 = int32(uint64(max_u64__17))
    println__T_int32(t333)
    var t334 uint32 = uint32(uint8(sixty_five__18))
    println__T_uint32(t334)
    var t335 int64 = int64(uint32(max_u32__19))
    println__T_int64(t335)
    var t336_source rune = 65
    var t336 uint32 = uint32(rune(t336_source))
    println__T_uint32(t336)
    var mtmp233 Option__char
    var inline576 uint32 = 128512
    var inline577 bool = utf8_valid_scalar(inline576)
    if inline577 {
        var inline578 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(inline576)
        var inline580 rune = inline578._1
        var inline582 Option__char = Some{
            _0: inline580,
        }
        mtmp233 = inline582
    } else {
        mtmp233 = None{}
    }
    switch mtmp233.(type) {
    case None:
        var inline565 string = "invalid"
        var inline566 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline565)
        _goml_runtime_core_string_println(inline566)
    case Some:
        var x234 rune = mtmp233.(Some)._0
        var t342 string
        var inline572 string = char_to_string(x234)
        t342 = inline572
        var inline569 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t342)
        _goml_runtime_core_string_println(inline569)
    default:
        panic("non-exhaustive match")
    }
    var t338 uint8 = uint8(uint16(three_hundred__20))
    var t339 uint32 = uint32(uint8(t338))
    println__T_uint32(t339)
    return struct{}{}
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t352 uint8
    var inline612 uint8 = 10
    var inline613 uint8 = ^inline612
    var inline614_rhs uint8 = 15
    var inline614 uint8 = inline613 & inline614_rhs
    var inline615_lhs uint8 = 1
    var inline615_rhs int = 4
    var inline615 uint8 = inline615_lhs << inline615_rhs
    var inline616_rhs uint8 = 31
    var inline616 uint8 = inline615 % inline616_rhs
    var inline617 uint8 = inline614 | inline616
    t352 = inline617
    println__T_uint8(t352)
    return struct{}{}
}

func println__T_uint8(value__31 uint8) struct{} {
    var t355 string
    var inline619 string = _goml_runtime_core_uint8_to_string(value__31)
    t355 = inline619
    _goml_runtime_core_string_println(t355)
    return struct{}{}
}

func println__T_uint16(value__31 uint16) struct{} {
    var t358 string
    var inline621 string = _goml_runtime_core_uint16_to_string(value__31)
    t358 = inline621
    _goml_runtime_core_string_println(t358)
    return struct{}{}
}

func println__T_uint32(value__31 uint32) struct{} {
    var t361 string
    var inline623 string = _goml_runtime_core_uint32_to_string(value__31)
    t361 = inline623
    _goml_runtime_core_string_println(t361)
    return struct{}{}
}

func println__T_uint64(value__31 uint64) struct{} {
    var t364 string
    var inline625 string = _goml_runtime_core_uint64_to_string(value__31)
    t364 = inline625
    _goml_runtime_core_string_println(t364)
    return struct{}{}
}

func println__T_int8(value__31 int8) struct{} {
    var t367 string
    var inline627 string = _goml_runtime_core_int8_to_string(value__31)
    t367 = inline627
    _goml_runtime_core_string_println(t367)
    return struct{}{}
}

func println__T_int16(value__31 int16) struct{} {
    var t370 string
    var inline629 string = _goml_runtime_core_int16_to_string(value__31)
    t370 = inline629
    _goml_runtime_core_string_println(t370)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t373 string
    var inline631 string = _goml_runtime_core_int32_to_string(value__31)
    t373 = inline631
    _goml_runtime_core_string_println(t373)
    return struct{}{}
}

func println__T_int64(value__31 int64) struct{} {
    var t376 string
    var inline633 string = _goml_runtime_core_int64_to_string(value__31)
    t376 = inline633
    _goml_runtime_core_string_println(t376)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__74 uint8) string {
    var t398 string = _goml_runtime_core_uint8_to_string(self__74)
    return t398
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t426 bool = value__4 <= 1114111
    if t426 {
        var t430 bool = value__4 >= 55296
        var jp428 bool
        if t430 {
            var t431 bool = value__4 <= 57343
            jp428 = t431
        } else {
            jp428 = false
        }
        var t429 bool = !jp428
        return t429
    } else {
        return false
    }
}

func char_to_string(value__29 rune) string {
    var t436 uint32 = uint32(rune(value__29))
    var t437 bool
    var inline648 bool = t436 <= 1114111
    if inline648 {
        var inline649 bool = t436 >= 55296
        var inline651 bool
        if inline649 {
            var inline653 bool = t436 <= 57343
            inline651 = inline653
        } else {
            inline651 = false
        }
        var inline652 bool = !inline651
        t437 = inline652
    } else {
        t437 = false
    }
    if t437 {
        var t438 string = _goml_runtime_core_char_to_string(value__29)
        return t438
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
