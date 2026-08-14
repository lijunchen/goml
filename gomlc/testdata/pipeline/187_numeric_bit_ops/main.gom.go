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
    var inline453 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__0)
    _goml_runtime_core_string_println(inline453)
    return struct{}{}
}

func unsigned_ops() struct{} {
    var t267_lhs uint8 = 13
    var t267_rhs uint8 = 5
    var t267 uint8 = t267_lhs % t267_rhs
    show_u8(t267)
    var t268_lhs uint8 = 12
    var t268_rhs uint8 = 10
    var t268 uint8 = t268_lhs & t268_rhs
    show_u8(t268)
    var t269_lhs uint8 = 12
    var t269_rhs uint8 = 3
    var t269 uint8 = t269_lhs | t269_rhs
    show_u8(t269)
    var t270_lhs uint8 = 12
    var t270_rhs uint8 = 10
    var t270 uint8 = t270_lhs ^ t270_rhs
    show_u8(t270)
    var t271_lhs uint8 = 1
    var t271_rhs int = 7
    var t271 uint8 = t271_lhs << t271_rhs
    show_u8(t271)
    var t272_lhs uint8 = 128
    var t272_rhs int = 7
    var t272 uint8 = t272_lhs >> t272_rhs
    println__T_uint8(t272)
    var t273_operand uint8 = 0
    var t273 uint8 = ^t273_operand
    println__T_uint8(t273)
    var t274_lhs uint8 = 1
    var t274_rhs int = 8
    var t274 uint8 = t274_lhs << t274_rhs
    println__T_uint8(t274)
    var t275_lhs uint16 = 513
    var t275_rhs uint16 = 256
    var t275 uint16 = t275_lhs % t275_rhs
    println__T_uint16(t275)
    var t276_lhs uint16 = 3855
    var t276_rhs uint16 = 255
    var t276 uint16 = t276_lhs & t276_rhs
    println__T_uint16(t276)
    var t277_lhs uint16 = 3840
    var t277_rhs uint16 = 15
    var t277 uint16 = t277_lhs | t277_rhs
    println__T_uint16(t277)
    var t278_lhs uint16 = 43690
    var t278_rhs uint16 = 3855
    var t278 uint16 = t278_lhs ^ t278_rhs
    println__T_uint16(t278)
    var t279_lhs uint16 = 1
    var t279_rhs int = 15
    var t279 uint16 = t279_lhs << t279_rhs
    println__T_uint16(t279)
    var t280_lhs uint16 = 32768
    var t280_rhs int = 15
    var t280 uint16 = t280_lhs >> t280_rhs
    println__T_uint16(t280)
    var t281_operand uint16 = 0
    var t281 uint16 = ^t281_operand
    println__T_uint16(t281)
    var t282_lhs uint32 = 1000000001
    var t282_rhs uint32 = 1000
    var t282 uint32 = t282_lhs % t282_rhs
    println__T_uint32(t282)
    var t283_lhs uint32 = 4042322160
    var t283_rhs uint32 = 252645135
    var t283 uint32 = t283_lhs & t283_rhs
    println__T_uint32(t283)
    var t284_lhs uint32 = 4042322160
    var t284_rhs uint32 = 252645135
    var t284 uint32 = t284_lhs | t284_rhs
    println__T_uint32(t284)
    var t285_lhs uint32 = 4042322160
    var t285_rhs uint32 = 252645135
    var t285 uint32 = t285_lhs ^ t285_rhs
    println__T_uint32(t285)
    var t286_lhs uint32 = 1
    var t286_rhs int = 31
    var t286 uint32 = t286_lhs << t286_rhs
    println__T_uint32(t286)
    var t287_lhs uint32 = 2147483648
    var t287_rhs int = 31
    var t287 uint32 = t287_lhs >> t287_rhs
    println__T_uint32(t287)
    var t288_operand uint32 = 0
    var t288 uint32 = ^t288_operand
    println__T_uint32(t288)
    var t289_lhs uint64 = 1000000000001
    var t289_rhs uint64 = 1000
    var t289 uint64 = t289_lhs % t289_rhs
    println__T_uint64(t289)
    var t290_lhs uint64 = 17361641481138401520
    var t290_rhs uint64 = 1085102592571150095
    var t290 uint64 = t290_lhs & t290_rhs
    println__T_uint64(t290)
    var t291_lhs uint64 = 17361641481138401520
    var t291_rhs uint64 = 1085102592571150095
    var t291 uint64 = t291_lhs | t291_rhs
    println__T_uint64(t291)
    var t292_lhs uint64 = 17361641481138401520
    var t292_rhs uint64 = 1085102592571150095
    var t292 uint64 = t292_lhs ^ t292_rhs
    println__T_uint64(t292)
    var t293_lhs uint64 = 1
    var t293_rhs int = 63
    var t293 uint64 = t293_lhs << t293_rhs
    println__T_uint64(t293)
    var t294_lhs uint64 = 9223372036854775808
    var t294_rhs int = 63
    var t294 uint64 = t294_lhs >> t294_rhs
    println__T_uint64(t294)
    var t295_operand uint64 = 0
    var t295 uint64 = ^t295_operand
    println__T_uint64(t295)
    return struct{}{}
}

func signed_ops() struct{} {
    var t298_lhs int8 = -13
    var t298_rhs int8 = 5
    var t298 int8 = t298_lhs % t298_rhs
    println__T_int8(t298)
    var t299_lhs int8 = -8
    var t299_rhs int = 2
    var t299 int8 = t299_lhs >> t299_rhs
    println__T_int8(t299)
    var t300_lhs int8 = 1
    var t300_rhs int = 6
    var t300 int8 = t300_lhs << t300_rhs
    println__T_int8(t300)
    var t301_operand int8 = 0
    var t301 int8 = ^t301_operand
    println__T_int8(t301)
    var t302_lhs int8 = -1
    var t302_rhs int = 7
    var t302 int8 = t302_lhs >> t302_rhs
    println__T_int8(t302)
    var t303_lhs int16 = -513
    var t303_rhs int16 = 256
    var t303 int16 = t303_lhs % t303_rhs
    println__T_int16(t303)
    var t304 int16 = -32767 - 1
    var t305_rhs int = 15
    var t305 int16 = t304 >> t305_rhs
    println__T_int16(t305)
    var t306_lhs int16 = 1
    var t306_rhs int = 14
    var t306 int16 = t306_lhs << t306_rhs
    println__T_int16(t306)
    var t307_operand int16 = 255
    var t307 int16 = ^t307_operand
    println__T_int16(t307)
    var t308_lhs int32 = -1000000001
    var t308_rhs int32 = 1000
    var t308 int32 = t308_lhs % t308_rhs
    println__T_int32(t308)
    var t309 int32 = -2147483647 - 1
    var t310_rhs int = 31
    var t310 int32 = t309 >> t310_rhs
    println__T_int32(t310)
    var t311_lhs int32 = 1
    var t311_rhs int = 30
    var t311 int32 = t311_lhs << t311_rhs
    println__T_int32(t311)
    var t312_operand int32 = 65535
    var t312 int32 = ^t312_operand
    println__T_int32(t312)
    var t313_lhs int64 = -1000000000001
    var t313_rhs int64 = 1000
    var t313 int64 = t313_lhs % t313_rhs
    println__T_int64(t313)
    var t314_lhs int64 = -9223372036854775807
    var t314_rhs int = 62
    var t314 int64 = t314_lhs >> t314_rhs
    println__T_int64(t314)
    var t315_lhs int64 = 1
    var t315_rhs int = 62
    var t315 int64 = t315_lhs << t315_rhs
    println__T_int64(t315)
    var t316_operand int64 = 4294967295
    var t316 int64 = ^t316_operand
    println__T_int64(t316)
    return struct{}{}
}

func precedence() struct{} {
    var t319_lhs uint8 = 3
    var t319_rhs uint8 = 1
    var t319 uint8 = t319_lhs & t319_rhs
    var t320_lhs uint8 = 2
    var t320 uint8 = t320_lhs ^ t319
    var t321_lhs uint8 = 1
    var t321 uint8 = t321_lhs | t320
    println__T_uint8(t321)
    var t322 int = 2 + 1
    var t323_lhs uint8 = 1
    var t323 uint8 = t323_lhs << t322
    println__T_uint8(t323)
    var t324_lhs int = 1
    var t324_rhs int = 2
    var t324 int = t324_lhs | t324_rhs
    var t325 bool
    var inline571 int = 3
    var inline572 bool = t324 == inline571
    t325 = inline572
    var t326 string
    var inline569 string = _goml_runtime_core_bool_to_string(t325)
    t326 = inline569
    var inline566 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t326)
    _goml_runtime_core_string_println(inline566)
    var t327_lhs int = 8
    var t327_rhs int = 1
    var t327 int = t327_lhs >> t327_rhs
    var t328 bool = t327 < 5
    var t329 string
    var inline564 string = _goml_runtime_core_bool_to_string(t328)
    t329 = inline564
    var inline561 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t329)
    _goml_runtime_core_string_println(inline561)
    var t330_operand uint8 = 1
    var t330 uint8 = ^t330_operand
    var t331_rhs uint8 = 15
    var t331 uint8 = t330 & t331_rhs
    println__T_uint8(t331)
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
    var t334 uint8 = uint8(uint16(five_eleven__8))
    println__T_uint8(t334)
    var t335 uint8 = uint8(uint16(two_fifty_six__9))
    println__T_uint8(t335)
    var t336 uint8 = uint8(int16(negative_one_i16__10))
    println__T_uint8(t336)
    var t337 int8 = int8(uint8(two_fifty_five__11))
    println__T_int8(t337)
    var t338 int8 = int8(uint8(one_twenty_eight__12))
    println__T_int8(t338)
    var t339 int8 = int8(int16(negative_one_twenty_nine__13))
    println__T_int8(t339)
    var t340 int16 = int16(uint16(max_u16__14))
    println__T_int16(t340)
    var t341 uint16 = uint16(int32(negative_one_i32__15))
    println__T_uint16(t341)
    var t342 uint64 = uint64(int8(negative_one_i8__16))
    println__T_uint64(t342)
    var t343 int32 = int32(uint64(max_u64__17))
    println__T_int32(t343)
    var t344 uint32 = uint32(uint8(sixty_five__18))
    println__T_uint32(t344)
    var t345 int64 = int64(uint32(max_u32__19))
    println__T_int64(t345)
    var t346_source rune = 65
    var t346 uint32 = uint32(rune(t346_source))
    println__T_uint32(t346)
    var mtmp243 Option__char
    var inline589 uint32 = 128512
    var inline590 Option__char = __goml_builtin_char_from_uint32(inline589)
    mtmp243 = inline590
    switch mtmp243.(type) {
    case None:
        var inline578 string = "invalid"
        var inline579 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline578)
        _goml_runtime_core_string_println(inline579)
    case Some:
        var x244 rune = mtmp243.(Some)._0
        var t352 string
        var inline585 string = char_to_string(x244)
        t352 = inline585
        var inline582 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t352)
        _goml_runtime_core_string_println(inline582)
    default:
        panic("non-exhaustive match")
    }
    var t348 uint8 = uint8(uint16(three_hundred__20))
    var t349 uint32 = uint32(uint8(t348))
    println__T_uint32(t349)
    return struct{}{}
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t362 uint8
    var inline620 uint8 = 10
    var inline621 uint8 = ^inline620
    var inline622_rhs uint8 = 15
    var inline622 uint8 = inline621 & inline622_rhs
    var inline623_lhs uint8 = 1
    var inline623_rhs int = 4
    var inline623 uint8 = inline623_lhs << inline623_rhs
    var inline624_rhs uint8 = 31
    var inline624 uint8 = inline623 % inline624_rhs
    var inline625 uint8 = inline622 | inline624
    t362 = inline625
    println__T_uint8(t362)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t365 string
    var inline627 string = _goml_runtime_core_uint8_to_string(value__1)
    t365 = inline627
    _goml_runtime_core_string_println(t365)
    return struct{}{}
}

func println__T_uint16(value__1 uint16) struct{} {
    var t368 string
    var inline629 string = _goml_runtime_core_uint16_to_string(value__1)
    t368 = inline629
    _goml_runtime_core_string_println(t368)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t371 string
    var inline631 string = _goml_runtime_core_uint32_to_string(value__1)
    t371 = inline631
    _goml_runtime_core_string_println(t371)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t374 string
    var inline633 string = _goml_runtime_core_uint64_to_string(value__1)
    t374 = inline633
    _goml_runtime_core_string_println(t374)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t377 string
    var inline635 string = _goml_runtime_core_int8_to_string(value__1)
    t377 = inline635
    _goml_runtime_core_string_println(t377)
    return struct{}{}
}

func println__T_int16(value__1 int16) struct{} {
    var t380 string
    var inline637 string = _goml_runtime_core_int16_to_string(value__1)
    t380 = inline637
    _goml_runtime_core_string_println(t380)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t383 string
    var inline639 string = _goml_runtime_core_int32_to_string(value__1)
    t383 = inline639
    _goml_runtime_core_string_println(t383)
    return struct{}{}
}

func println__T_int64(value__1 int64) struct{} {
    var t386 string
    var inline641 string = _goml_runtime_core_int64_to_string(value__1)
    t386 = inline641
    _goml_runtime_core_string_println(t386)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__72 uint8) string {
    var t405 string = _goml_runtime_core_uint8_to_string(self__72)
    return t405
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t433 bool
    var inline655 bool = value__30 <= 1114111
    if inline655 {
        var inline656 bool = value__30 >= 55296
        var inline658 bool
        if inline656 {
            var inline660 bool = value__30 <= 57343
            inline658 = inline660
        } else {
            inline658 = false
        }
        var inline659 bool = !inline658
        t433 = inline659
    } else {
        t433 = false
    }
    if t433 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t434 Option__char = Some{
            _0: x24,
        }
        return t434
    } else {
        return None{}
    }
}

func char_to_string(value__29 rune) string {
    var t439 uint32 = uint32(rune(value__29))
    var t440 bool
    var inline662 bool = t439 <= 1114111
    if inline662 {
        var inline663 bool = t439 >= 55296
        var inline665 bool
        if inline663 {
            var inline667 bool = t439 <= 57343
            inline665 = inline667
        } else {
            inline665 = false
        }
        var inline666 bool = !inline665
        t440 = inline666
    } else {
        t440 = false
    }
    if t440 {
        var t441 string = _goml_runtime_core_char_to_string(value__29)
        return t441
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
