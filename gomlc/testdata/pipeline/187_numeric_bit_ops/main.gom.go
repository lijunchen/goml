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
    var inline458 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__0)
    _goml_runtime_core_string_println(inline458)
    return struct{}{}
}

func unsigned_ops() struct{} {
    var t272_lhs uint8 = 13
    var t272_rhs uint8 = 5
    var t272 uint8 = t272_lhs % t272_rhs
    show_u8(t272)
    var t273_lhs uint8 = 12
    var t273_rhs uint8 = 10
    var t273 uint8 = t273_lhs & t273_rhs
    show_u8(t273)
    var t274_lhs uint8 = 12
    var t274_rhs uint8 = 3
    var t274 uint8 = t274_lhs | t274_rhs
    show_u8(t274)
    var t275_lhs uint8 = 12
    var t275_rhs uint8 = 10
    var t275 uint8 = t275_lhs ^ t275_rhs
    show_u8(t275)
    var t276_lhs uint8 = 1
    var t276_rhs int = 7
    var t276 uint8 = t276_lhs << t276_rhs
    show_u8(t276)
    var t277_lhs uint8 = 128
    var t277_rhs int = 7
    var t277 uint8 = t277_lhs >> t277_rhs
    println__T_uint8(t277)
    var t278_operand uint8 = 0
    var t278 uint8 = ^t278_operand
    println__T_uint8(t278)
    var t279_lhs uint8 = 1
    var t279_rhs int = 8
    var t279 uint8 = t279_lhs << t279_rhs
    println__T_uint8(t279)
    var t280_lhs uint16 = 513
    var t280_rhs uint16 = 256
    var t280 uint16 = t280_lhs % t280_rhs
    println__T_uint16(t280)
    var t281_lhs uint16 = 3855
    var t281_rhs uint16 = 255
    var t281 uint16 = t281_lhs & t281_rhs
    println__T_uint16(t281)
    var t282_lhs uint16 = 3840
    var t282_rhs uint16 = 15
    var t282 uint16 = t282_lhs | t282_rhs
    println__T_uint16(t282)
    var t283_lhs uint16 = 43690
    var t283_rhs uint16 = 3855
    var t283 uint16 = t283_lhs ^ t283_rhs
    println__T_uint16(t283)
    var t284_lhs uint16 = 1
    var t284_rhs int = 15
    var t284 uint16 = t284_lhs << t284_rhs
    println__T_uint16(t284)
    var t285_lhs uint16 = 32768
    var t285_rhs int = 15
    var t285 uint16 = t285_lhs >> t285_rhs
    println__T_uint16(t285)
    var t286_operand uint16 = 0
    var t286 uint16 = ^t286_operand
    println__T_uint16(t286)
    var t287_lhs uint32 = 1000000001
    var t287_rhs uint32 = 1000
    var t287 uint32 = t287_lhs % t287_rhs
    println__T_uint32(t287)
    var t288_lhs uint32 = 4042322160
    var t288_rhs uint32 = 252645135
    var t288 uint32 = t288_lhs & t288_rhs
    println__T_uint32(t288)
    var t289_lhs uint32 = 4042322160
    var t289_rhs uint32 = 252645135
    var t289 uint32 = t289_lhs | t289_rhs
    println__T_uint32(t289)
    var t290_lhs uint32 = 4042322160
    var t290_rhs uint32 = 252645135
    var t290 uint32 = t290_lhs ^ t290_rhs
    println__T_uint32(t290)
    var t291_lhs uint32 = 1
    var t291_rhs int = 31
    var t291 uint32 = t291_lhs << t291_rhs
    println__T_uint32(t291)
    var t292_lhs uint32 = 2147483648
    var t292_rhs int = 31
    var t292 uint32 = t292_lhs >> t292_rhs
    println__T_uint32(t292)
    var t293_operand uint32 = 0
    var t293 uint32 = ^t293_operand
    println__T_uint32(t293)
    var t294_lhs uint64 = 1000000000001
    var t294_rhs uint64 = 1000
    var t294 uint64 = t294_lhs % t294_rhs
    println__T_uint64(t294)
    var t295_lhs uint64 = 17361641481138401520
    var t295_rhs uint64 = 1085102592571150095
    var t295 uint64 = t295_lhs & t295_rhs
    println__T_uint64(t295)
    var t296_lhs uint64 = 17361641481138401520
    var t296_rhs uint64 = 1085102592571150095
    var t296 uint64 = t296_lhs | t296_rhs
    println__T_uint64(t296)
    var t297_lhs uint64 = 17361641481138401520
    var t297_rhs uint64 = 1085102592571150095
    var t297 uint64 = t297_lhs ^ t297_rhs
    println__T_uint64(t297)
    var t298_lhs uint64 = 1
    var t298_rhs int = 63
    var t298 uint64 = t298_lhs << t298_rhs
    println__T_uint64(t298)
    var t299_lhs uint64 = 9223372036854775808
    var t299_rhs int = 63
    var t299 uint64 = t299_lhs >> t299_rhs
    println__T_uint64(t299)
    var t300_operand uint64 = 0
    var t300 uint64 = ^t300_operand
    println__T_uint64(t300)
    return struct{}{}
}

func signed_ops() struct{} {
    var t303_lhs int8 = -13
    var t303_rhs int8 = 5
    var t303 int8 = t303_lhs % t303_rhs
    println__T_int8(t303)
    var t304_lhs int8 = -8
    var t304_rhs int = 2
    var t304 int8 = t304_lhs >> t304_rhs
    println__T_int8(t304)
    var t305_lhs int8 = 1
    var t305_rhs int = 6
    var t305 int8 = t305_lhs << t305_rhs
    println__T_int8(t305)
    var t306_operand int8 = 0
    var t306 int8 = ^t306_operand
    println__T_int8(t306)
    var t307_lhs int8 = -1
    var t307_rhs int = 7
    var t307 int8 = t307_lhs >> t307_rhs
    println__T_int8(t307)
    var t308_lhs int16 = -513
    var t308_rhs int16 = 256
    var t308 int16 = t308_lhs % t308_rhs
    println__T_int16(t308)
    var t309 int16 = -32767 - 1
    var t310_rhs int = 15
    var t310 int16 = t309 >> t310_rhs
    println__T_int16(t310)
    var t311_lhs int16 = 1
    var t311_rhs int = 14
    var t311 int16 = t311_lhs << t311_rhs
    println__T_int16(t311)
    var t312_operand int16 = 255
    var t312 int16 = ^t312_operand
    println__T_int16(t312)
    var t313_lhs int32 = -1000000001
    var t313_rhs int32 = 1000
    var t313 int32 = t313_lhs % t313_rhs
    println__T_int32(t313)
    var t314 int32 = -2147483647 - 1
    var t315_rhs int = 31
    var t315 int32 = t314 >> t315_rhs
    println__T_int32(t315)
    var t316_lhs int32 = 1
    var t316_rhs int = 30
    var t316 int32 = t316_lhs << t316_rhs
    println__T_int32(t316)
    var t317_operand int32 = 65535
    var t317 int32 = ^t317_operand
    println__T_int32(t317)
    var t318_lhs int64 = -1000000000001
    var t318_rhs int64 = 1000
    var t318 int64 = t318_lhs % t318_rhs
    println__T_int64(t318)
    var t319_lhs int64 = -9223372036854775807
    var t319_rhs int = 62
    var t319 int64 = t319_lhs >> t319_rhs
    println__T_int64(t319)
    var t320_lhs int64 = 1
    var t320_rhs int = 62
    var t320 int64 = t320_lhs << t320_rhs
    println__T_int64(t320)
    var t321_operand int64 = 4294967295
    var t321 int64 = ^t321_operand
    println__T_int64(t321)
    return struct{}{}
}

func precedence() struct{} {
    var t324_lhs uint8 = 3
    var t324_rhs uint8 = 1
    var t324 uint8 = t324_lhs & t324_rhs
    var t325_lhs uint8 = 2
    var t325 uint8 = t325_lhs ^ t324
    var t326_lhs uint8 = 1
    var t326 uint8 = t326_lhs | t325
    println__T_uint8(t326)
    var t327 int = 2 + 1
    var t328_lhs uint8 = 1
    var t328 uint8 = t328_lhs << t327
    println__T_uint8(t328)
    var t329_lhs int = 1
    var t329_rhs int = 2
    var t329 int = t329_lhs | t329_rhs
    var t330 bool
    var inline576 int = 3
    var inline577 bool = t329 == inline576
    t330 = inline577
    var t331 string
    var inline574 string = _goml_runtime_core_bool_to_string(t330)
    t331 = inline574
    var inline571 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t331)
    _goml_runtime_core_string_println(inline571)
    var t332_lhs int = 8
    var t332_rhs int = 1
    var t332 int = t332_lhs >> t332_rhs
    var t333 bool = t332 < 5
    var t334 string
    var inline569 string = _goml_runtime_core_bool_to_string(t333)
    t334 = inline569
    var inline566 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t334)
    _goml_runtime_core_string_println(inline566)
    var t335_operand uint8 = 1
    var t335 uint8 = ^t335_operand
    var t336_rhs uint8 = 15
    var t336 uint8 = t335 & t336_rhs
    println__T_uint8(t336)
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
    var t339 uint8 = uint8(uint16(five_eleven__8))
    println__T_uint8(t339)
    var t340 uint8 = uint8(uint16(two_fifty_six__9))
    println__T_uint8(t340)
    var t341 uint8 = uint8(int16(negative_one_i16__10))
    println__T_uint8(t341)
    var t342 int8 = int8(uint8(two_fifty_five__11))
    println__T_int8(t342)
    var t343 int8 = int8(uint8(one_twenty_eight__12))
    println__T_int8(t343)
    var t344 int8 = int8(int16(negative_one_twenty_nine__13))
    println__T_int8(t344)
    var t345 int16 = int16(uint16(max_u16__14))
    println__T_int16(t345)
    var t346 uint16 = uint16(int32(negative_one_i32__15))
    println__T_uint16(t346)
    var t347 uint64 = uint64(int8(negative_one_i8__16))
    println__T_uint64(t347)
    var t348 int32 = int32(uint64(max_u64__17))
    println__T_int32(t348)
    var t349 uint32 = uint32(uint8(sixty_five__18))
    println__T_uint32(t349)
    var t350 int64 = int64(uint32(max_u32__19))
    println__T_int64(t350)
    var t351_source rune = 65
    var t351 uint32 = uint32(rune(t351_source))
    println__T_uint32(t351)
    var mtmp248 Option__char
    var inline594 uint32 = 128512
    var inline595 Option__char = __goml_builtin_char_from_uint32(inline594)
    mtmp248 = inline595
    switch mtmp248.(type) {
    case None:
        var inline583 string = "invalid"
        var inline584 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline583)
        _goml_runtime_core_string_println(inline584)
    case Some:
        var x249 rune = mtmp248.(Some)._0
        var t357 string
        var inline590 string = char_to_string(x249)
        t357 = inline590
        var inline587 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t357)
        _goml_runtime_core_string_println(inline587)
    default:
        panic("non-exhaustive match")
    }
    var t353 uint8 = uint8(uint16(three_hundred__20))
    var t354 uint32 = uint32(uint8(t353))
    println__T_uint32(t354)
    return struct{}{}
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t367 uint8
    var inline625 uint8 = 10
    var inline626 uint8 = ^inline625
    var inline627_rhs uint8 = 15
    var inline627 uint8 = inline626 & inline627_rhs
    var inline628_lhs uint8 = 1
    var inline628_rhs int = 4
    var inline628 uint8 = inline628_lhs << inline628_rhs
    var inline629_rhs uint8 = 31
    var inline629 uint8 = inline628 % inline629_rhs
    var inline630 uint8 = inline627 | inline629
    t367 = inline630
    println__T_uint8(t367)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t370 string
    var inline632 string = _goml_runtime_core_uint8_to_string(value__1)
    t370 = inline632
    _goml_runtime_core_string_println(t370)
    return struct{}{}
}

func println__T_uint16(value__1 uint16) struct{} {
    var t373 string
    var inline634 string = _goml_runtime_core_uint16_to_string(value__1)
    t373 = inline634
    _goml_runtime_core_string_println(t373)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t376 string
    var inline636 string = _goml_runtime_core_uint32_to_string(value__1)
    t376 = inline636
    _goml_runtime_core_string_println(t376)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t379 string
    var inline638 string = _goml_runtime_core_uint64_to_string(value__1)
    t379 = inline638
    _goml_runtime_core_string_println(t379)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t382 string
    var inline640 string = _goml_runtime_core_int8_to_string(value__1)
    t382 = inline640
    _goml_runtime_core_string_println(t382)
    return struct{}{}
}

func println__T_int16(value__1 int16) struct{} {
    var t385 string
    var inline642 string = _goml_runtime_core_int16_to_string(value__1)
    t385 = inline642
    _goml_runtime_core_string_println(t385)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t388 string
    var inline644 string = _goml_runtime_core_int32_to_string(value__1)
    t388 = inline644
    _goml_runtime_core_string_println(t388)
    return struct{}{}
}

func println__T_int64(value__1 int64) struct{} {
    var t391 string
    var inline646 string = _goml_runtime_core_int64_to_string(value__1)
    t391 = inline646
    _goml_runtime_core_string_println(t391)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__72 uint8) string {
    var t410 string = _goml_runtime_core_uint8_to_string(self__72)
    return t410
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t438 bool
    var inline660 bool = value__30 <= 1114111
    if inline660 {
        var inline661 bool = value__30 >= 55296
        var inline663 bool
        if inline661 {
            var inline665 bool = value__30 <= 57343
            inline663 = inline665
        } else {
            inline663 = false
        }
        var inline664 bool = !inline663
        t438 = inline664
    } else {
        t438 = false
    }
    if t438 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t439 Option__char = Some{
            _0: x24,
        }
        return t439
    } else {
        return None{}
    }
}

func char_to_string(value__29 rune) string {
    var t444 uint32 = uint32(rune(value__29))
    var t445 bool
    var inline667 bool = t444 <= 1114111
    if inline667 {
        var inline668 bool = t444 >= 55296
        var inline670 bool
        if inline668 {
            var inline672 bool = t444 <= 57343
            inline670 = inline672
        } else {
            inline670 = false
        }
        var inline671 bool = !inline670
        t445 = inline671
    } else {
        t445 = false
    }
    if t445 {
        var t446 string = _goml_runtime_core_char_to_string(value__29)
        return t446
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
