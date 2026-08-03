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
    var inline445 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__0)
    _goml_runtime_core_string_println(inline445)
    return struct{}{}
}

func unsigned_ops() struct{} {
    var t262_lhs uint8 = 13
    var t262_rhs uint8 = 5
    var t262 uint8 = t262_lhs % t262_rhs
    show_u8(t262)
    var t263_lhs uint8 = 12
    var t263_rhs uint8 = 10
    var t263 uint8 = t263_lhs & t263_rhs
    show_u8(t263)
    var t264_lhs uint8 = 12
    var t264_rhs uint8 = 3
    var t264 uint8 = t264_lhs | t264_rhs
    show_u8(t264)
    var t265_lhs uint8 = 12
    var t265_rhs uint8 = 10
    var t265 uint8 = t265_lhs ^ t265_rhs
    show_u8(t265)
    var t266_lhs uint8 = 1
    var t266_rhs int = 7
    var t266 uint8 = t266_lhs << t266_rhs
    show_u8(t266)
    var t267_lhs uint8 = 128
    var t267_rhs int = 7
    var t267 uint8 = t267_lhs >> t267_rhs
    println__T_uint8(t267)
    var t268_operand uint8 = 0
    var t268 uint8 = ^t268_operand
    println__T_uint8(t268)
    var t269_lhs uint8 = 1
    var t269_rhs int = 8
    var t269 uint8 = t269_lhs << t269_rhs
    println__T_uint8(t269)
    var t270_lhs uint16 = 513
    var t270_rhs uint16 = 256
    var t270 uint16 = t270_lhs % t270_rhs
    println__T_uint16(t270)
    var t271_lhs uint16 = 3855
    var t271_rhs uint16 = 255
    var t271 uint16 = t271_lhs & t271_rhs
    println__T_uint16(t271)
    var t272_lhs uint16 = 3840
    var t272_rhs uint16 = 15
    var t272 uint16 = t272_lhs | t272_rhs
    println__T_uint16(t272)
    var t273_lhs uint16 = 43690
    var t273_rhs uint16 = 3855
    var t273 uint16 = t273_lhs ^ t273_rhs
    println__T_uint16(t273)
    var t274_lhs uint16 = 1
    var t274_rhs int = 15
    var t274 uint16 = t274_lhs << t274_rhs
    println__T_uint16(t274)
    var t275_lhs uint16 = 32768
    var t275_rhs int = 15
    var t275 uint16 = t275_lhs >> t275_rhs
    println__T_uint16(t275)
    var t276_operand uint16 = 0
    var t276 uint16 = ^t276_operand
    println__T_uint16(t276)
    var t277_lhs uint32 = 1000000001
    var t277_rhs uint32 = 1000
    var t277 uint32 = t277_lhs % t277_rhs
    println__T_uint32(t277)
    var t278_lhs uint32 = 4042322160
    var t278_rhs uint32 = 252645135
    var t278 uint32 = t278_lhs & t278_rhs
    println__T_uint32(t278)
    var t279_lhs uint32 = 4042322160
    var t279_rhs uint32 = 252645135
    var t279 uint32 = t279_lhs | t279_rhs
    println__T_uint32(t279)
    var t280_lhs uint32 = 4042322160
    var t280_rhs uint32 = 252645135
    var t280 uint32 = t280_lhs ^ t280_rhs
    println__T_uint32(t280)
    var t281_lhs uint32 = 1
    var t281_rhs int = 31
    var t281 uint32 = t281_lhs << t281_rhs
    println__T_uint32(t281)
    var t282_lhs uint32 = 2147483648
    var t282_rhs int = 31
    var t282 uint32 = t282_lhs >> t282_rhs
    println__T_uint32(t282)
    var t283_operand uint32 = 0
    var t283 uint32 = ^t283_operand
    println__T_uint32(t283)
    var t284_lhs uint64 = 1000000000001
    var t284_rhs uint64 = 1000
    var t284 uint64 = t284_lhs % t284_rhs
    println__T_uint64(t284)
    var t285_lhs uint64 = 17361641481138401520
    var t285_rhs uint64 = 1085102592571150095
    var t285 uint64 = t285_lhs & t285_rhs
    println__T_uint64(t285)
    var t286_lhs uint64 = 17361641481138401520
    var t286_rhs uint64 = 1085102592571150095
    var t286 uint64 = t286_lhs | t286_rhs
    println__T_uint64(t286)
    var t287_lhs uint64 = 17361641481138401520
    var t287_rhs uint64 = 1085102592571150095
    var t287 uint64 = t287_lhs ^ t287_rhs
    println__T_uint64(t287)
    var t288_lhs uint64 = 1
    var t288_rhs int = 63
    var t288 uint64 = t288_lhs << t288_rhs
    println__T_uint64(t288)
    var t289_lhs uint64 = 9223372036854775808
    var t289_rhs int = 63
    var t289 uint64 = t289_lhs >> t289_rhs
    println__T_uint64(t289)
    var t290_operand uint64 = 0
    var t290 uint64 = ^t290_operand
    println__T_uint64(t290)
    return struct{}{}
}

func signed_ops() struct{} {
    var t293_lhs int8 = -13
    var t293_rhs int8 = 5
    var t293 int8 = t293_lhs % t293_rhs
    println__T_int8(t293)
    var t294_lhs int8 = -8
    var t294_rhs int = 2
    var t294 int8 = t294_lhs >> t294_rhs
    println__T_int8(t294)
    var t295_lhs int8 = 1
    var t295_rhs int = 6
    var t295 int8 = t295_lhs << t295_rhs
    println__T_int8(t295)
    var t296_operand int8 = 0
    var t296 int8 = ^t296_operand
    println__T_int8(t296)
    var t297_lhs int8 = -1
    var t297_rhs int = 7
    var t297 int8 = t297_lhs >> t297_rhs
    println__T_int8(t297)
    var t298_lhs int16 = -513
    var t298_rhs int16 = 256
    var t298 int16 = t298_lhs % t298_rhs
    println__T_int16(t298)
    var t299 int16 = -32767 - 1
    var t300_rhs int = 15
    var t300 int16 = t299 >> t300_rhs
    println__T_int16(t300)
    var t301_lhs int16 = 1
    var t301_rhs int = 14
    var t301 int16 = t301_lhs << t301_rhs
    println__T_int16(t301)
    var t302_operand int16 = 255
    var t302 int16 = ^t302_operand
    println__T_int16(t302)
    var t303_lhs int32 = -1000000001
    var t303_rhs int32 = 1000
    var t303 int32 = t303_lhs % t303_rhs
    println__T_int32(t303)
    var t304 int32 = -2147483647 - 1
    var t305_rhs int = 31
    var t305 int32 = t304 >> t305_rhs
    println__T_int32(t305)
    var t306_lhs int32 = 1
    var t306_rhs int = 30
    var t306 int32 = t306_lhs << t306_rhs
    println__T_int32(t306)
    var t307_operand int32 = 65535
    var t307 int32 = ^t307_operand
    println__T_int32(t307)
    var t308_lhs int64 = -1000000000001
    var t308_rhs int64 = 1000
    var t308 int64 = t308_lhs % t308_rhs
    println__T_int64(t308)
    var t309_lhs int64 = -9223372036854775807
    var t309_rhs int = 62
    var t309 int64 = t309_lhs >> t309_rhs
    println__T_int64(t309)
    var t310_lhs int64 = 1
    var t310_rhs int = 62
    var t310 int64 = t310_lhs << t310_rhs
    println__T_int64(t310)
    var t311_operand int64 = 4294967295
    var t311 int64 = ^t311_operand
    println__T_int64(t311)
    return struct{}{}
}

func precedence() struct{} {
    var t314_lhs uint8 = 3
    var t314_rhs uint8 = 1
    var t314 uint8 = t314_lhs & t314_rhs
    var t315_lhs uint8 = 2
    var t315 uint8 = t315_lhs ^ t314
    var t316_lhs uint8 = 1
    var t316 uint8 = t316_lhs | t315
    println__T_uint8(t316)
    var t317 int = 2 + 1
    var t318_lhs uint8 = 1
    var t318 uint8 = t318_lhs << t317
    println__T_uint8(t318)
    var t319_lhs int = 1
    var t319_rhs int = 2
    var t319 int = t319_lhs | t319_rhs
    var t320 bool
    var inline563 int = 3
    var inline564 bool = t319 == inline563
    t320 = inline564
    var t321 string
    var inline561 string = _goml_runtime_core_bool_to_string(t320)
    t321 = inline561
    var inline558 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t321)
    _goml_runtime_core_string_println(inline558)
    var t322_lhs int = 8
    var t322_rhs int = 1
    var t322 int = t322_lhs >> t322_rhs
    var t323 bool = t322 < 5
    var t324 string
    var inline556 string = _goml_runtime_core_bool_to_string(t323)
    t324 = inline556
    var inline553 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t324)
    _goml_runtime_core_string_println(inline553)
    var t325_operand uint8 = 1
    var t325 uint8 = ^t325_operand
    var t326_rhs uint8 = 15
    var t326 uint8 = t325 & t326_rhs
    println__T_uint8(t326)
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
    var t329 uint8 = uint8(uint16(five_eleven__8))
    println__T_uint8(t329)
    var t330 uint8 = uint8(uint16(two_fifty_six__9))
    println__T_uint8(t330)
    var t331 uint8 = uint8(int16(negative_one_i16__10))
    println__T_uint8(t331)
    var t332 int8 = int8(uint8(two_fifty_five__11))
    println__T_int8(t332)
    var t333 int8 = int8(uint8(one_twenty_eight__12))
    println__T_int8(t333)
    var t334 int8 = int8(int16(negative_one_twenty_nine__13))
    println__T_int8(t334)
    var t335 int16 = int16(uint16(max_u16__14))
    println__T_int16(t335)
    var t336 uint16 = uint16(int32(negative_one_i32__15))
    println__T_uint16(t336)
    var t337 uint64 = uint64(int8(negative_one_i8__16))
    println__T_uint64(t337)
    var t338 int32 = int32(uint64(max_u64__17))
    println__T_int32(t338)
    var t339 uint32 = uint32(uint8(sixty_five__18))
    println__T_uint32(t339)
    var t340 int64 = int64(uint32(max_u32__19))
    println__T_int64(t340)
    var t341_source rune = 65
    var t341 uint32 = uint32(rune(t341_source))
    println__T_uint32(t341)
    var mtmp238 Option__char
    var inline581 uint32 = 128512
    var inline582 bool = utf8_valid_scalar(inline581)
    if inline582 {
        var inline583 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(inline581)
        var inline585 rune = inline583._1
        var inline587 Option__char = Some{
            _0: inline585,
        }
        mtmp238 = inline587
    } else {
        mtmp238 = None{}
    }
    switch mtmp238.(type) {
    case None:
        var inline570 string = "invalid"
        var inline571 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline570)
        _goml_runtime_core_string_println(inline571)
    case Some:
        var x239 rune = mtmp238.(Some)._0
        var t347 string
        var inline577 string = char_to_string(x239)
        t347 = inline577
        var inline574 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t347)
        _goml_runtime_core_string_println(inline574)
    default:
        panic("non-exhaustive match")
    }
    var t343 uint8 = uint8(uint16(three_hundred__20))
    var t344 uint32 = uint32(uint8(t343))
    println__T_uint32(t344)
    return struct{}{}
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t357 uint8
    var inline617 uint8 = 10
    var inline618 uint8 = ^inline617
    var inline619_rhs uint8 = 15
    var inline619 uint8 = inline618 & inline619_rhs
    var inline620_lhs uint8 = 1
    var inline620_rhs int = 4
    var inline620 uint8 = inline620_lhs << inline620_rhs
    var inline621_rhs uint8 = 31
    var inline621 uint8 = inline620 % inline621_rhs
    var inline622 uint8 = inline619 | inline621
    t357 = inline622
    println__T_uint8(t357)
    return struct{}{}
}

func println__T_uint8(value__31 uint8) struct{} {
    var t360 string
    var inline624 string = _goml_runtime_core_uint8_to_string(value__31)
    t360 = inline624
    _goml_runtime_core_string_println(t360)
    return struct{}{}
}

func println__T_uint16(value__31 uint16) struct{} {
    var t363 string
    var inline626 string = _goml_runtime_core_uint16_to_string(value__31)
    t363 = inline626
    _goml_runtime_core_string_println(t363)
    return struct{}{}
}

func println__T_uint32(value__31 uint32) struct{} {
    var t366 string
    var inline628 string = _goml_runtime_core_uint32_to_string(value__31)
    t366 = inline628
    _goml_runtime_core_string_println(t366)
    return struct{}{}
}

func println__T_uint64(value__31 uint64) struct{} {
    var t369 string
    var inline630 string = _goml_runtime_core_uint64_to_string(value__31)
    t369 = inline630
    _goml_runtime_core_string_println(t369)
    return struct{}{}
}

func println__T_int8(value__31 int8) struct{} {
    var t372 string
    var inline632 string = _goml_runtime_core_int8_to_string(value__31)
    t372 = inline632
    _goml_runtime_core_string_println(t372)
    return struct{}{}
}

func println__T_int16(value__31 int16) struct{} {
    var t375 string
    var inline634 string = _goml_runtime_core_int16_to_string(value__31)
    t375 = inline634
    _goml_runtime_core_string_println(t375)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t378 string
    var inline636 string = _goml_runtime_core_int32_to_string(value__31)
    t378 = inline636
    _goml_runtime_core_string_println(t378)
    return struct{}{}
}

func println__T_int64(value__31 int64) struct{} {
    var t381 string
    var inline638 string = _goml_runtime_core_int64_to_string(value__31)
    t381 = inline638
    _goml_runtime_core_string_println(t381)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__74 uint8) string {
    var t403 string = _goml_runtime_core_uint8_to_string(self__74)
    return t403
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t431 bool = value__4 <= 1114111
    if t431 {
        var t435 bool = value__4 >= 55296
        var jp433 bool
        if t435 {
            var t436 bool = value__4 <= 57343
            jp433 = t436
        } else {
            jp433 = false
        }
        var t434 bool = !jp433
        return t434
    } else {
        return false
    }
}

func char_to_string(value__29 rune) string {
    var t441 uint32 = uint32(rune(value__29))
    var t442 bool
    var inline653 bool = t441 <= 1114111
    if inline653 {
        var inline654 bool = t441 >= 55296
        var inline656 bool
        if inline654 {
            var inline658 bool = t441 <= 57343
            inline656 = inline658
        } else {
            inline656 = false
        }
        var inline657 bool = !inline656
        t442 = inline657
    } else {
        t442 = false
    }
    if t442 {
        var t443 string = _goml_runtime_core_char_to_string(value__29)
        return t443
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
