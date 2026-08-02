package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    if value > 1114111 || value >= 55296 && value <= 57343 {
        return Tuple2_4bool_4char{
            _0: false,
            _1: 0,
        }
    }
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
    var inline405 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__0)
    _goml_runtime_core_string_println(inline405)
    return struct{}{}
}

func unsigned_ops() struct{} {
    var t240_lhs uint8 = 13
    var t240_rhs uint8 = 5
    var t240 uint8 = t240_lhs % t240_rhs
    show_u8(t240)
    var t241_lhs uint8 = 12
    var t241_rhs uint8 = 10
    var t241 uint8 = t241_lhs & t241_rhs
    show_u8(t241)
    var t242_lhs uint8 = 12
    var t242_rhs uint8 = 3
    var t242 uint8 = t242_lhs | t242_rhs
    show_u8(t242)
    var t243_lhs uint8 = 12
    var t243_rhs uint8 = 10
    var t243 uint8 = t243_lhs ^ t243_rhs
    show_u8(t243)
    var t244_lhs uint8 = 1
    var t244_rhs int = 7
    var t244 uint8 = t244_lhs << t244_rhs
    show_u8(t244)
    var t245_lhs uint8 = 128
    var t245_rhs int = 7
    var t245 uint8 = t245_lhs >> t245_rhs
    println__T_uint8(t245)
    var t246_operand uint8 = 0
    var t246 uint8 = ^t246_operand
    println__T_uint8(t246)
    var t247_lhs uint8 = 1
    var t247_rhs int = 8
    var t247 uint8 = t247_lhs << t247_rhs
    println__T_uint8(t247)
    var t248_lhs uint16 = 513
    var t248_rhs uint16 = 256
    var t248 uint16 = t248_lhs % t248_rhs
    println__T_uint16(t248)
    var t249_lhs uint16 = 3855
    var t249_rhs uint16 = 255
    var t249 uint16 = t249_lhs & t249_rhs
    println__T_uint16(t249)
    var t250_lhs uint16 = 3840
    var t250_rhs uint16 = 15
    var t250 uint16 = t250_lhs | t250_rhs
    println__T_uint16(t250)
    var t251_lhs uint16 = 43690
    var t251_rhs uint16 = 3855
    var t251 uint16 = t251_lhs ^ t251_rhs
    println__T_uint16(t251)
    var t252_lhs uint16 = 1
    var t252_rhs int = 15
    var t252 uint16 = t252_lhs << t252_rhs
    println__T_uint16(t252)
    var t253_lhs uint16 = 32768
    var t253_rhs int = 15
    var t253 uint16 = t253_lhs >> t253_rhs
    println__T_uint16(t253)
    var t254_operand uint16 = 0
    var t254 uint16 = ^t254_operand
    println__T_uint16(t254)
    var t255_lhs uint32 = 1000000001
    var t255_rhs uint32 = 1000
    var t255 uint32 = t255_lhs % t255_rhs
    println__T_uint32(t255)
    var t256_lhs uint32 = 4042322160
    var t256_rhs uint32 = 252645135
    var t256 uint32 = t256_lhs & t256_rhs
    println__T_uint32(t256)
    var t257_lhs uint32 = 4042322160
    var t257_rhs uint32 = 252645135
    var t257 uint32 = t257_lhs | t257_rhs
    println__T_uint32(t257)
    var t258_lhs uint32 = 4042322160
    var t258_rhs uint32 = 252645135
    var t258 uint32 = t258_lhs ^ t258_rhs
    println__T_uint32(t258)
    var t259_lhs uint32 = 1
    var t259_rhs int = 31
    var t259 uint32 = t259_lhs << t259_rhs
    println__T_uint32(t259)
    var t260_lhs uint32 = 2147483648
    var t260_rhs int = 31
    var t260 uint32 = t260_lhs >> t260_rhs
    println__T_uint32(t260)
    var t261_operand uint32 = 0
    var t261 uint32 = ^t261_operand
    println__T_uint32(t261)
    var t262_lhs uint64 = 1000000000001
    var t262_rhs uint64 = 1000
    var t262 uint64 = t262_lhs % t262_rhs
    println__T_uint64(t262)
    var t263_lhs uint64 = 17361641481138401520
    var t263_rhs uint64 = 1085102592571150095
    var t263 uint64 = t263_lhs & t263_rhs
    println__T_uint64(t263)
    var t264_lhs uint64 = 17361641481138401520
    var t264_rhs uint64 = 1085102592571150095
    var t264 uint64 = t264_lhs | t264_rhs
    println__T_uint64(t264)
    var t265_lhs uint64 = 17361641481138401520
    var t265_rhs uint64 = 1085102592571150095
    var t265 uint64 = t265_lhs ^ t265_rhs
    println__T_uint64(t265)
    var t266_lhs uint64 = 1
    var t266_rhs int = 63
    var t266 uint64 = t266_lhs << t266_rhs
    println__T_uint64(t266)
    var t267_lhs uint64 = 9223372036854775808
    var t267_rhs int = 63
    var t267 uint64 = t267_lhs >> t267_rhs
    println__T_uint64(t267)
    var t268_operand uint64 = 0
    var t268 uint64 = ^t268_operand
    println__T_uint64(t268)
    return struct{}{}
}

func signed_ops() struct{} {
    var t271_lhs int8 = -13
    var t271_rhs int8 = 5
    var t271 int8 = t271_lhs % t271_rhs
    println__T_int8(t271)
    var t272_lhs int8 = -8
    var t272_rhs int = 2
    var t272 int8 = t272_lhs >> t272_rhs
    println__T_int8(t272)
    var t273_lhs int8 = 1
    var t273_rhs int = 6
    var t273 int8 = t273_lhs << t273_rhs
    println__T_int8(t273)
    var t274_operand int8 = 0
    var t274 int8 = ^t274_operand
    println__T_int8(t274)
    var t275_lhs int8 = -1
    var t275_rhs int = 7
    var t275 int8 = t275_lhs >> t275_rhs
    println__T_int8(t275)
    var t276_lhs int16 = -513
    var t276_rhs int16 = 256
    var t276 int16 = t276_lhs % t276_rhs
    println__T_int16(t276)
    var t277 int16 = -32767 - 1
    var t278_rhs int = 15
    var t278 int16 = t277 >> t278_rhs
    println__T_int16(t278)
    var t279_lhs int16 = 1
    var t279_rhs int = 14
    var t279 int16 = t279_lhs << t279_rhs
    println__T_int16(t279)
    var t280_operand int16 = 255
    var t280 int16 = ^t280_operand
    println__T_int16(t280)
    var t281_lhs int32 = -1000000001
    var t281_rhs int32 = 1000
    var t281 int32 = t281_lhs % t281_rhs
    println__T_int32(t281)
    var t282 int32 = -2147483647 - 1
    var t283_rhs int = 31
    var t283 int32 = t282 >> t283_rhs
    println__T_int32(t283)
    var t284_lhs int32 = 1
    var t284_rhs int = 30
    var t284 int32 = t284_lhs << t284_rhs
    println__T_int32(t284)
    var t285_operand int32 = 65535
    var t285 int32 = ^t285_operand
    println__T_int32(t285)
    var t286_lhs int64 = -1000000000001
    var t286_rhs int64 = 1000
    var t286 int64 = t286_lhs % t286_rhs
    println__T_int64(t286)
    var t287_lhs int64 = -9223372036854775807
    var t287_rhs int = 62
    var t287 int64 = t287_lhs >> t287_rhs
    println__T_int64(t287)
    var t288_lhs int64 = 1
    var t288_rhs int = 62
    var t288 int64 = t288_lhs << t288_rhs
    println__T_int64(t288)
    var t289_operand int64 = 4294967295
    var t289 int64 = ^t289_operand
    println__T_int64(t289)
    return struct{}{}
}

func precedence() struct{} {
    var t292_lhs uint8 = 3
    var t292_rhs uint8 = 1
    var t292 uint8 = t292_lhs & t292_rhs
    var t293_lhs uint8 = 2
    var t293 uint8 = t293_lhs ^ t292
    var t294_lhs uint8 = 1
    var t294 uint8 = t294_lhs | t293
    println__T_uint8(t294)
    var t295 int = 2 + 1
    var t296_lhs uint8 = 1
    var t296 uint8 = t296_lhs << t295
    println__T_uint8(t296)
    var t297_lhs int = 1
    var t297_rhs int = 2
    var t297 int = t297_lhs | t297_rhs
    var t298 bool
    var inline523 int = 3
    var inline524 bool = t297 == inline523
    t298 = inline524
    var t299 string
    var inline521 string = _goml_runtime_core_bool_to_string(t298)
    t299 = inline521
    var inline518 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t299)
    _goml_runtime_core_string_println(inline518)
    var t300_lhs int = 8
    var t300_rhs int = 1
    var t300 int = t300_lhs >> t300_rhs
    var t301 bool = t300 < 5
    var t302 string
    var inline516 string = _goml_runtime_core_bool_to_string(t301)
    t302 = inline516
    var inline513 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t302)
    _goml_runtime_core_string_println(inline513)
    var t303_operand uint8 = 1
    var t303 uint8 = ^t303_operand
    var t304_rhs uint8 = 15
    var t304 uint8 = t303 & t304_rhs
    println__T_uint8(t304)
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
    var t307 uint8 = uint8(uint16(five_eleven__8))
    println__T_uint8(t307)
    var t308 uint8 = uint8(uint16(two_fifty_six__9))
    println__T_uint8(t308)
    var t309 uint8 = uint8(int16(negative_one_i16__10))
    println__T_uint8(t309)
    var t310 int8 = int8(uint8(two_fifty_five__11))
    println__T_int8(t310)
    var t311 int8 = int8(uint8(one_twenty_eight__12))
    println__T_int8(t311)
    var t312 int8 = int8(int16(negative_one_twenty_nine__13))
    println__T_int8(t312)
    var t313 int16 = int16(uint16(max_u16__14))
    println__T_int16(t313)
    var t314 uint16 = uint16(int32(negative_one_i32__15))
    println__T_uint16(t314)
    var t315 uint64 = uint64(int8(negative_one_i8__16))
    println__T_uint64(t315)
    var t316 int32 = int32(uint64(max_u64__17))
    println__T_int32(t316)
    var t317 uint32 = uint32(uint8(sixty_five__18))
    println__T_uint32(t317)
    var t318 int64 = int64(uint32(max_u32__19))
    println__T_int64(t318)
    var t319_source rune = 65
    var t319 uint32 = uint32(rune(t319_source))
    println__T_uint32(t319)
    var mtmp216 Option__char
    var inline541 uint32 = 128512
    var inline542 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(inline541)
    var inline543 bool = inline542._0
    var inline544 rune = inline542._1
    if inline543 {
        var inline547 Option__char = Some{
            _0: inline544,
        }
        mtmp216 = inline547
    } else {
        mtmp216 = None{}
    }
    switch mtmp216.(type) {
    case None:
        var inline530 string = "invalid"
        var inline531 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline530)
        _goml_runtime_core_string_println(inline531)
    case Some:
        var x217 rune = mtmp216.(Some)._0
        var t325 string
        var inline537 string = _goml_runtime_core_char_to_string(x217)
        t325 = inline537
        var inline534 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t325)
        _goml_runtime_core_string_println(inline534)
    default:
        panic("non-exhaustive match")
    }
    var t321 uint8 = uint8(uint16(three_hundred__20))
    var t322 uint32 = uint32(uint8(t321))
    println__T_uint32(t322)
    return struct{}{}
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t335 uint8
    var inline577 uint8 = 10
    var inline578 uint8 = ^inline577
    var inline579_rhs uint8 = 15
    var inline579 uint8 = inline578 & inline579_rhs
    var inline580_lhs uint8 = 1
    var inline580_rhs int = 4
    var inline580 uint8 = inline580_lhs << inline580_rhs
    var inline581_rhs uint8 = 31
    var inline581 uint8 = inline580 % inline581_rhs
    var inline582 uint8 = inline579 | inline581
    t335 = inline582
    println__T_uint8(t335)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t338 string
    var inline584 string = _goml_runtime_core_uint8_to_string(value__1)
    t338 = inline584
    _goml_runtime_core_string_println(t338)
    return struct{}{}
}

func println__T_uint16(value__1 uint16) struct{} {
    var t341 string
    var inline586 string = _goml_runtime_core_uint16_to_string(value__1)
    t341 = inline586
    _goml_runtime_core_string_println(t341)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t344 string
    var inline588 string = _goml_runtime_core_uint32_to_string(value__1)
    t344 = inline588
    _goml_runtime_core_string_println(t344)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t347 string
    var inline590 string = _goml_runtime_core_uint64_to_string(value__1)
    t347 = inline590
    _goml_runtime_core_string_println(t347)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t350 string
    var inline592 string = _goml_runtime_core_int8_to_string(value__1)
    t350 = inline592
    _goml_runtime_core_string_println(t350)
    return struct{}{}
}

func println__T_int16(value__1 int16) struct{} {
    var t353 string
    var inline594 string = _goml_runtime_core_int16_to_string(value__1)
    t353 = inline594
    _goml_runtime_core_string_println(t353)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t356 string
    var inline596 string = _goml_runtime_core_int32_to_string(value__1)
    t356 = inline596
    _goml_runtime_core_string_println(t356)
    return struct{}{}
}

func println__T_int64(value__1 int64) struct{} {
    var t359 string
    var inline598 string = _goml_runtime_core_int64_to_string(value__1)
    t359 = inline598
    _goml_runtime_core_string_println(t359)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var t380 string = _goml_runtime_core_uint8_to_string(self__45)
    return t380
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
