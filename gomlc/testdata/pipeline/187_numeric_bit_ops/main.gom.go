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
    println__T_uint8(value__0)
    return struct{}{}
}

func show_u16(value__1 uint16) struct{} {
    println__T_uint16(value__1)
    return struct{}{}
}

func show_u32(value__2 uint32) struct{} {
    println__T_uint32(value__2)
    return struct{}{}
}

func show_u64(value__3 uint64) struct{} {
    println__T_uint64(value__3)
    return struct{}{}
}

func show_i8(value__4 int8) struct{} {
    println__T_int8(value__4)
    return struct{}{}
}

func show_i16(value__5 int16) struct{} {
    println__T_int16(value__5)
    return struct{}{}
}

func show_i32(value__6 int32) struct{} {
    println__T_int32(value__6)
    return struct{}{}
}

func show_i64(value__7 int64) struct{} {
    println__T_int64(value__7)
    return struct{}{}
}

func unsigned_ops() struct{} {
    var t237_lhs uint8 = 13
    var t237_rhs uint8 = 5
    var t237 uint8 = t237_lhs % t237_rhs
    show_u8(t237)
    var t238_lhs uint8 = 12
    var t238_rhs uint8 = 10
    var t238 uint8 = t238_lhs & t238_rhs
    show_u8(t238)
    var t239_lhs uint8 = 12
    var t239_rhs uint8 = 3
    var t239 uint8 = t239_lhs | t239_rhs
    show_u8(t239)
    var t240_lhs uint8 = 12
    var t240_rhs uint8 = 10
    var t240 uint8 = t240_lhs ^ t240_rhs
    show_u8(t240)
    var t241_lhs uint8 = 1
    var t241_rhs int = 7
    var t241 uint8 = t241_lhs << t241_rhs
    show_u8(t241)
    var t242_lhs uint8 = 128
    var t242_rhs int = 7
    var t242 uint8 = t242_lhs >> t242_rhs
    show_u8(t242)
    var t243_operand uint8 = 0
    var t243 uint8 = ^t243_operand
    show_u8(t243)
    var t244_lhs uint8 = 1
    var t244_rhs int = 8
    var t244 uint8 = t244_lhs << t244_rhs
    show_u8(t244)
    var t245_lhs uint16 = 513
    var t245_rhs uint16 = 256
    var t245 uint16 = t245_lhs % t245_rhs
    show_u16(t245)
    var t246_lhs uint16 = 3855
    var t246_rhs uint16 = 255
    var t246 uint16 = t246_lhs & t246_rhs
    show_u16(t246)
    var t247_lhs uint16 = 3840
    var t247_rhs uint16 = 15
    var t247 uint16 = t247_lhs | t247_rhs
    show_u16(t247)
    var t248_lhs uint16 = 43690
    var t248_rhs uint16 = 3855
    var t248 uint16 = t248_lhs ^ t248_rhs
    show_u16(t248)
    var t249_lhs uint16 = 1
    var t249_rhs int = 15
    var t249 uint16 = t249_lhs << t249_rhs
    show_u16(t249)
    var t250_lhs uint16 = 32768
    var t250_rhs int = 15
    var t250 uint16 = t250_lhs >> t250_rhs
    show_u16(t250)
    var t251_operand uint16 = 0
    var t251 uint16 = ^t251_operand
    show_u16(t251)
    var t252_lhs uint32 = 1000000001
    var t252_rhs uint32 = 1000
    var t252 uint32 = t252_lhs % t252_rhs
    show_u32(t252)
    var t253_lhs uint32 = 4042322160
    var t253_rhs uint32 = 252645135
    var t253 uint32 = t253_lhs & t253_rhs
    show_u32(t253)
    var t254_lhs uint32 = 4042322160
    var t254_rhs uint32 = 252645135
    var t254 uint32 = t254_lhs | t254_rhs
    show_u32(t254)
    var t255_lhs uint32 = 4042322160
    var t255_rhs uint32 = 252645135
    var t255 uint32 = t255_lhs ^ t255_rhs
    show_u32(t255)
    var t256_lhs uint32 = 1
    var t256_rhs int = 31
    var t256 uint32 = t256_lhs << t256_rhs
    show_u32(t256)
    var t257_lhs uint32 = 2147483648
    var t257_rhs int = 31
    var t257 uint32 = t257_lhs >> t257_rhs
    show_u32(t257)
    var t258_operand uint32 = 0
    var t258 uint32 = ^t258_operand
    show_u32(t258)
    var t259_lhs uint64 = 1000000000001
    var t259_rhs uint64 = 1000
    var t259 uint64 = t259_lhs % t259_rhs
    show_u64(t259)
    var t260_lhs uint64 = 17361641481138401520
    var t260_rhs uint64 = 1085102592571150095
    var t260 uint64 = t260_lhs & t260_rhs
    show_u64(t260)
    var t261_lhs uint64 = 17361641481138401520
    var t261_rhs uint64 = 1085102592571150095
    var t261 uint64 = t261_lhs | t261_rhs
    show_u64(t261)
    var t262_lhs uint64 = 17361641481138401520
    var t262_rhs uint64 = 1085102592571150095
    var t262 uint64 = t262_lhs ^ t262_rhs
    show_u64(t262)
    var t263_lhs uint64 = 1
    var t263_rhs int = 63
    var t263 uint64 = t263_lhs << t263_rhs
    show_u64(t263)
    var t264_lhs uint64 = 9223372036854775808
    var t264_rhs int = 63
    var t264 uint64 = t264_lhs >> t264_rhs
    show_u64(t264)
    var t265_operand uint64 = 0
    var t265 uint64 = ^t265_operand
    show_u64(t265)
    return struct{}{}
}

func signed_ops() struct{} {
    var t268_lhs int8 = -13
    var t268_rhs int8 = 5
    var t268 int8 = t268_lhs % t268_rhs
    show_i8(t268)
    var t269_lhs int8 = -8
    var t269_rhs int = 2
    var t269 int8 = t269_lhs >> t269_rhs
    show_i8(t269)
    var t270_lhs int8 = 1
    var t270_rhs int = 6
    var t270 int8 = t270_lhs << t270_rhs
    show_i8(t270)
    var t271_operand int8 = 0
    var t271 int8 = ^t271_operand
    show_i8(t271)
    var t272_lhs int8 = -1
    var t272_rhs int = 7
    var t272 int8 = t272_lhs >> t272_rhs
    show_i8(t272)
    var t273_lhs int16 = -513
    var t273_rhs int16 = 256
    var t273 int16 = t273_lhs % t273_rhs
    show_i16(t273)
    var t274 int16 = -32767 - 1
    var t275_rhs int = 15
    var t275 int16 = t274 >> t275_rhs
    show_i16(t275)
    var t276_lhs int16 = 1
    var t276_rhs int = 14
    var t276 int16 = t276_lhs << t276_rhs
    show_i16(t276)
    var t277_operand int16 = 255
    var t277 int16 = ^t277_operand
    show_i16(t277)
    var t278_lhs int32 = -1000000001
    var t278_rhs int32 = 1000
    var t278 int32 = t278_lhs % t278_rhs
    show_i32(t278)
    var t279 int32 = -2147483647 - 1
    var t280_rhs int = 31
    var t280 int32 = t279 >> t280_rhs
    show_i32(t280)
    var t281_lhs int32 = 1
    var t281_rhs int = 30
    var t281 int32 = t281_lhs << t281_rhs
    show_i32(t281)
    var t282_operand int32 = 65535
    var t282 int32 = ^t282_operand
    show_i32(t282)
    var t283_lhs int64 = -1000000000001
    var t283_rhs int64 = 1000
    var t283 int64 = t283_lhs % t283_rhs
    show_i64(t283)
    var t284_lhs int64 = -9223372036854775807
    var t284_rhs int = 62
    var t284 int64 = t284_lhs >> t284_rhs
    show_i64(t284)
    var t285_lhs int64 = 1
    var t285_rhs int = 62
    var t285 int64 = t285_lhs << t285_rhs
    show_i64(t285)
    var t286_operand int64 = 4294967295
    var t286 int64 = ^t286_operand
    show_i64(t286)
    return struct{}{}
}

func precedence() struct{} {
    var t289_lhs uint8 = 3
    var t289_rhs uint8 = 1
    var t289 uint8 = t289_lhs & t289_rhs
    var t290_lhs uint8 = 2
    var t290 uint8 = t290_lhs ^ t289
    var t291_lhs uint8 = 1
    var t291 uint8 = t291_lhs | t290
    show_u8(t291)
    var t292 int = 2 + 1
    var t293_lhs uint8 = 1
    var t293 uint8 = t293_lhs << t292
    show_u8(t293)
    var t294_lhs int = 1
    var t294_rhs int = 2
    var t294 int = t294_lhs | t294_rhs
    var t295 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t294, 3)
    var t296 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t295)
    println__T_string(t296)
    var t297_lhs int = 8
    var t297_rhs int = 1
    var t297 int = t297_lhs >> t297_rhs
    var t298 bool = t297 < 5
    var t299 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t298)
    println__T_string(t299)
    var t300_operand uint8 = 1
    var t300 uint8 = ^t300_operand
    var t301_rhs uint8 = 15
    var t301 uint8 = t300 & t301_rhs
    show_u8(t301)
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
    var t304 uint8 = uint8(uint16(five_eleven__8))
    show_u8(t304)
    var t305 uint8 = uint8(uint16(two_fifty_six__9))
    show_u8(t305)
    var t306 uint8 = uint8(int16(negative_one_i16__10))
    show_u8(t306)
    var t307 int8 = int8(uint8(two_fifty_five__11))
    show_i8(t307)
    var t308 int8 = int8(uint8(one_twenty_eight__12))
    show_i8(t308)
    var t309 int8 = int8(int16(negative_one_twenty_nine__13))
    show_i8(t309)
    var t310 int16 = int16(uint16(max_u16__14))
    show_i16(t310)
    var t311 uint16 = uint16(int32(negative_one_i32__15))
    show_u16(t311)
    var t312 uint64 = uint64(int8(negative_one_i8__16))
    show_u64(t312)
    var t313 int32 = int32(uint64(max_u64__17))
    show_i32(t313)
    var t314 uint32 = uint32(uint8(sixty_five__18))
    show_u32(t314)
    var t315 int64 = int64(uint32(max_u32__19))
    show_i64(t315)
    var t316_source rune = 65
    var t316 uint32 = uint32(rune(t316_source))
    show_u32(t316)
    var mtmp213 Option__char = char_from_uint32(128512)
    switch mtmp213.(type) {
    case None:
        println__T_string("invalid")
    case Some:
        var x214 rune = mtmp213.(Some)._0
        var value__21 rune = x214
        var t322 string = _goml_m_inherent_i_char_i_char_i_to__string(value__21)
        println__T_string(t322)
    default:
        panic("non-exhaustive match")
    }
    var t318 uint8 = uint8(uint16(three_hundred__20))
    var t319 uint32 = uint32(uint8(t318))
    show_u32(t319)
    return struct{}{}
}

func contextual(value__22 uint8) uint8 {
    var retv325 uint8
    var t326 uint8 = ^value__22
    var t327_rhs uint8 = 15
    var t327 uint8 = t326 & t327_rhs
    var t328_lhs uint8 = 1
    var t328_rhs int = 4
    var t328 uint8 = t328_lhs << t328_rhs
    var t329_rhs uint8 = 31
    var t329 uint8 = t328 % t329_rhs
    var t330 uint8 = t327 | t329
    retv325 = t330
    return retv325
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t332 uint8 = contextual(10)
    show_u8(t332)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t335 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t335)
    return struct{}{}
}

func println__T_uint16(value__1 uint16) struct{} {
    var t338 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(value__1)
    _goml_runtime_core_string_println(t338)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t341 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(value__1)
    _goml_runtime_core_string_println(t341)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t344 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t344)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t347 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    _goml_runtime_core_string_println(t347)
    return struct{}{}
}

func println__T_int16(value__1 int16) struct{} {
    var t350 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(value__1)
    _goml_runtime_core_string_println(t350)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t353 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t353)
    return struct{}{}
}

func println__T_int64(value__1 int64) struct{} {
    var t356 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(value__1)
    _goml_runtime_core_string_println(t356)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t359 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t359)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv362 bool
    var t363 bool = self__59 == other__60
    retv362 = t363
    return retv362
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv365 string
    var t366 string = _goml_runtime_core_bool_to_string(self__37)
    retv365 = t366
    return retv365
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv368 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp370 Option__char
    if valid__3 {
        var t371 Option__char = Some{
            _0: value__4,
        }
        jp370 = t371
    } else {
        jp370 = None{}
    }
    retv368 = jp370
    return retv368
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv373 string
    var t374 string = _goml_runtime_core_char_to_string(self__7)
    retv373 = t374
    return retv373
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv376 string
    var t377 string = _goml_runtime_core_uint8_to_string(self__45)
    retv376 = t377
    return retv376
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv379 string
    var t380 string = _goml_runtime_core_uint16_to_string(self__46)
    retv379 = t380
    return retv379
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv382 string
    var t383 string = _goml_runtime_core_uint32_to_string(self__47)
    retv382 = t383
    return retv382
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv385 string
    var t386 string = _goml_runtime_core_uint64_to_string(self__48)
    retv385 = t386
    return retv385
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var retv388 string
    var t389 string = _goml_runtime_core_int8_to_string(self__41)
    retv388 = t389
    return retv388
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv391 string
    var t392 string = _goml_runtime_core_int16_to_string(self__42)
    retv391 = t392
    return retv391
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv394 string
    var t395 string = _goml_runtime_core_int32_to_string(self__43)
    retv394 = t395
    return retv394
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv397 string
    var t398 string = _goml_runtime_core_int64_to_string(self__44)
    retv397 = t398
    return retv397
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv400 string
    retv400 = self__38
    return retv400
}

func main() {
    main0()
}
