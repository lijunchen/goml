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
    var t193_lhs uint8 = 13
    var t193_rhs uint8 = 5
    var t193 uint8 = t193_lhs % t193_rhs
    show_u8(t193)
    var t194_lhs uint8 = 12
    var t194_rhs uint8 = 10
    var t194 uint8 = t194_lhs & t194_rhs
    show_u8(t194)
    var t195_lhs uint8 = 12
    var t195_rhs uint8 = 3
    var t195 uint8 = t195_lhs | t195_rhs
    show_u8(t195)
    var t196_lhs uint8 = 12
    var t196_rhs uint8 = 10
    var t196 uint8 = t196_lhs ^ t196_rhs
    show_u8(t196)
    var t197_lhs uint8 = 1
    var t197_rhs int = 7
    var t197 uint8 = t197_lhs << t197_rhs
    show_u8(t197)
    var t198_lhs uint8 = 128
    var t198_rhs int = 7
    var t198 uint8 = t198_lhs >> t198_rhs
    show_u8(t198)
    var t199_operand uint8 = 0
    var t199 uint8 = ^t199_operand
    show_u8(t199)
    var t200_lhs uint8 = 1
    var t200_rhs int = 8
    var t200 uint8 = t200_lhs << t200_rhs
    show_u8(t200)
    var t201_lhs uint16 = 513
    var t201_rhs uint16 = 256
    var t201 uint16 = t201_lhs % t201_rhs
    show_u16(t201)
    var t202_lhs uint16 = 3855
    var t202_rhs uint16 = 255
    var t202 uint16 = t202_lhs & t202_rhs
    show_u16(t202)
    var t203_lhs uint16 = 3840
    var t203_rhs uint16 = 15
    var t203 uint16 = t203_lhs | t203_rhs
    show_u16(t203)
    var t204_lhs uint16 = 43690
    var t204_rhs uint16 = 3855
    var t204 uint16 = t204_lhs ^ t204_rhs
    show_u16(t204)
    var t205_lhs uint16 = 1
    var t205_rhs int = 15
    var t205 uint16 = t205_lhs << t205_rhs
    show_u16(t205)
    var t206_lhs uint16 = 32768
    var t206_rhs int = 15
    var t206 uint16 = t206_lhs >> t206_rhs
    show_u16(t206)
    var t207_operand uint16 = 0
    var t207 uint16 = ^t207_operand
    show_u16(t207)
    var t208_lhs uint32 = 1000000001
    var t208_rhs uint32 = 1000
    var t208 uint32 = t208_lhs % t208_rhs
    show_u32(t208)
    var t209_lhs uint32 = 4042322160
    var t209_rhs uint32 = 252645135
    var t209 uint32 = t209_lhs & t209_rhs
    show_u32(t209)
    var t210_lhs uint32 = 4042322160
    var t210_rhs uint32 = 252645135
    var t210 uint32 = t210_lhs | t210_rhs
    show_u32(t210)
    var t211_lhs uint32 = 4042322160
    var t211_rhs uint32 = 252645135
    var t211 uint32 = t211_lhs ^ t211_rhs
    show_u32(t211)
    var t212_lhs uint32 = 1
    var t212_rhs int = 31
    var t212 uint32 = t212_lhs << t212_rhs
    show_u32(t212)
    var t213_lhs uint32 = 2147483648
    var t213_rhs int = 31
    var t213 uint32 = t213_lhs >> t213_rhs
    show_u32(t213)
    var t214_operand uint32 = 0
    var t214 uint32 = ^t214_operand
    show_u32(t214)
    var t215_lhs uint64 = 1000000000001
    var t215_rhs uint64 = 1000
    var t215 uint64 = t215_lhs % t215_rhs
    show_u64(t215)
    var t216_lhs uint64 = 17361641481138401520
    var t216_rhs uint64 = 1085102592571150095
    var t216 uint64 = t216_lhs & t216_rhs
    show_u64(t216)
    var t217_lhs uint64 = 17361641481138401520
    var t217_rhs uint64 = 1085102592571150095
    var t217 uint64 = t217_lhs | t217_rhs
    show_u64(t217)
    var t218_lhs uint64 = 17361641481138401520
    var t218_rhs uint64 = 1085102592571150095
    var t218 uint64 = t218_lhs ^ t218_rhs
    show_u64(t218)
    var t219_lhs uint64 = 1
    var t219_rhs int = 63
    var t219 uint64 = t219_lhs << t219_rhs
    show_u64(t219)
    var t220_lhs uint64 = 9223372036854775808
    var t220_rhs int = 63
    var t220 uint64 = t220_lhs >> t220_rhs
    show_u64(t220)
    var t221_operand uint64 = 0
    var t221 uint64 = ^t221_operand
    show_u64(t221)
    return struct{}{}
}

func signed_ops() struct{} {
    var t224_lhs int8 = -13
    var t224_rhs int8 = 5
    var t224 int8 = t224_lhs % t224_rhs
    show_i8(t224)
    var t225_lhs int8 = -8
    var t225_rhs int = 2
    var t225 int8 = t225_lhs >> t225_rhs
    show_i8(t225)
    var t226_lhs int8 = 1
    var t226_rhs int = 6
    var t226 int8 = t226_lhs << t226_rhs
    show_i8(t226)
    var t227_operand int8 = 0
    var t227 int8 = ^t227_operand
    show_i8(t227)
    var t228_lhs int8 = -1
    var t228_rhs int = 7
    var t228 int8 = t228_lhs >> t228_rhs
    show_i8(t228)
    var t229_lhs int16 = -513
    var t229_rhs int16 = 256
    var t229 int16 = t229_lhs % t229_rhs
    show_i16(t229)
    var t230 int16 = -32767 - 1
    var t231_rhs int = 15
    var t231 int16 = t230 >> t231_rhs
    show_i16(t231)
    var t232_lhs int16 = 1
    var t232_rhs int = 14
    var t232 int16 = t232_lhs << t232_rhs
    show_i16(t232)
    var t233_operand int16 = 255
    var t233 int16 = ^t233_operand
    show_i16(t233)
    var t234_lhs int32 = -1000000001
    var t234_rhs int32 = 1000
    var t234 int32 = t234_lhs % t234_rhs
    show_i32(t234)
    var t235 int32 = -2147483647 - 1
    var t236_rhs int = 31
    var t236 int32 = t235 >> t236_rhs
    show_i32(t236)
    var t237_lhs int32 = 1
    var t237_rhs int = 30
    var t237 int32 = t237_lhs << t237_rhs
    show_i32(t237)
    var t238_operand int32 = 65535
    var t238 int32 = ^t238_operand
    show_i32(t238)
    var t239_lhs int64 = -1000000000001
    var t239_rhs int64 = 1000
    var t239 int64 = t239_lhs % t239_rhs
    show_i64(t239)
    var t240_lhs int64 = -9223372036854775807
    var t240_rhs int = 62
    var t240 int64 = t240_lhs >> t240_rhs
    show_i64(t240)
    var t241_lhs int64 = 1
    var t241_rhs int = 62
    var t241 int64 = t241_lhs << t241_rhs
    show_i64(t241)
    var t242_operand int64 = 4294967295
    var t242 int64 = ^t242_operand
    show_i64(t242)
    return struct{}{}
}

func precedence() struct{} {
    var t245_lhs uint8 = 3
    var t245_rhs uint8 = 1
    var t245 uint8 = t245_lhs & t245_rhs
    var t246_lhs uint8 = 2
    var t246 uint8 = t246_lhs ^ t245
    var t247_lhs uint8 = 1
    var t247 uint8 = t247_lhs | t246
    show_u8(t247)
    var t248 int = 2 + 1
    var t249_lhs uint8 = 1
    var t249 uint8 = t249_lhs << t248
    show_u8(t249)
    var t250_lhs int = 1
    var t250_rhs int = 2
    var t250 int = t250_lhs | t250_rhs
    var t251 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t250, 3)
    var t252 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t251)
    println__T_string(t252)
    var t253_lhs int = 8
    var t253_rhs int = 1
    var t253 int = t253_lhs >> t253_rhs
    var t254 bool = t253 < 5
    var t255 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t254)
    println__T_string(t255)
    var t256_operand uint8 = 1
    var t256 uint8 = ^t256_operand
    var t257_rhs uint8 = 15
    var t257 uint8 = t256 & t257_rhs
    show_u8(t257)
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
    var t260 uint8 = uint8(uint16(five_eleven__8))
    show_u8(t260)
    var t261 uint8 = uint8(uint16(two_fifty_six__9))
    show_u8(t261)
    var t262 uint8 = uint8(int16(negative_one_i16__10))
    show_u8(t262)
    var t263 int8 = int8(uint8(two_fifty_five__11))
    show_i8(t263)
    var t264 int8 = int8(uint8(one_twenty_eight__12))
    show_i8(t264)
    var t265 int8 = int8(int16(negative_one_twenty_nine__13))
    show_i8(t265)
    var t266 int16 = int16(uint16(max_u16__14))
    show_i16(t266)
    var t267 uint16 = uint16(int32(negative_one_i32__15))
    show_u16(t267)
    var t268 uint64 = uint64(int8(negative_one_i8__16))
    show_u64(t268)
    var t269 int32 = int32(uint64(max_u64__17))
    show_i32(t269)
    var t270 uint32 = uint32(uint8(sixty_five__18))
    show_u32(t270)
    var t271 int64 = int64(uint32(max_u32__19))
    show_i64(t271)
    var t272_source rune = 65
    var t272 uint32 = uint32(rune(t272_source))
    show_u32(t272)
    var mtmp169 Option__char = char_from_uint32(128512)
    switch mtmp169.(type) {
    case None:
        println__T_string("invalid")
    case Some:
        var x170 rune = mtmp169.(Some)._0
        var value__21 rune = x170
        var t278 string = _goml_m_inherent_i_char_i_char_i_to__string(value__21)
        println__T_string(t278)
    default:
        panic("non-exhaustive match")
    }
    var t274 uint8 = uint8(uint16(three_hundred__20))
    var t275 uint32 = uint32(uint8(t274))
    show_u32(t275)
    return struct{}{}
}

func contextual(value__22 uint8) uint8 {
    var retv281 uint8
    var t282 uint8 = ^value__22
    var t283_rhs uint8 = 15
    var t283 uint8 = t282 & t283_rhs
    var t284_lhs uint8 = 1
    var t284_rhs int = 4
    var t284 uint8 = t284_lhs << t284_rhs
    var t285_rhs uint8 = 31
    var t285 uint8 = t284 % t285_rhs
    var t286 uint8 = t283 | t285
    retv281 = t286
    return retv281
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t288 uint8 = contextual(10)
    show_u8(t288)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t291 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t291)
    return struct{}{}
}

func println__T_uint16(value__1 uint16) struct{} {
    var t294 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(value__1)
    _goml_runtime_core_string_println(t294)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t297 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(value__1)
    _goml_runtime_core_string_println(t297)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t300 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t300)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t303 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    _goml_runtime_core_string_println(t303)
    return struct{}{}
}

func println__T_int16(value__1 int16) struct{} {
    var t306 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(value__1)
    _goml_runtime_core_string_println(t306)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t309 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t309)
    return struct{}{}
}

func println__T_int64(value__1 int64) struct{} {
    var t312 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(value__1)
    _goml_runtime_core_string_println(t312)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t315 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t315)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv318 bool
    var t319 bool = self__59 == other__60
    retv318 = t319
    return retv318
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv321 string
    var t322 string = _goml_runtime_core_bool_to_string(self__37)
    retv321 = t322
    return retv321
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv324 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp326 Option__char
    if valid__3 {
        var t327 Option__char = Some{
            _0: value__4,
        }
        jp326 = t327
    } else {
        jp326 = None{}
    }
    retv324 = jp326
    return retv324
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv329 string
    var t330 string = _goml_runtime_core_char_to_string(self__7)
    retv329 = t330
    return retv329
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv332 string
    var t333 string = _goml_runtime_core_uint8_to_string(self__45)
    retv332 = t333
    return retv332
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv335 string
    var t336 string = _goml_runtime_core_uint16_to_string(self__46)
    retv335 = t336
    return retv335
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv338 string
    var t339 string = _goml_runtime_core_uint32_to_string(self__47)
    retv338 = t339
    return retv338
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv341 string
    var t342 string = _goml_runtime_core_uint64_to_string(self__48)
    retv341 = t342
    return retv341
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var retv344 string
    var t345 string = _goml_runtime_core_int8_to_string(self__41)
    retv344 = t345
    return retv344
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv347 string
    var t348 string = _goml_runtime_core_int16_to_string(self__42)
    retv347 = t348
    return retv347
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv350 string
    var t351 string = _goml_runtime_core_int32_to_string(self__43)
    retv350 = t351
    return retv350
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv353 string
    var t354 string = _goml_runtime_core_int64_to_string(self__44)
    retv353 = t354
    return retv353
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv356 string
    retv356 = self__38
    return retv356
}

func main() {
    main0()
}
