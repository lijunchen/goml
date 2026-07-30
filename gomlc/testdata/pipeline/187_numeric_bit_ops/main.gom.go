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
    var t153_lhs uint8 = 13
    var t153_rhs uint8 = 5
    var t153 uint8 = t153_lhs % t153_rhs
    show_u8(t153)
    var t154_lhs uint8 = 12
    var t154_rhs uint8 = 10
    var t154 uint8 = t154_lhs & t154_rhs
    show_u8(t154)
    var t155_lhs uint8 = 12
    var t155_rhs uint8 = 3
    var t155 uint8 = t155_lhs | t155_rhs
    show_u8(t155)
    var t156_lhs uint8 = 12
    var t156_rhs uint8 = 10
    var t156 uint8 = t156_lhs ^ t156_rhs
    show_u8(t156)
    var t157_lhs uint8 = 1
    var t157_rhs int = 7
    var t157 uint8 = t157_lhs << t157_rhs
    show_u8(t157)
    var t158_lhs uint8 = 128
    var t158_rhs int = 7
    var t158 uint8 = t158_lhs >> t158_rhs
    show_u8(t158)
    var t159_operand uint8 = 0
    var t159 uint8 = ^t159_operand
    show_u8(t159)
    var t160_lhs uint8 = 1
    var t160_rhs int = 8
    var t160 uint8 = t160_lhs << t160_rhs
    show_u8(t160)
    var t161_lhs uint16 = 513
    var t161_rhs uint16 = 256
    var t161 uint16 = t161_lhs % t161_rhs
    show_u16(t161)
    var t162_lhs uint16 = 3855
    var t162_rhs uint16 = 255
    var t162 uint16 = t162_lhs & t162_rhs
    show_u16(t162)
    var t163_lhs uint16 = 3840
    var t163_rhs uint16 = 15
    var t163 uint16 = t163_lhs | t163_rhs
    show_u16(t163)
    var t164_lhs uint16 = 43690
    var t164_rhs uint16 = 3855
    var t164 uint16 = t164_lhs ^ t164_rhs
    show_u16(t164)
    var t165_lhs uint16 = 1
    var t165_rhs int = 15
    var t165 uint16 = t165_lhs << t165_rhs
    show_u16(t165)
    var t166_lhs uint16 = 32768
    var t166_rhs int = 15
    var t166 uint16 = t166_lhs >> t166_rhs
    show_u16(t166)
    var t167_operand uint16 = 0
    var t167 uint16 = ^t167_operand
    show_u16(t167)
    var t168_lhs uint32 = 1000000001
    var t168_rhs uint32 = 1000
    var t168 uint32 = t168_lhs % t168_rhs
    show_u32(t168)
    var t169_lhs uint32 = 4042322160
    var t169_rhs uint32 = 252645135
    var t169 uint32 = t169_lhs & t169_rhs
    show_u32(t169)
    var t170_lhs uint32 = 4042322160
    var t170_rhs uint32 = 252645135
    var t170 uint32 = t170_lhs | t170_rhs
    show_u32(t170)
    var t171_lhs uint32 = 4042322160
    var t171_rhs uint32 = 252645135
    var t171 uint32 = t171_lhs ^ t171_rhs
    show_u32(t171)
    var t172_lhs uint32 = 1
    var t172_rhs int = 31
    var t172 uint32 = t172_lhs << t172_rhs
    show_u32(t172)
    var t173_lhs uint32 = 2147483648
    var t173_rhs int = 31
    var t173 uint32 = t173_lhs >> t173_rhs
    show_u32(t173)
    var t174_operand uint32 = 0
    var t174 uint32 = ^t174_operand
    show_u32(t174)
    var t175_lhs uint64 = 1000000000001
    var t175_rhs uint64 = 1000
    var t175 uint64 = t175_lhs % t175_rhs
    show_u64(t175)
    var t176_lhs uint64 = 17361641481138401520
    var t176_rhs uint64 = 1085102592571150095
    var t176 uint64 = t176_lhs & t176_rhs
    show_u64(t176)
    var t177_lhs uint64 = 17361641481138401520
    var t177_rhs uint64 = 1085102592571150095
    var t177 uint64 = t177_lhs | t177_rhs
    show_u64(t177)
    var t178_lhs uint64 = 17361641481138401520
    var t178_rhs uint64 = 1085102592571150095
    var t178 uint64 = t178_lhs ^ t178_rhs
    show_u64(t178)
    var t179_lhs uint64 = 1
    var t179_rhs int = 63
    var t179 uint64 = t179_lhs << t179_rhs
    show_u64(t179)
    var t180_lhs uint64 = 9223372036854775808
    var t180_rhs int = 63
    var t180 uint64 = t180_lhs >> t180_rhs
    show_u64(t180)
    var t181_operand uint64 = 0
    var t181 uint64 = ^t181_operand
    show_u64(t181)
    return struct{}{}
}

func signed_ops() struct{} {
    var t184_lhs int8 = -13
    var t184_rhs int8 = 5
    var t184 int8 = t184_lhs % t184_rhs
    show_i8(t184)
    var t185_lhs int8 = -8
    var t185_rhs int = 2
    var t185 int8 = t185_lhs >> t185_rhs
    show_i8(t185)
    var t186_lhs int8 = 1
    var t186_rhs int = 6
    var t186 int8 = t186_lhs << t186_rhs
    show_i8(t186)
    var t187_operand int8 = 0
    var t187 int8 = ^t187_operand
    show_i8(t187)
    var t188_lhs int8 = -1
    var t188_rhs int = 7
    var t188 int8 = t188_lhs >> t188_rhs
    show_i8(t188)
    var t189_lhs int16 = -513
    var t189_rhs int16 = 256
    var t189 int16 = t189_lhs % t189_rhs
    show_i16(t189)
    var t190 int16 = -32767 - 1
    var t191_rhs int = 15
    var t191 int16 = t190 >> t191_rhs
    show_i16(t191)
    var t192_lhs int16 = 1
    var t192_rhs int = 14
    var t192 int16 = t192_lhs << t192_rhs
    show_i16(t192)
    var t193_operand int16 = 255
    var t193 int16 = ^t193_operand
    show_i16(t193)
    var t194_lhs int32 = -1000000001
    var t194_rhs int32 = 1000
    var t194 int32 = t194_lhs % t194_rhs
    show_i32(t194)
    var t195 int32 = -2147483647 - 1
    var t196_rhs int = 31
    var t196 int32 = t195 >> t196_rhs
    show_i32(t196)
    var t197_lhs int32 = 1
    var t197_rhs int = 30
    var t197 int32 = t197_lhs << t197_rhs
    show_i32(t197)
    var t198_operand int32 = 65535
    var t198 int32 = ^t198_operand
    show_i32(t198)
    var t199_lhs int64 = -1000000000001
    var t199_rhs int64 = 1000
    var t199 int64 = t199_lhs % t199_rhs
    show_i64(t199)
    var t200_lhs int64 = -9223372036854775807
    var t200_rhs int = 62
    var t200 int64 = t200_lhs >> t200_rhs
    show_i64(t200)
    var t201_lhs int64 = 1
    var t201_rhs int = 62
    var t201 int64 = t201_lhs << t201_rhs
    show_i64(t201)
    var t202_operand int64 = 4294967295
    var t202 int64 = ^t202_operand
    show_i64(t202)
    return struct{}{}
}

func precedence() struct{} {
    var t205_lhs uint8 = 3
    var t205_rhs uint8 = 1
    var t205 uint8 = t205_lhs & t205_rhs
    var t206_lhs uint8 = 2
    var t206 uint8 = t206_lhs ^ t205
    var t207_lhs uint8 = 1
    var t207 uint8 = t207_lhs | t206
    show_u8(t207)
    var t208 int = 2 + 1
    var t209_lhs uint8 = 1
    var t209 uint8 = t209_lhs << t208
    show_u8(t209)
    var t210_lhs int = 1
    var t210_rhs int = 2
    var t210 int = t210_lhs | t210_rhs
    var t211 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t210, 3)
    var t212 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t211)
    println__T_string(t212)
    var t213_lhs int = 8
    var t213_rhs int = 1
    var t213 int = t213_lhs >> t213_rhs
    var t214 bool = t213 < 5
    var t215 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t214)
    println__T_string(t215)
    var t216_operand uint8 = 1
    var t216 uint8 = ^t216_operand
    var t217_rhs uint8 = 15
    var t217 uint8 = t216 & t217_rhs
    show_u8(t217)
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
    var t220 uint8 = uint8(uint16(five_eleven__8))
    show_u8(t220)
    var t221 uint8 = uint8(uint16(two_fifty_six__9))
    show_u8(t221)
    var t222 uint8 = uint8(int16(negative_one_i16__10))
    show_u8(t222)
    var t223 int8 = int8(uint8(two_fifty_five__11))
    show_i8(t223)
    var t224 int8 = int8(uint8(one_twenty_eight__12))
    show_i8(t224)
    var t225 int8 = int8(int16(negative_one_twenty_nine__13))
    show_i8(t225)
    var t226 int16 = int16(uint16(max_u16__14))
    show_i16(t226)
    var t227 uint16 = uint16(int32(negative_one_i32__15))
    show_u16(t227)
    var t228 uint64 = uint64(int8(negative_one_i8__16))
    show_u64(t228)
    var t229 int32 = int32(uint64(max_u64__17))
    show_i32(t229)
    var t230 uint32 = uint32(uint8(sixty_five__18))
    show_u32(t230)
    var t231 int64 = int64(uint32(max_u32__19))
    show_i64(t231)
    var t232_source rune = 65
    var t232 uint32 = uint32(rune(t232_source))
    show_u32(t232)
    var mtmp129 Option__char = char_from_uint32(128512)
    switch mtmp129.(type) {
    case None:
        println__T_string("invalid")
    case Some:
        var x130 rune = mtmp129.(Some)._0
        var value__21 rune = x130
        var t238 string = _goml_m_inherent_i_char_i_char_i_to__string(value__21)
        println__T_string(t238)
    default:
        panic("non-exhaustive match")
    }
    var t234 uint8 = uint8(uint16(three_hundred__20))
    var t235 uint32 = uint32(uint8(t234))
    show_u32(t235)
    return struct{}{}
}

func contextual(value__22 uint8) uint8 {
    var retv241 uint8
    var t242 uint8 = ^value__22
    var t243_rhs uint8 = 15
    var t243 uint8 = t242 & t243_rhs
    var t244_lhs uint8 = 1
    var t244_rhs int = 4
    var t244 uint8 = t244_lhs << t244_rhs
    var t245_rhs uint8 = 31
    var t245 uint8 = t244 % t245_rhs
    var t246 uint8 = t243 | t245
    retv241 = t246
    return retv241
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t248 uint8 = contextual(10)
    show_u8(t248)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t251 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t251)
    return struct{}{}
}

func println__T_uint16(value__1 uint16) struct{} {
    var t254 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(value__1)
    _goml_runtime_core_string_println(t254)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t257 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(value__1)
    _goml_runtime_core_string_println(t257)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t260 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t260)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t263 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    _goml_runtime_core_string_println(t263)
    return struct{}{}
}

func println__T_int16(value__1 int16) struct{} {
    var t266 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(value__1)
    _goml_runtime_core_string_println(t266)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t269 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t269)
    return struct{}{}
}

func println__T_int64(value__1 int64) struct{} {
    var t272 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(value__1)
    _goml_runtime_core_string_println(t272)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t275)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv278 bool
    var t279 bool = self__59 == other__60
    retv278 = t279
    return retv278
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv281 string
    var t282 string = _goml_runtime_core_bool_to_string(self__37)
    retv281 = t282
    return retv281
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv284 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp286 Option__char
    if valid__3 {
        var t287 Option__char = Some{
            _0: value__4,
        }
        jp286 = t287
    } else {
        jp286 = None{}
    }
    retv284 = jp286
    return retv284
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv289 string
    var t290 string = _goml_runtime_core_char_to_string(self__7)
    retv289 = t290
    return retv289
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv292 string
    var t293 string = _goml_runtime_core_uint8_to_string(self__45)
    retv292 = t293
    return retv292
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv295 string
    var t296 string = _goml_runtime_core_uint16_to_string(self__46)
    retv295 = t296
    return retv295
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv298 string
    var t299 string = _goml_runtime_core_uint32_to_string(self__47)
    retv298 = t299
    return retv298
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv301 string
    var t302 string = _goml_runtime_core_uint64_to_string(self__48)
    retv301 = t302
    return retv301
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var retv304 string
    var t305 string = _goml_runtime_core_int8_to_string(self__41)
    retv304 = t305
    return retv304
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv307 string
    var t308 string = _goml_runtime_core_int16_to_string(self__42)
    retv307 = t308
    return retv307
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv310 string
    var t311 string = _goml_runtime_core_int32_to_string(self__43)
    retv310 = t311
    return retv310
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv313 string
    var t314 string = _goml_runtime_core_int64_to_string(self__44)
    retv313 = t314
    return retv313
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv316 string
    retv316 = self__38
    return retv316
}

func main() {
    main0()
}
