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
    var t149_lhs uint8 = 13
    var t149_rhs uint8 = 5
    var t149 uint8 = t149_lhs % t149_rhs
    show_u8(t149)
    var t150_lhs uint8 = 12
    var t150_rhs uint8 = 10
    var t150 uint8 = t150_lhs & t150_rhs
    show_u8(t150)
    var t151_lhs uint8 = 12
    var t151_rhs uint8 = 3
    var t151 uint8 = t151_lhs | t151_rhs
    show_u8(t151)
    var t152_lhs uint8 = 12
    var t152_rhs uint8 = 10
    var t152 uint8 = t152_lhs ^ t152_rhs
    show_u8(t152)
    var t153_lhs uint8 = 1
    var t153_rhs int = 7
    var t153 uint8 = t153_lhs << t153_rhs
    show_u8(t153)
    var t154_lhs uint8 = 128
    var t154_rhs int = 7
    var t154 uint8 = t154_lhs >> t154_rhs
    show_u8(t154)
    var t155_operand uint8 = 0
    var t155 uint8 = ^t155_operand
    show_u8(t155)
    var t156_lhs uint8 = 1
    var t156_rhs int = 8
    var t156 uint8 = t156_lhs << t156_rhs
    show_u8(t156)
    var t157_lhs uint16 = 513
    var t157_rhs uint16 = 256
    var t157 uint16 = t157_lhs % t157_rhs
    show_u16(t157)
    var t158_lhs uint16 = 3855
    var t158_rhs uint16 = 255
    var t158 uint16 = t158_lhs & t158_rhs
    show_u16(t158)
    var t159_lhs uint16 = 3840
    var t159_rhs uint16 = 15
    var t159 uint16 = t159_lhs | t159_rhs
    show_u16(t159)
    var t160_lhs uint16 = 43690
    var t160_rhs uint16 = 3855
    var t160 uint16 = t160_lhs ^ t160_rhs
    show_u16(t160)
    var t161_lhs uint16 = 1
    var t161_rhs int = 15
    var t161 uint16 = t161_lhs << t161_rhs
    show_u16(t161)
    var t162_lhs uint16 = 32768
    var t162_rhs int = 15
    var t162 uint16 = t162_lhs >> t162_rhs
    show_u16(t162)
    var t163_operand uint16 = 0
    var t163 uint16 = ^t163_operand
    show_u16(t163)
    var t164_lhs uint32 = 1000000001
    var t164_rhs uint32 = 1000
    var t164 uint32 = t164_lhs % t164_rhs
    show_u32(t164)
    var t165_lhs uint32 = 4042322160
    var t165_rhs uint32 = 252645135
    var t165 uint32 = t165_lhs & t165_rhs
    show_u32(t165)
    var t166_lhs uint32 = 4042322160
    var t166_rhs uint32 = 252645135
    var t166 uint32 = t166_lhs | t166_rhs
    show_u32(t166)
    var t167_lhs uint32 = 4042322160
    var t167_rhs uint32 = 252645135
    var t167 uint32 = t167_lhs ^ t167_rhs
    show_u32(t167)
    var t168_lhs uint32 = 1
    var t168_rhs int = 31
    var t168 uint32 = t168_lhs << t168_rhs
    show_u32(t168)
    var t169_lhs uint32 = 2147483648
    var t169_rhs int = 31
    var t169 uint32 = t169_lhs >> t169_rhs
    show_u32(t169)
    var t170_operand uint32 = 0
    var t170 uint32 = ^t170_operand
    show_u32(t170)
    var t171_lhs uint64 = 1000000000001
    var t171_rhs uint64 = 1000
    var t171 uint64 = t171_lhs % t171_rhs
    show_u64(t171)
    var t172_lhs uint64 = 17361641481138401520
    var t172_rhs uint64 = 1085102592571150095
    var t172 uint64 = t172_lhs & t172_rhs
    show_u64(t172)
    var t173_lhs uint64 = 17361641481138401520
    var t173_rhs uint64 = 1085102592571150095
    var t173 uint64 = t173_lhs | t173_rhs
    show_u64(t173)
    var t174_lhs uint64 = 17361641481138401520
    var t174_rhs uint64 = 1085102592571150095
    var t174 uint64 = t174_lhs ^ t174_rhs
    show_u64(t174)
    var t175_lhs uint64 = 1
    var t175_rhs int = 63
    var t175 uint64 = t175_lhs << t175_rhs
    show_u64(t175)
    var t176_lhs uint64 = 9223372036854775808
    var t176_rhs int = 63
    var t176 uint64 = t176_lhs >> t176_rhs
    show_u64(t176)
    var t177_operand uint64 = 0
    var t177 uint64 = ^t177_operand
    show_u64(t177)
    return struct{}{}
}

func signed_ops() struct{} {
    var t180_lhs int8 = -13
    var t180_rhs int8 = 5
    var t180 int8 = t180_lhs % t180_rhs
    show_i8(t180)
    var t181_lhs int8 = -8
    var t181_rhs int = 2
    var t181 int8 = t181_lhs >> t181_rhs
    show_i8(t181)
    var t182_lhs int8 = 1
    var t182_rhs int = 6
    var t182 int8 = t182_lhs << t182_rhs
    show_i8(t182)
    var t183_operand int8 = 0
    var t183 int8 = ^t183_operand
    show_i8(t183)
    var t184_lhs int8 = -1
    var t184_rhs int = 7
    var t184 int8 = t184_lhs >> t184_rhs
    show_i8(t184)
    var t185_lhs int16 = -513
    var t185_rhs int16 = 256
    var t185 int16 = t185_lhs % t185_rhs
    show_i16(t185)
    var t186 int16 = -32767 - 1
    var t187_rhs int = 15
    var t187 int16 = t186 >> t187_rhs
    show_i16(t187)
    var t188_lhs int16 = 1
    var t188_rhs int = 14
    var t188 int16 = t188_lhs << t188_rhs
    show_i16(t188)
    var t189_operand int16 = 255
    var t189 int16 = ^t189_operand
    show_i16(t189)
    var t190_lhs int32 = -1000000001
    var t190_rhs int32 = 1000
    var t190 int32 = t190_lhs % t190_rhs
    show_i32(t190)
    var t191 int32 = -2147483647 - 1
    var t192_rhs int = 31
    var t192 int32 = t191 >> t192_rhs
    show_i32(t192)
    var t193_lhs int32 = 1
    var t193_rhs int = 30
    var t193 int32 = t193_lhs << t193_rhs
    show_i32(t193)
    var t194_operand int32 = 65535
    var t194 int32 = ^t194_operand
    show_i32(t194)
    var t195_lhs int64 = -1000000000001
    var t195_rhs int64 = 1000
    var t195 int64 = t195_lhs % t195_rhs
    show_i64(t195)
    var t196_lhs int64 = -9223372036854775807
    var t196_rhs int = 62
    var t196 int64 = t196_lhs >> t196_rhs
    show_i64(t196)
    var t197_lhs int64 = 1
    var t197_rhs int = 62
    var t197 int64 = t197_lhs << t197_rhs
    show_i64(t197)
    var t198_operand int64 = 4294967295
    var t198 int64 = ^t198_operand
    show_i64(t198)
    return struct{}{}
}

func precedence() struct{} {
    var t201_lhs uint8 = 3
    var t201_rhs uint8 = 1
    var t201 uint8 = t201_lhs & t201_rhs
    var t202_lhs uint8 = 2
    var t202 uint8 = t202_lhs ^ t201
    var t203_lhs uint8 = 1
    var t203 uint8 = t203_lhs | t202
    show_u8(t203)
    var t204 int = 2 + 1
    var t205_lhs uint8 = 1
    var t205 uint8 = t205_lhs << t204
    show_u8(t205)
    var t206_lhs int = 1
    var t206_rhs int = 2
    var t206 int = t206_lhs | t206_rhs
    var t207 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t206, 3)
    var t208 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t207)
    println__T_string(t208)
    var t209_lhs int = 8
    var t209_rhs int = 1
    var t209 int = t209_lhs >> t209_rhs
    var t210 bool = t209 < 5
    var t211 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t210)
    println__T_string(t211)
    var t212_operand uint8 = 1
    var t212 uint8 = ^t212_operand
    var t213_rhs uint8 = 15
    var t213 uint8 = t212 & t213_rhs
    show_u8(t213)
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
    var t216 uint8 = uint8(uint16(five_eleven__8))
    show_u8(t216)
    var t217 uint8 = uint8(uint16(two_fifty_six__9))
    show_u8(t217)
    var t218 uint8 = uint8(int16(negative_one_i16__10))
    show_u8(t218)
    var t219 int8 = int8(uint8(two_fifty_five__11))
    show_i8(t219)
    var t220 int8 = int8(uint8(one_twenty_eight__12))
    show_i8(t220)
    var t221 int8 = int8(int16(negative_one_twenty_nine__13))
    show_i8(t221)
    var t222 int16 = int16(uint16(max_u16__14))
    show_i16(t222)
    var t223 uint16 = uint16(int32(negative_one_i32__15))
    show_u16(t223)
    var t224 uint64 = uint64(int8(negative_one_i8__16))
    show_u64(t224)
    var t225 int32 = int32(uint64(max_u64__17))
    show_i32(t225)
    var t226 uint32 = uint32(uint8(sixty_five__18))
    show_u32(t226)
    var t227 int64 = int64(uint32(max_u32__19))
    show_i64(t227)
    var t228_source rune = 65
    var t228 uint32 = uint32(rune(t228_source))
    show_u32(t228)
    var mtmp125 Option__char = char_from_uint32(128512)
    switch mtmp125.(type) {
    case None:
        println__T_string("invalid")
    case Some:
        var x126 rune = mtmp125.(Some)._0
        var value__21 rune = x126
        var t234 string = _goml_m_inherent_i_char_i_char_i_to__string(value__21)
        println__T_string(t234)
    default:
        panic("non-exhaustive match")
    }
    var t230 uint8 = uint8(uint16(three_hundred__20))
    var t231 uint32 = uint32(uint8(t230))
    show_u32(t231)
    return struct{}{}
}

func contextual(value__22 uint8) uint8 {
    var retv237 uint8
    var t238 uint8 = ^value__22
    var t239_rhs uint8 = 15
    var t239 uint8 = t238 & t239_rhs
    var t240_lhs uint8 = 1
    var t240_rhs int = 4
    var t240 uint8 = t240_lhs << t240_rhs
    var t241_rhs uint8 = 31
    var t241 uint8 = t240 % t241_rhs
    var t242 uint8 = t239 | t241
    retv237 = t242
    return retv237
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t244 uint8 = contextual(10)
    show_u8(t244)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t247 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t247)
    return struct{}{}
}

func println__T_uint16(value__1 uint16) struct{} {
    var t250 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(value__1)
    _goml_runtime_core_string_println(t250)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t253 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(value__1)
    _goml_runtime_core_string_println(t253)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t256 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t256)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t259 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    _goml_runtime_core_string_println(t259)
    return struct{}{}
}

func println__T_int16(value__1 int16) struct{} {
    var t262 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(value__1)
    _goml_runtime_core_string_println(t262)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t265 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t265)
    return struct{}{}
}

func println__T_int64(value__1 int64) struct{} {
    var t268 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(value__1)
    _goml_runtime_core_string_println(t268)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t271)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv274 bool
    var t275 bool = self__59 == other__60
    retv274 = t275
    return retv274
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv277 string
    var t278 string = _goml_runtime_core_bool_to_string(self__37)
    retv277 = t278
    return retv277
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv280 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp282 Option__char
    if valid__3 {
        var t283 Option__char = Some{
            _0: value__4,
        }
        jp282 = t283
    } else {
        jp282 = None{}
    }
    retv280 = jp282
    return retv280
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv285 string
    var t286 string = _goml_runtime_core_char_to_string(self__7)
    retv285 = t286
    return retv285
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv288 string
    var t289 string = _goml_runtime_core_uint8_to_string(self__45)
    retv288 = t289
    return retv288
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv291 string
    var t292 string = _goml_runtime_core_uint16_to_string(self__46)
    retv291 = t292
    return retv291
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv294 string
    var t295 string = _goml_runtime_core_uint32_to_string(self__47)
    retv294 = t295
    return retv294
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv297 string
    var t298 string = _goml_runtime_core_uint64_to_string(self__48)
    retv297 = t298
    return retv297
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var retv300 string
    var t301 string = _goml_runtime_core_int8_to_string(self__41)
    retv300 = t301
    return retv300
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv303 string
    var t304 string = _goml_runtime_core_int16_to_string(self__42)
    retv303 = t304
    return retv303
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv306 string
    var t307 string = _goml_runtime_core_int32_to_string(self__43)
    retv306 = t307
    return retv306
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv309 string
    var t310 string = _goml_runtime_core_int64_to_string(self__44)
    retv309 = t310
    return retv309
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv312 string
    retv312 = self__38
    return retv312
}

func main() {
    main0()
}
