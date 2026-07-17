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
    var t146_lhs uint8 = 13
    var t146_rhs uint8 = 5
    var t146 uint8 = t146_lhs % t146_rhs
    show_u8(t146)
    var t147_lhs uint8 = 12
    var t147_rhs uint8 = 10
    var t147 uint8 = t147_lhs & t147_rhs
    show_u8(t147)
    var t148_lhs uint8 = 12
    var t148_rhs uint8 = 3
    var t148 uint8 = t148_lhs | t148_rhs
    show_u8(t148)
    var t149_lhs uint8 = 12
    var t149_rhs uint8 = 10
    var t149 uint8 = t149_lhs ^ t149_rhs
    show_u8(t149)
    var t150_lhs uint8 = 1
    var t150_rhs uint32 = 7
    var t150 uint8 = t150_lhs << t150_rhs
    show_u8(t150)
    var t151_lhs uint8 = 128
    var t151_rhs uint16 = 7
    var t151 uint8 = t151_lhs >> t151_rhs
    show_u8(t151)
    var t152_operand uint8 = 0
    var t152 uint8 = ^t152_operand
    show_u8(t152)
    var t153_lhs uint8 = 1
    var t153_rhs uint8 = 8
    var t153 uint8 = t153_lhs << t153_rhs
    show_u8(t153)
    var t154_lhs uint16 = 513
    var t154_rhs uint16 = 256
    var t154 uint16 = t154_lhs % t154_rhs
    show_u16(t154)
    var t155_lhs uint16 = 3855
    var t155_rhs uint16 = 255
    var t155 uint16 = t155_lhs & t155_rhs
    show_u16(t155)
    var t156_lhs uint16 = 3840
    var t156_rhs uint16 = 15
    var t156 uint16 = t156_lhs | t156_rhs
    show_u16(t156)
    var t157_lhs uint16 = 43690
    var t157_rhs uint16 = 3855
    var t157 uint16 = t157_lhs ^ t157_rhs
    show_u16(t157)
    var t158_lhs uint16 = 1
    var t158_rhs uint32 = 15
    var t158 uint16 = t158_lhs << t158_rhs
    show_u16(t158)
    var t159_lhs uint16 = 32768
    var t159_rhs uint8 = 15
    var t159 uint16 = t159_lhs >> t159_rhs
    show_u16(t159)
    var t160_operand uint16 = 0
    var t160 uint16 = ^t160_operand
    show_u16(t160)
    var t161_lhs uint32 = 1000000001
    var t161_rhs uint32 = 1000
    var t161 uint32 = t161_lhs % t161_rhs
    show_u32(t161)
    var t162_lhs uint32 = 4042322160
    var t162_rhs uint32 = 252645135
    var t162 uint32 = t162_lhs & t162_rhs
    show_u32(t162)
    var t163_lhs uint32 = 4042322160
    var t163_rhs uint32 = 252645135
    var t163 uint32 = t163_lhs | t163_rhs
    show_u32(t163)
    var t164_lhs uint32 = 4042322160
    var t164_rhs uint32 = 252645135
    var t164 uint32 = t164_lhs ^ t164_rhs
    show_u32(t164)
    var t165_lhs uint32 = 1
    var t165_rhs uint32 = 31
    var t165 uint32 = t165_lhs << t165_rhs
    show_u32(t165)
    var t166_lhs uint32 = 2147483648
    var t166_rhs uint32 = 31
    var t166 uint32 = t166_lhs >> t166_rhs
    show_u32(t166)
    var t167_operand uint32 = 0
    var t167 uint32 = ^t167_operand
    show_u32(t167)
    var t168_lhs uint64 = 1000000000001
    var t168_rhs uint64 = 1000
    var t168 uint64 = t168_lhs % t168_rhs
    show_u64(t168)
    var t169_lhs uint64 = 17361641481138401520
    var t169_rhs uint64 = 1085102592571150095
    var t169 uint64 = t169_lhs & t169_rhs
    show_u64(t169)
    var t170_lhs uint64 = 17361641481138401520
    var t170_rhs uint64 = 1085102592571150095
    var t170 uint64 = t170_lhs | t170_rhs
    show_u64(t170)
    var t171_lhs uint64 = 17361641481138401520
    var t171_rhs uint64 = 1085102592571150095
    var t171 uint64 = t171_lhs ^ t171_rhs
    show_u64(t171)
    var t172_lhs uint64 = 1
    var t172_rhs uint64 = 63
    var t172 uint64 = t172_lhs << t172_rhs
    show_u64(t172)
    var t173_lhs uint64 = 9223372036854775808
    var t173_rhs uint64 = 63
    var t173 uint64 = t173_lhs >> t173_rhs
    show_u64(t173)
    var t174_operand uint64 = 0
    var t174 uint64 = ^t174_operand
    show_u64(t174)
    return struct{}{}
}

func signed_ops() struct{} {
    var t177 int8 = -13
    var t178_rhs int8 = 5
    var t178 int8 = t177 % t178_rhs
    show_i8(t178)
    var t179 int8 = -8
    var t180_rhs int8 = 2
    var t180 int8 = t179 >> t180_rhs
    show_i8(t180)
    var t181_lhs int8 = 1
    var t181_rhs int8 = 6
    var t181 int8 = t181_lhs << t181_rhs
    show_i8(t181)
    var t182_operand int8 = 0
    var t182 int8 = ^t182_operand
    show_i8(t182)
    var t183 int8 = -1
    var t184_rhs int8 = 7
    var t184 int8 = t183 >> t184_rhs
    show_i8(t184)
    var t185 int16 = -513
    var t186_rhs int16 = 256
    var t186 int16 = t185 % t186_rhs
    show_i16(t186)
    var t187 int16 = -32767
    var t188 int16 = t187 - 1
    var t189_rhs int16 = 15
    var t189 int16 = t188 >> t189_rhs
    show_i16(t189)
    var t190_lhs int16 = 1
    var t190_rhs int16 = 14
    var t190 int16 = t190_lhs << t190_rhs
    show_i16(t190)
    var t191_operand int16 = 255
    var t191 int16 = ^t191_operand
    show_i16(t191)
    var t192 int32 = -1000000001
    var t193_rhs int32 = 1000
    var t193 int32 = t192 % t193_rhs
    show_i32(t193)
    var t194 int32 = -2147483647
    var t195 int32 = t194 - 1
    var t196_rhs int32 = 31
    var t196 int32 = t195 >> t196_rhs
    show_i32(t196)
    var t197_lhs int32 = 1
    var t197_rhs int32 = 30
    var t197 int32 = t197_lhs << t197_rhs
    show_i32(t197)
    var t198_operand int32 = 65535
    var t198 int32 = ^t198_operand
    show_i32(t198)
    var t199 int64 = -1000000000001
    var t200_rhs int64 = 1000
    var t200 int64 = t199 % t200_rhs
    show_i64(t200)
    var t201 int64 = -9223372036854775807
    var t202_rhs int64 = 62
    var t202 int64 = t201 >> t202_rhs
    show_i64(t202)
    var t203_lhs int64 = 1
    var t203_rhs int64 = 62
    var t203 int64 = t203_lhs << t203_rhs
    show_i64(t203)
    var t204_operand int64 = 4294967295
    var t204 int64 = ^t204_operand
    show_i64(t204)
    return struct{}{}
}

func precedence() struct{} {
    var t207_lhs uint8 = 3
    var t207_rhs uint8 = 1
    var t207 uint8 = t207_lhs & t207_rhs
    var t208_lhs uint8 = 2
    var t208 uint8 = t208_lhs ^ t207
    var t209_lhs uint8 = 1
    var t209 uint8 = t209_lhs | t208
    show_u8(t209)
    var t210 uint8 = 2 + 1
    var t211_lhs uint8 = 1
    var t211 uint8 = t211_lhs << t210
    show_u8(t211)
    var t212_lhs uint8 = 1
    var t212_rhs uint8 = 2
    var t212 uint8 = t212_lhs | t212_rhs
    var t213 bool = t212 == 3
    var t214 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t213)
    println__T_string(t214)
    var t215_lhs uint8 = 8
    var t215_rhs uint8 = 1
    var t215 uint8 = t215_lhs >> t215_rhs
    var t216 bool = t215 < 5
    var t217 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t216)
    println__T_string(t217)
    var t218_operand uint8 = 1
    var t218 uint8 = ^t218_operand
    var t219_rhs uint8 = 15
    var t219 uint8 = t218 & t219_rhs
    show_u8(t219)
    return struct{}{}
}

func casts() struct{} {
    var t222_source uint16 = 511
    var t222 uint8 = uint8(uint16(t222_source))
    show_u8(t222)
    var t223_source uint16 = 256
    var t223 uint8 = uint8(uint16(t223_source))
    show_u8(t223)
    var t224 int16 = -1
    var t225 uint8 = uint8(int16(t224))
    show_u8(t225)
    var t226_source uint8 = 255
    var t226 int8 = int8(uint8(t226_source))
    show_i8(t226)
    var t227_source uint8 = 128
    var t227 int8 = int8(uint8(t227_source))
    show_i8(t227)
    var t228 int16 = -129
    var t229 int8 = int8(int16(t228))
    show_i8(t229)
    var t230_source uint16 = 65535
    var t230 int16 = int16(uint16(t230_source))
    show_i16(t230)
    var t231 int32 = -1
    var t232 uint16 = uint16(int32(t231))
    show_u16(t232)
    var t233 int8 = -1
    var t234 uint64 = uint64(int8(t233))
    show_u64(t234)
    var t235_source uint64 = 18446744073709551615
    var t235 int32 = int32(uint64(t235_source))
    show_i32(t235)
    var t236_source uint8 = 65
    var t236 uint32 = uint32(uint8(t236_source))
    show_u32(t236)
    var t237_source uint32 = 4294967295
    var t237 int64 = int64(uint32(t237_source))
    show_i64(t237)
    var t238_source rune = 65
    var t238 uint32 = uint32(rune(t238_source))
    show_u32(t238)
    var mtmp122 Option__char = char_from_uint32(128512)
    switch mtmp122.(type) {
    case None:
        println__T_string("invalid")
    case Some:
        var x123 rune = mtmp122.(Some)._0
        var value__8 rune = x123
        var t244 string = _goml_m_inherent_i_char_i_char_i_to__string(value__8)
        println__T_string(t244)
    default:
        panic("non-exhaustive match")
    }
    var t240_source uint16 = 300
    var t240 uint8 = uint8(uint16(t240_source))
    var t241 uint32 = uint32(uint8(t240))
    show_u32(t241)
    return struct{}{}
}

func contextual(value__9 uint8) uint8 {
    var retv247 uint8
    var t248 uint8 = ^value__9
    var t249_rhs uint8 = 15
    var t249 uint8 = t248 & t249_rhs
    var t250_lhs uint8 = 1
    var t250_rhs int32 = 4
    var t250 uint8 = t250_lhs << t250_rhs
    var t251_rhs uint8 = 31
    var t251 uint8 = t250 % t251_rhs
    var t252 uint8 = t249 | t251
    retv247 = t252
    return retv247
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t254 uint8 = contextual(10)
    show_u8(t254)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t257 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t257)
    return struct{}{}
}

func println__T_uint16(value__1 uint16) struct{} {
    var t260 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(value__1)
    _goml_runtime_core_string_println(t260)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t263 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(value__1)
    _goml_runtime_core_string_println(t263)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t266 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t266)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t269 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    _goml_runtime_core_string_println(t269)
    return struct{}{}
}

func println__T_int16(value__1 int16) struct{} {
    var t272 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(value__1)
    _goml_runtime_core_string_println(t272)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t275 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t275)
    return struct{}{}
}

func println__T_int64(value__1 int64) struct{} {
    var t278 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(value__1)
    _goml_runtime_core_string_println(t278)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t281 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t281)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv284 string
    var t285 string = _goml_runtime_core_bool_to_string(self__36)
    retv284 = t285
    return retv284
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv287 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp289 Option__char
    if valid__3 {
        var t290 Option__char = Some{
            _0: value__4,
        }
        jp289 = t290
    } else {
        jp289 = None{}
    }
    retv287 = jp289
    return retv287
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__6 rune) string {
    var retv292 string
    var t293 string = _goml_runtime_core_char_to_string(self__6)
    retv292 = t293
    return retv292
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__43 uint8) string {
    var retv295 string
    var t296 string = _goml_runtime_core_uint8_to_string(self__43)
    retv295 = t296
    return retv295
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__44 uint16) string {
    var retv298 string
    var t299 string = _goml_runtime_core_uint16_to_string(self__44)
    retv298 = t299
    return retv298
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__45 uint32) string {
    var retv301 string
    var t302 string = _goml_runtime_core_uint32_to_string(self__45)
    retv301 = t302
    return retv301
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__46 uint64) string {
    var retv304 string
    var t305 string = _goml_runtime_core_uint64_to_string(self__46)
    retv304 = t305
    return retv304
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__39 int8) string {
    var retv307 string
    var t308 string = _goml_runtime_core_int8_to_string(self__39)
    retv307 = t308
    return retv307
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__40 int16) string {
    var retv310 string
    var t311 string = _goml_runtime_core_int16_to_string(self__40)
    retv310 = t311
    return retv310
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv313 string
    var t314 string = _goml_runtime_core_int32_to_string(self__41)
    retv313 = t314
    return retv313
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__42 int64) string {
    var retv316 string
    var t317 string = _goml_runtime_core_int64_to_string(self__42)
    retv316 = t317
    return retv316
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv319 string
    retv319 = self__37
    return retv319
}

func main() {
    main0()
}
