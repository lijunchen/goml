package main

import (
    _goml_os "os"
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

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type Ordering int32

type Option__char struct {
    _tag int32
    _v1_0 rune
}

func show_u8(value__0 uint8) struct{} {
    var inline1133 string = _goml_m_trait__impl_i_ToString_i_u8_i_to__string(value__0)
    _goml_runtime_core_string_println(inline1133)
    return struct{}{}
}

func unsigned_ops() struct{} {
    var t881_lhs uint8 = 13
    var t881_rhs uint8 = 5
    var t881 uint8 = t881_lhs % t881_rhs
    show_u8(t881)
    var t882_lhs uint8 = 12
    var t882_rhs uint8 = 10
    var t882 uint8 = t882_lhs & t882_rhs
    show_u8(t882)
    var t883_lhs uint8 = 12
    var t883_rhs uint8 = 3
    var t883 uint8 = t883_lhs | t883_rhs
    show_u8(t883)
    var t884_lhs uint8 = 12
    var t884_rhs uint8 = 10
    var t884 uint8 = t884_lhs ^ t884_rhs
    show_u8(t884)
    var t885_lhs uint8 = 1
    var t885_rhs int = 7
    var t885 uint8 = t885_lhs << t885_rhs
    show_u8(t885)
    var t886_lhs uint8 = 128
    var t886_rhs int = 7
    var t886 uint8 = t886_lhs >> t886_rhs
    println__T_u8(t886)
    var t887_operand uint8 = 0
    var t887 uint8 = ^t887_operand
    println__T_u8(t887)
    var t888_lhs uint8 = 1
    var t888_rhs int = 8
    var t888 uint8 = t888_lhs << t888_rhs
    println__T_u8(t888)
    var t889_lhs uint16 = 513
    var t889_rhs uint16 = 256
    var t889 uint16 = t889_lhs % t889_rhs
    println__T_u16(t889)
    var t890_lhs uint16 = 3855
    var t890_rhs uint16 = 255
    var t890 uint16 = t890_lhs & t890_rhs
    println__T_u16(t890)
    var t891_lhs uint16 = 3840
    var t891_rhs uint16 = 15
    var t891 uint16 = t891_lhs | t891_rhs
    println__T_u16(t891)
    var t892_lhs uint16 = 43690
    var t892_rhs uint16 = 3855
    var t892 uint16 = t892_lhs ^ t892_rhs
    println__T_u16(t892)
    var t893_lhs uint16 = 1
    var t893_rhs int = 15
    var t893 uint16 = t893_lhs << t893_rhs
    println__T_u16(t893)
    var t894_lhs uint16 = 32768
    var t894_rhs int = 15
    var t894 uint16 = t894_lhs >> t894_rhs
    println__T_u16(t894)
    var t895_operand uint16 = 0
    var t895 uint16 = ^t895_operand
    println__T_u16(t895)
    var t896_lhs uint32 = 1000000001
    var t896_rhs uint32 = 1000
    var t896 uint32 = t896_lhs % t896_rhs
    println__T_u32(t896)
    var t897_lhs uint32 = 4042322160
    var t897_rhs uint32 = 252645135
    var t897 uint32 = t897_lhs & t897_rhs
    println__T_u32(t897)
    var t898_lhs uint32 = 4042322160
    var t898_rhs uint32 = 252645135
    var t898 uint32 = t898_lhs | t898_rhs
    println__T_u32(t898)
    var t899_lhs uint32 = 4042322160
    var t899_rhs uint32 = 252645135
    var t899 uint32 = t899_lhs ^ t899_rhs
    println__T_u32(t899)
    var t900_lhs uint32 = 1
    var t900_rhs int = 31
    var t900 uint32 = t900_lhs << t900_rhs
    println__T_u32(t900)
    var t901_lhs uint32 = 2147483648
    var t901_rhs int = 31
    var t901 uint32 = t901_lhs >> t901_rhs
    println__T_u32(t901)
    var t902_operand uint32 = 0
    var t902 uint32 = ^t902_operand
    println__T_u32(t902)
    var t903_lhs uint64 = 1000000000001
    var t903_rhs uint64 = 1000
    var t903 uint64 = t903_lhs % t903_rhs
    println__T_u64(t903)
    var t904_lhs uint64 = 17361641481138401520
    var t904_rhs uint64 = 1085102592571150095
    var t904 uint64 = t904_lhs & t904_rhs
    println__T_u64(t904)
    var t905_lhs uint64 = 17361641481138401520
    var t905_rhs uint64 = 1085102592571150095
    var t905 uint64 = t905_lhs | t905_rhs
    println__T_u64(t905)
    var t906_lhs uint64 = 17361641481138401520
    var t906_rhs uint64 = 1085102592571150095
    var t906 uint64 = t906_lhs ^ t906_rhs
    println__T_u64(t906)
    var t907_lhs uint64 = 1
    var t907_rhs int = 63
    var t907 uint64 = t907_lhs << t907_rhs
    println__T_u64(t907)
    var t908_lhs uint64 = 9223372036854775808
    var t908_rhs int = 63
    var t908 uint64 = t908_lhs >> t908_rhs
    println__T_u64(t908)
    var t909_operand uint64 = 0
    var t909 uint64 = ^t909_operand
    println__T_u64(t909)
    return struct{}{}
}

func signed_ops() struct{} {
    var t912_lhs int8 = -13
    var t912_rhs int8 = 5
    var t912 int8 = t912_lhs % t912_rhs
    println__T_i8(t912)
    var t913_lhs int8 = -8
    var t913_rhs int = 2
    var t913 int8 = t913_lhs >> t913_rhs
    println__T_i8(t913)
    var t914_lhs int8 = 1
    var t914_rhs int = 6
    var t914 int8 = t914_lhs << t914_rhs
    println__T_i8(t914)
    var t915_operand int8 = 0
    var t915 int8 = ^t915_operand
    println__T_i8(t915)
    var t916_lhs int8 = -1
    var t916_rhs int = 7
    var t916 int8 = t916_lhs >> t916_rhs
    println__T_i8(t916)
    var t917_lhs int16 = -513
    var t917_rhs int16 = 256
    var t917 int16 = t917_lhs % t917_rhs
    println__T_i16(t917)
    var t918 int16 = -32767 - 1
    var t919_rhs int = 15
    var t919 int16 = t918 >> t919_rhs
    println__T_i16(t919)
    var t920_lhs int16 = 1
    var t920_rhs int = 14
    var t920 int16 = t920_lhs << t920_rhs
    println__T_i16(t920)
    var t921_operand int16 = 255
    var t921 int16 = ^t921_operand
    println__T_i16(t921)
    var t922_lhs int32 = -1000000001
    var t922_rhs int32 = 1000
    var t922 int32 = t922_lhs % t922_rhs
    println__T_i32(t922)
    var t923 int32 = -2147483647 - 1
    var t924_rhs int = 31
    var t924 int32 = t923 >> t924_rhs
    println__T_i32(t924)
    var t925_lhs int32 = 1
    var t925_rhs int = 30
    var t925 int32 = t925_lhs << t925_rhs
    println__T_i32(t925)
    var t926_operand int32 = 65535
    var t926 int32 = ^t926_operand
    println__T_i32(t926)
    var t927_lhs int64 = -1000000000001
    var t927_rhs int64 = 1000
    var t927 int64 = t927_lhs % t927_rhs
    println__T_i64(t927)
    var t928_lhs int64 = -9223372036854775807
    var t928_rhs int = 62
    var t928 int64 = t928_lhs >> t928_rhs
    println__T_i64(t928)
    var t929_lhs int64 = 1
    var t929_rhs int = 62
    var t929 int64 = t929_lhs << t929_rhs
    println__T_i64(t929)
    var t930_operand int64 = 4294967295
    var t930 int64 = ^t930_operand
    println__T_i64(t930)
    return struct{}{}
}

func precedence() struct{} {
    var t933_lhs uint8 = 3
    var t933_rhs uint8 = 1
    var t933 uint8 = t933_lhs & t933_rhs
    var t934_lhs uint8 = 2
    var t934 uint8 = t934_lhs ^ t933
    var t935_lhs uint8 = 1
    var t935 uint8 = t935_lhs | t934
    println__T_u8(t935)
    var t936 int = 2 + 1
    var t937_lhs uint8 = 1
    var t937 uint8 = t937_lhs << t936
    println__T_u8(t937)
    var t938_lhs int = 1
    var t938_rhs int = 2
    var t938 int = t938_lhs | t938_rhs
    var t939 bool = t938 == 3
    var t940 string
    var inline1249 string = _goml_runtime_core_bool_to_string(t939)
    t940 = inline1249
    var inline1246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t940)
    _goml_runtime_core_string_println(inline1246)
    var t941_lhs int = 8
    var t941_rhs int = 1
    var t941 int = t941_lhs >> t941_rhs
    var t942 bool = t941 < 5
    var t943 string
    var inline1244 string = _goml_runtime_core_bool_to_string(t942)
    t943 = inline1244
    var inline1241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t943)
    _goml_runtime_core_string_println(inline1241)
    var t944_operand uint8 = 1
    var t944 uint8 = ^t944_operand
    var t945_rhs uint8 = 15
    var t945 uint8 = t944 & t945_rhs
    println__T_u8(t945)
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
    var t948 uint8 = uint8(uint16(five_eleven__8))
    println__T_u8(t948)
    var t949 uint8 = uint8(uint16(two_fifty_six__9))
    println__T_u8(t949)
    var t950 uint8 = uint8(int16(negative_one_i16__10))
    println__T_u8(t950)
    var t951 int8 = int8(uint8(two_fifty_five__11))
    println__T_i8(t951)
    var t952 int8 = int8(uint8(one_twenty_eight__12))
    println__T_i8(t952)
    var t953 int8 = int8(int16(negative_one_twenty_nine__13))
    println__T_i8(t953)
    var t954 int16 = int16(uint16(max_u16__14))
    println__T_i16(t954)
    var t955 uint16 = uint16(int32(negative_one_i32__15))
    println__T_u16(t955)
    var t956 uint64 = uint64(int8(negative_one_i8__16))
    println__T_u64(t956)
    var t957 int32 = int32(uint64(max_u64__17))
    println__T_i32(t957)
    var t958 uint32 = uint32(uint8(sixty_five__18))
    println__T_u32(t958)
    var t959 int64 = int64(uint32(max_u32__19))
    println__T_i64(t959)
    var t960_source rune = 65
    var t960 uint32 = uint32(rune(t960_source))
    println__T_u32(t960)
    var mtmp857 Option__char
    var inline1266 uint32 = 128512
    var inline1267 Option__char = __goml_builtin_char_from_uint32(inline1266)
    mtmp857 = inline1267
    switch mtmp857._tag {
    case 0:
        var inline1255 string = "invalid"
        var inline1256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1255)
        _goml_runtime_core_string_println(inline1256)
    case 1:
        var x858 rune = mtmp857._v1_0
        var t966 string
        var inline1262 string = char_to_string(x858)
        t966 = inline1262
        var inline1259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t966)
        _goml_runtime_core_string_println(inline1259)
    default:
        panic("non-exhaustive match")
    }
    var t962 uint8 = uint8(uint16(three_hundred__20))
    var t963 uint32 = uint32(uint8(t962))
    println__T_u32(t963)
    return struct{}{}
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t976 uint8
    var inline1297 uint8 = 10
    var inline1298 uint8 = ^inline1297
    var inline1299_rhs uint8 = 15
    var inline1299 uint8 = inline1298 & inline1299_rhs
    var inline1300_lhs uint8 = 1
    var inline1300_rhs int = 4
    var inline1300 uint8 = inline1300_lhs << inline1300_rhs
    var inline1301_rhs uint8 = 31
    var inline1301 uint8 = inline1300 % inline1301_rhs
    var inline1302 uint8 = inline1299 | inline1301
    t976 = inline1302
    println__T_u8(t976)
    return struct{}{}
}

func println__T_u8(value__1 uint8) struct{} {
    var t979 string
    var inline1304 string = __goml_builtin_uint8_to_string(value__1)
    t979 = inline1304
    _goml_runtime_core_string_println(t979)
    return struct{}{}
}

func println__T_u16(value__1 uint16) struct{} {
    var t982 string
    var inline1306 string = __goml_builtin_uint16_to_string(value__1)
    t982 = inline1306
    _goml_runtime_core_string_println(t982)
    return struct{}{}
}

func println__T_u32(value__1 uint32) struct{} {
    var t985 string
    var inline1308 string = __goml_builtin_uint32_to_string(value__1)
    t985 = inline1308
    _goml_runtime_core_string_println(t985)
    return struct{}{}
}

func println__T_u64(value__1 uint64) struct{} {
    var t988 string
    var inline1310 string = __goml_builtin_uint64_to_string(value__1)
    t988 = inline1310
    _goml_runtime_core_string_println(t988)
    return struct{}{}
}

func println__T_i8(value__1 int8) struct{} {
    var t991 string
    var inline1312 string = __goml_builtin_int8_to_string(value__1)
    t991 = inline1312
    _goml_runtime_core_string_println(t991)
    return struct{}{}
}

func println__T_i16(value__1 int16) struct{} {
    var t994 string
    var inline1314 string = __goml_builtin_int16_to_string(value__1)
    t994 = inline1314
    _goml_runtime_core_string_println(t994)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t997 string
    var inline1316 string = __goml_builtin_int32_to_string(value__1)
    t997 = inline1316
    _goml_runtime_core_string_println(t997)
    return struct{}{}
}

func println__T_i64(value__1 int64) struct{} {
    var t1000 string
    var inline1318 string = __goml_builtin_int64_to_string(value__1)
    t1000 = inline1318
    _goml_runtime_core_string_println(t1000)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_u8_i_to__string(self__409 uint8) string {
    var inline1332 uint64 = uint64(uint8(self__409))
    var inline1333 string = decimal_string(inline1332)
    return inline1333
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t1044 bool
    var inline1354 bool = value__283 <= 1114111
    if inline1354 {
        var inline1355 bool = value__283 >= 55296
        var inline1357 bool
        if inline1355 {
            var inline1359 bool = value__283 <= 57343
            inline1357 = inline1359
        } else {
            inline1357 = false
        }
        var inline1358 bool = !inline1357
        t1044 = inline1358
    } else {
        t1044 = false
    }
    if t1044 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t1045 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t1045
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func char_to_string(value__282 rune) string {
    var t1050 uint32 = uint32(rune(value__282))
    var t1051 bool
    var inline1361 bool = t1050 <= 1114111
    if inline1361 {
        var inline1362 bool = t1050 >= 55296
        var inline1364 bool
        if inline1362 {
            var inline1366 bool = t1050 <= 57343
            inline1364 = inline1366
        } else {
            inline1364 = false
        }
        var inline1365 bool = !inline1364
        t1051 = inline1365
    } else {
        t1051 = false
    }
    if t1051 {
        var t1052 string = _goml_runtime_core_char_to_string(value__282)
        return t1052
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func __goml_builtin_uint8_to_string(value__228 uint8) string {
    var t1055 uint64 = uint64(uint8(value__228))
    var t1056 string = decimal_string(t1055)
    return t1056
}

func __goml_builtin_uint16_to_string(value__229 uint16) string {
    var t1059 uint64 = uint64(uint16(value__229))
    var t1060 string = decimal_string(t1059)
    return t1060
}

func __goml_builtin_uint32_to_string(value__230 uint32) string {
    var t1063 uint64 = uint64(uint32(value__230))
    var t1064 string = decimal_string(t1063)
    return t1064
}

func __goml_builtin_uint64_to_string(value__231 uint64) string {
    var t1067 string = decimal_string(value__231)
    return t1067
}

func __goml_builtin_int8_to_string(value__223 int8) string {
    var t1070 int64 = int64(int8(value__223))
    var inline1368 bool = t1070 < 0
    if inline1368 {
        var inline1369 uint64 = uint64(int64(t1070))
        var inline1370 uint64 = 0 - inline1369
        var inline1371 string = decimal_string(inline1370)
        var inline1372 string = "-" + inline1371
        return inline1372
    } else {
        var inline1373 uint64 = uint64(int64(t1070))
        var inline1374 string = decimal_string(inline1373)
        return inline1374
    }
}

func __goml_builtin_int16_to_string(value__224 int16) string {
    var t1074 int64 = int64(int16(value__224))
    var inline1376 bool = t1074 < 0
    if inline1376 {
        var inline1377 uint64 = uint64(int64(t1074))
        var inline1378 uint64 = 0 - inline1377
        var inline1379 string = decimal_string(inline1378)
        var inline1380 string = "-" + inline1379
        return inline1380
    } else {
        var inline1381 uint64 = uint64(int64(t1074))
        var inline1382 string = decimal_string(inline1381)
        return inline1382
    }
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t1078 int64 = int64(int32(value__225))
    var inline1384 bool = t1078 < 0
    if inline1384 {
        var inline1385 uint64 = uint64(int64(t1078))
        var inline1386 uint64 = 0 - inline1385
        var inline1387 string = decimal_string(inline1386)
        var inline1388 string = "-" + inline1387
        return inline1388
    } else {
        var inline1389 uint64 = uint64(int64(t1078))
        var inline1390 string = decimal_string(inline1389)
        return inline1390
    }
}

func __goml_builtin_int64_to_string(value__226 int64) string {
    var inline1392 bool = value__226 < 0
    if inline1392 {
        var inline1393 uint64 = uint64(int64(value__226))
        var inline1394 uint64 = 0 - inline1393
        var inline1395 string = decimal_string(inline1394)
        var inline1396 string = "-" + inline1395
        return inline1396
    } else {
        var inline1397 uint64 = uint64(int64(value__226))
        var inline1398 string = decimal_string(inline1397)
        return inline1398
    }
}

func decimal_string(value__208 uint64) string {
    var t1115 bool = value__208 == 0
    if t1115 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1108:
        for {
            var t1109 bool = remaining__210 > 0
            if t1109 {
                var t1110_rhs uint64 = 10
                var t1110 uint64 = remaining__210 % t1110_rhs
                var t1111 uint8 = uint8(uint64(t1110))
                var t1112 uint8 = t1111 + 48
                vec_push__Vec_5uint8(reversed__209, t1112)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1113 uint64 = compound_old353 / compound_value354
                remaining__210 = t1113
                continue
            } else {
                break Loop_loop1108
            }
        }
        var t1097 int
        var inline1408 int = vec_len__Vec_5uint8(reversed__209)
        t1097 = inline1408
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1097)
        var offset__212 int = 0
        Loop_loop1099:
        for {
            var t1100 int
            var inline1406 int = vec_len__Vec_5uint8(reversed__209)
            t1100 = inline1406
            var t1101 bool = offset__212 < t1100
            if t1101 {
                var t1102 int
                var inline1404 int = vec_len__Vec_5uint8(reversed__209)
                t1102 = inline1404
                var t1103 int = t1102 - offset__212
                var t1104 int = t1103 - 1
                var t1105 uint8 = vec_get__Vec_5uint8(reversed__209, t1104)
                vec_push__Vec_5uint8(bytes__211, t1105)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1106 int = compound_old358 + compound_value359
                offset__212 = t1106
                continue
            } else {
                break Loop_loop1099
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func main() {
    main0()
}
