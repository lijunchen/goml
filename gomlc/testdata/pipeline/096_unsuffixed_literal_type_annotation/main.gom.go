package main

import (
    _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29 "math"
)

import (
    _goml_os "os"
)

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

func _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(arg0 float32) uint32 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float32bits(arg0)
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_new__Vec_5uint8() *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: nil,
    }
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

func vec_set__Vec_5uint8(vec *_goml_vec_uint8, index int, value uint8) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
}

func vec_new__Vec_6uint32() *_goml_vec_uint32 {
    return &_goml_vec_uint32{
        items: nil,
    }
}

func vec_push__Vec_6uint32(vec *_goml_vec_uint32, elem uint32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6uint32(vec *_goml_vec_uint32, index int) uint32 {
    return vec.items[index]
}

func vec_set__Vec_6uint32(vec *_goml_vec_uint32, index int, value uint32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_6uint32(vec *_goml_vec_uint32) int {
    return int(len(vec.items))
}

func vec_truncate__Vec_6uint32(vec *_goml_vec_uint32, new_len int) struct{} {
    if new_len < 0 {
        panic("negative vector length")
    }
    if new_len < int(len(vec.items)) {
        clear(vec.items[new_len:int(len(vec.items))])
        vec.items = vec.items[0:new_len]
    }
    return struct{}{}
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_6string_4bool struct {
    _0 string
    _1 bool
}

type Tuple2_4bool_6uint64 struct {
    _0 bool
    _1 uint64
}

type Tuple2_6uint64_4bool struct {
    _0 uint64
    _1 bool
}

type Tuple2_4bool_3int struct {
    _0 bool
    _1 int
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

func main0() struct{} {
    var a__2 uint8 = 1
    var b__3 int8 = 2
    var c__4 int16 = 3
    var d__5 uint16 = 4
    var e__6 uint32 = 5
    var f__7 int64 = 6
    var g__8 uint64 = 7
    var h__9 float32 = 1
    var t811 string = _goml_m_trait__impl_i_ToString_i_u8_i_to__string(a__2)
    println__T_string(t811)
    var t812 string
    var inline1937 string = __goml_builtin_int8_to_string(b__3)
    t812 = inline1937
    var inline1934 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
    _goml_runtime_core_string_println(inline1934)
    var t813 string
    var inline1932 string = __goml_builtin_int16_to_string(c__4)
    t813 = inline1932
    var inline1929 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t813)
    _goml_runtime_core_string_println(inline1929)
    var t814 string
    var inline1927 string = __goml_builtin_uint16_to_string(d__5)
    t814 = inline1927
    var inline1924 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t814)
    _goml_runtime_core_string_println(inline1924)
    var t815 string
    var inline1922 string = __goml_builtin_uint32_to_string(e__6)
    t815 = inline1922
    var inline1919 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t815)
    _goml_runtime_core_string_println(inline1919)
    var t816 string
    var inline1917 string = __goml_builtin_int64_to_string(f__7)
    t816 = inline1917
    var inline1914 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t816)
    _goml_runtime_core_string_println(inline1914)
    var t817 string
    var inline1912 string = __goml_builtin_uint64_to_string(g__8)
    t817 = inline1912
    var inline1909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t817)
    _goml_runtime_core_string_println(inline1909)
    var t818 string
    var inline1907 string = __goml_builtin_float32_to_string(h__9)
    t818 = inline1907
    var inline1904 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t818)
    _goml_runtime_core_string_println(inline1904)
    var t819 uint8
    var inline1902 uint8 = 10
    t819 = inline1902
    var t820 string
    var inline1900 string = __goml_builtin_uint8_to_string(t819)
    t820 = inline1900
    var inline1897 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
    _goml_runtime_core_string_println(inline1897)
    var t821 float32
    var inline1895 float32 = 2.5
    t821 = inline1895
    var t822 string
    var inline1893 string = __goml_builtin_float32_to_string(t821)
    t822 = inline1893
    var inline1890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
    _goml_runtime_core_string_println(inline1890)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t824 string
    t824 = value__1
    _goml_runtime_core_string_println(t824)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_u8_i_to__string(self__409 uint8) string {
    var inline1940 uint64 = uint64(uint8(self__409))
    var inline1941 string = decimal_string(inline1940)
    return inline1941
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_uint8_to_string(value__228 uint8) string {
    var t854 uint64 = uint64(uint8(value__228))
    var t855 string = decimal_string(t854)
    return t855
}

func __goml_builtin_int8_to_string(value__223 int8) string {
    var t858 int64 = int64(int8(value__223))
    var inline1963 bool = t858 < 0
    if inline1963 {
        var inline1964 uint64 = uint64(int64(t858))
        var inline1965 uint64 = 0 - inline1964
        var inline1966 string = decimal_string(inline1965)
        var inline1967 string = "-" + inline1966
        return inline1967
    } else {
        var inline1968 uint64 = uint64(int64(t858))
        var inline1969 string = decimal_string(inline1968)
        return inline1969
    }
}

func __goml_builtin_int16_to_string(value__224 int16) string {
    var t862 int64 = int64(int16(value__224))
    var inline1971 bool = t862 < 0
    if inline1971 {
        var inline1972 uint64 = uint64(int64(t862))
        var inline1973 uint64 = 0 - inline1972
        var inline1974 string = decimal_string(inline1973)
        var inline1975 string = "-" + inline1974
        return inline1975
    } else {
        var inline1976 uint64 = uint64(int64(t862))
        var inline1977 string = decimal_string(inline1976)
        return inline1977
    }
}

func __goml_builtin_uint16_to_string(value__229 uint16) string {
    var t866 uint64 = uint64(uint16(value__229))
    var t867 string = decimal_string(t866)
    return t867
}

func __goml_builtin_uint32_to_string(value__230 uint32) string {
    var t870 uint64 = uint64(uint32(value__230))
    var t871 string = decimal_string(t870)
    return t871
}

func __goml_builtin_int64_to_string(value__226 int64) string {
    var inline1979 bool = value__226 < 0
    if inline1979 {
        var inline1980 uint64 = uint64(int64(value__226))
        var inline1981 uint64 = 0 - inline1980
        var inline1982 string = decimal_string(inline1981)
        var inline1983 string = "-" + inline1982
        return inline1983
    } else {
        var inline1984 uint64 = uint64(int64(value__226))
        var inline1985 string = decimal_string(inline1984)
        return inline1985
    }
}

func __goml_builtin_uint64_to_string(value__231 uint64) string {
    var t877 string = decimal_string(value__231)
    return t877
}

func __goml_builtin_float32_to_string(value__194 float32) string {
    var t880 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(value__194)
    var t881 uint64 = uint64(uint32(t880))
    var t882 string = format_float_bits(t881, 23, 8, 127)
    return t882
}

func decimal_string(value__208 uint64) string {
    var t905 bool = value__208 == 0
    if t905 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop898:
        for {
            var t899 bool = remaining__210 > 0
            if t899 {
                var t900_rhs uint64 = 10
                var t900 uint64 = remaining__210 % t900_rhs
                var t901 uint8 = uint8(uint64(t900))
                var t902 uint8 = t901 + 48
                vec_push__Vec_5uint8(reversed__209, t902)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t903 uint64 = compound_old353 / compound_value354
                remaining__210 = t903
                continue
            } else {
                break Loop_loop898
            }
        }
        var t887 int
        var inline1995 int = vec_len__Vec_5uint8(reversed__209)
        t887 = inline1995
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t887)
        var offset__212 int = 0
        Loop_loop889:
        for {
            var t890 int
            var inline1993 int = vec_len__Vec_5uint8(reversed__209)
            t890 = inline1993
            var t891 bool = offset__212 < t890
            if t891 {
                var t892 int
                var inline1991 int = vec_len__Vec_5uint8(reversed__209)
                t892 = inline1991
                var t893 int = t892 - offset__212
                var t894 int = t893 - 1
                var t895 uint8 = vec_get__Vec_5uint8(reversed__209, t894)
                vec_push__Vec_5uint8(bytes__211, t895)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t896 int = compound_old358 + compound_value359
                offset__212 = t896
                continue
            } else {
                break Loop_loop889
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t919 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t919
    var t920 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t920 != 0
    var t921_lhs uint64 = 1
    var t921 uint64 = t921_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t921 - 1
    var t922 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t922 & exponent_mask__166
    var t923_lhs uint64 = 1
    var t923 uint64 = t923_lhs << mantissa_bits__161
    var t924 uint64 = t923 - 1
    var fraction__168 uint64 = bits__160 & t924
    var t988 bool = exponent__167 == exponent_mask__166
    if t988 {
        var t990 bool = fraction__168 == 0
        if t990 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t996 bool = exponent__167 == 0
        var jp994 bool
        if t996 {
            var t997 bool = fraction__168 == 0
            jp994 = t997
        } else {
            jp994 = false
        }
        if jp994 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t985 bool = exponent__167 == 0
            var jp927 uint64
            if t985 {
                jp927 = fraction__168
            } else {
                var t986_lhs uint64 = 1
                var t986 uint64 = t986_lhs << mantissa_bits__161
                var t987 uint64 = fraction__168 | t986
                jp927 = t987
            }
            var t979 bool = exponent__167 == 0
            var jp929 int
            if t979 {
                var t980 int = 1 - exponent_bias__163
                var t981 int = t980 - mantissa_bits__161
                jp929 = t981
            } else {
                var t982 int = int(uint64(exponent__167))
                var t983 int = t982 - exponent_bias__163
                var t984 int = t983 - mantissa_bits__161
                jp929 = t984
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp927)
            var t934 bool = jp929 >= 0
            var jp931 int
            if t934 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp929)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t953 bool = mantissa_bits__161 == 23
                var jp936 int
                if t953 {
                    jp936 = 9
                } else {
                    jp936 = 17
                }
                var t950 int
                var inline2003 int = _goml_runtime_core_string_len(digits__173)
                t950 = inline2003
                var t951 bool = t950 < jp936
                var jp938 int
                if t951 {
                    var inline1997 int = _goml_runtime_core_string_len(digits__173)
                    jp938 = inline1997
                } else {
                    jp938 = jp936
                }
                var count__176 int = 1
                Loop_loop941:
                for {
                    var t942 bool = count__176 <= jp938
                    if t942 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t943 int
                        var inline1999 int = _goml_runtime_core_string_len(digits__173)
                        t943 = inline1999
                        var jp945 int
                        if x319 {
                            jp945 = 1
                        } else {
                            jp945 = 0
                        }
                        var point__180 int = t943 + jp945
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t949 bool = x322 == bits__160
                        if t949 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t947 int = compound_old324 + compound_value325
                            count__176 = t947
                            continue
                        }
                    } else {
                        break Loop_loop941
                    }
                }
                var inline2001 int = _goml_runtime_core_string_len(digits__173)
                jp931 = inline2001
                var t932 string = float_natural_decimal(exact_value__171)
                var t933 string = fixed_float_text(t932, jp931, negative__165)
                return t933
            } else {
                var count__183 int = 0
                var t975 int = 0 - jp929
                Loop_loop974:
                for {
                    var t976 bool = count__183 < t975
                    if t976 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t977 int = compound_old329 + compound_value330
                        count__183 = t977
                        continue
                    } else {
                        break Loop_loop974
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t955 int
                var inline2009 int = _goml_runtime_core_string_len(digits__184)
                t955 = inline2009
                var point__185 int = t955 + jp929
                var t973 bool = mantissa_bits__161 == 23
                var jp957 int
                if t973 {
                    jp957 = 9
                } else {
                    jp957 = 17
                }
                var t970 int
                var inline2007 int = _goml_runtime_core_string_len(digits__184)
                t970 = inline2007
                var t971 bool = t970 < jp957
                var jp959 int
                if t971 {
                    var inline2005 int = _goml_runtime_core_string_len(digits__184)
                    jp959 = inline2005
                } else {
                    jp959 = jp957
                }
                count__183 = 1
                Loop_loop961:
                for {
                    var t962 bool = count__183 <= jp959
                    if t962 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp964 int
                        if x336 {
                            jp964 = 1
                        } else {
                            jp964 = 0
                        }
                        var t965 int = point__185 + jp964
                        var candidate__191 string = fixed_float_text(rounded__190, t965, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t969 bool = x339 == bits__160
                        if t969 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t967 int = compound_old341 + compound_value342
                            count__183 = t967
                            continue
                        }
                    } else {
                        break Loop_loop961
                    }
                }
                jp931 = point__185
                var t932 string = float_natural_decimal(exact_value__171)
                var t933 string = fixed_float_text(t932, jp931, negative__165)
                return t933
            }
        }
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline2015 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2016 FloatNatural = FloatNatural{
        words: inline2015,
    }
    result__2 = inline2016
    var t1006 bool = value__1 != 0
    if t1006 {
        var t1007 *_goml_vec_uint32 = result__2.words
        var t1008 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t1007, t1008)
        var t1009_rhs int = 32
        var t1009 uint64 = value__1 >> t1009_rhs
        var high__3 uint32 = uint32(uint64(t1009))
        var t1011 bool = high__3 != 0
        if t1011 {
            var t1012 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t1012, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t1041 bool
    var inline2033 *_goml_vec_uint32 = value__28.words
    var inline2034 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2033)
    t1041 = inline2034
    if t1041 {
        var inline2018 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2019 FloatNatural = FloatNatural{
            words: inline2018,
        }
        return inline2019
    } else {
        var t1044 bool = bits__29 == 0
        if t1044 {
            var t1045 FloatNatural = float_natural_copy(value__28)
            return t1045
        } else {
            var result__30 FloatNatural
            var inline2030 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2031 FloatNatural = FloatNatural{
                words: inline2030,
            }
            result__30 = inline2031
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop1036:
            for {
                var t1037 bool = index__33 < word_shift__31
                if t1037 {
                    var t1038 *_goml_vec_uint32 = result__30.words
                    var inline2021 uint32 = 0
                    vec_push__Vec_6uint32(t1038, inline2021)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t1039 int = compound_old52 + compound_value53
                    index__33 = t1039
                    continue
                } else {
                    break Loop_loop1036
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop1024:
            for {
                var t1025 *_goml_vec_uint32 = value__28.words
                var t1026 int
                var inline2026 int = vec_len__Vec_6uint32(t1025)
                t1026 = inline2026
                var t1027 bool = index__33 < t1026
                if t1027 {
                    var t1028 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t1028, index__33)
                    var t1029 uint64 = uint64(uint32(word__35))
                    var t1030 uint64 = t1029 << bit_shift__32
                    var shifted__36 uint64 = t1030 | carry__34
                    var t1031 *_goml_vec_uint32 = result__30.words
                    var t1032 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t1031, t1032)
                    var t1033_rhs int = 32
                    var t1033 uint64 = shifted__36 >> t1033_rhs
                    carry__34 = t1033
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t1034 int = compound_old59 + compound_value60
                    index__33 = t1034
                    continue
                } else {
                    break Loop_loop1024
                }
            }
            var t1020 bool = carry__34 != 0
            if t1020 {
                var t1021 *_goml_vec_uint32 = result__30.words
                var t1022 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t1021, t1022)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t1068 bool
    var inline2049 *_goml_vec_uint32 = value__49.words
    var inline2050 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2049)
    t1068 = inline2050
    if t1068 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop1061:
        for {
            var t1062 bool
            var inline2038 *_goml_vec_uint32 = current__50.words
            var inline2039 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2038)
            t1062 = inline2039
            var t1063 bool = !t1062
            if t1063 {
                var t1064 uint32 = float_natural_divide_small(current__50, 10)
                var t1065 uint8 = uint8(uint32(t1064))
                var t1066 uint8 = t1065 + 48
                vec_push__Vec_5uint8(reversed__51, t1066)
                continue
            } else {
                break Loop_loop1061
            }
        }
        var t1050 int
        var inline2047 int = vec_len__Vec_5uint8(reversed__51)
        t1050 = inline2047
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1050)
        var offset__53 int = 0
        Loop_loop1052:
        for {
            var t1053 int
            var inline2045 int = vec_len__Vec_5uint8(reversed__51)
            t1053 = inline2045
            var t1054 bool = offset__53 < t1053
            if t1054 {
                var t1055 int
                var inline2043 int = vec_len__Vec_5uint8(reversed__51)
                t1055 = inline2043
                var t1056 int = t1055 - offset__53
                var t1057 int = t1056 - 1
                var t1058 uint8 = vec_get__Vec_5uint8(reversed__51, t1057)
                vec_push__Vec_5uint8(output__52, t1058)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t1059 int = compound_old98 + compound_value99
                offset__53 = t1059
                continue
            } else {
                break Loop_loop1052
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1071 int = _goml_runtime_core_string_len(self__289)
    return t1071
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t1074 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1074)
    var index__148 int = 0
    Loop_loop1129:
    for {
        var t1130 bool = index__148 < count__146
        if t1130 {
            var t1131 uint8
            var inline2054 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1131 = inline2054
            vec_push__Vec_5uint8(output__147, t1131)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1132 int = compound_old267 + compound_value268
            index__148 = t1132
            continue
        } else {
            break Loop_loop1129
        }
    }
    var t1126 int
    var inline2075 int = _goml_runtime_core_string_len(exact__145)
    t1126 = inline2075
    var t1127 bool = count__146 == t1126
    if t1127 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1128 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1128
    } else {
        var next__150 uint8
        var inline2073 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline2073
        var trailing__151 bool = false
        var t1077 int = count__146 + 1
        index__148 = t1077
        Loop_loop1118:
        for {
            var t1119 int
            var inline2058 int = _goml_runtime_core_string_len(exact__145)
            t1119 = inline2058
            var t1120 bool = index__148 < t1119
            if t1120 {
                var t1124 uint8
                var inline2056 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1124 = inline2056
                var t1125 bool = t1124 != 48
                if t1125 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1122 int = compound_old278 + compound_value279
                index__148 = t1122
                continue
            } else {
                break Loop_loop1118
            }
        }
        var t1106 bool = next__150 > 53
        var jp1080 bool
        if t1106 {
            jp1080 = true
        } else {
            var t1109 bool = next__150 == 53
            if t1109 {
                if trailing__151 {
                    jp1080 = true
                } else {
                    var t1112 int
                    var inline2060 int = vec_len__Vec_5uint8(output__147)
                    t1112 = inline2060
                    var t1113 int = t1112 - 1
                    var t1114 uint8 = vec_get__Vec_5uint8(output__147, t1113)
                    var t1115 uint8 = t1114 - 48
                    var t1116_rhs uint8 = 2
                    var t1116 uint8 = t1115 % t1116_rhs
                    var t1117 bool = t1116 == 1
                    jp1080 = t1117
                }
            } else {
                jp1080 = false
            }
        }
        if jp1080 {
            var index__153 int
            var inline2071 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline2071
            Loop_loop1094:
            for {
                var t1095 bool = index__153 > 0
                if t1095 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1096 int = compound_old282 - compound_value283
                    index__153 = t1096
                    var t1099 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1100 bool = t1099 < 57
                    if t1100 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1101 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1101)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1103 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1103
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1094
                }
            }
            var t1084 int
            var inline2069 int = vec_len__Vec_5uint8(output__147)
            t1084 = inline2069
            var t1085 int = t1084 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1085)
            var inline2066 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline2066)
            index__153 = 0
            Loop_loop1088:
            for {
                var t1089 int
                var inline2064 int = vec_len__Vec_5uint8(output__147)
                t1089 = inline2064
                var t1090 bool = index__153 < t1089
                if t1090 {
                    var t1091 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t1091)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t1092 int = compound_old302 + compound_value303
                    index__153 = t1092
                    continue
                } else {
                    break Loop_loop1088
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t1087 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t1087
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1105 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1105
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline2082 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline2082
    Loop_loop1138:
    for {
        var t1143 bool = length__159 > 1
        var jp1140 bool
        if t1143 {
            var t1144 int = length__159 - 1
            var t1145 uint8
            var inline2077 uint8 = _goml_runtime_core_string_byte_get(value__158, t1144)
            t1145 = inline2077
            var t1146 bool = t1145 == 48
            jp1140 = t1146
        } else {
            jp1140 = false
        }
        if jp1140 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1141 int = compound_old312 - compound_value313
            length__159 = t1141
            continue
        } else {
            break Loop_loop1138
        }
    }
    var inline2079 int = 0
    var inline2080 string = string_byte_slice(value__158, inline2079, length__159)
    return inline2080
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline2084 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline2084)
    } else {}
    var t1151 bool = decimal_point__138 <= 0
    if t1151 {
        var inline2099 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline2099)
        var inline2096 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline2096)
        var index__141 int = 0
        var t1161 int = 0 - decimal_point__138
        Loop_loop1160:
        for {
            var t1162 bool = index__141 < t1161
            if t1162 {
                var inline2087 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline2087)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1163 int = compound_old234 + compound_value235
                index__141 = t1163
                continue
            } else {
                break Loop_loop1160
            }
        }
        index__141 = 0
        Loop_loop1154:
        for {
            var t1155 int
            var inline2094 int = _goml_runtime_core_string_len(digits__137)
            t1155 = inline2094
            var t1156 bool = index__141 < t1155
            if t1156 {
                var t1157 uint8
                var inline2092 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1157 = inline2092
                vec_push__Vec_5uint8(bytes__140, t1157)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1158 int = compound_old240 + compound_value241
                index__141 = t1158
                continue
            } else {
                break Loop_loop1154
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1166 int
        var inline2124 int = _goml_runtime_core_string_len(digits__137)
        t1166 = inline2124
        var t1167 bool = decimal_point__138 >= t1166
        if t1167 {
            var index__142 int = 0
            Loop_loop1174:
            for {
                var t1175 int
                var inline2106 int = _goml_runtime_core_string_len(digits__137)
                t1175 = inline2106
                var t1176 bool = index__142 < t1175
                if t1176 {
                    var t1177 uint8
                    var inline2104 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1177 = inline2104
                    vec_push__Vec_5uint8(bytes__140, t1177)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1178 int = compound_old244 + compound_value245
                    index__142 = t1178
                    continue
                } else {
                    break Loop_loop1174
                }
            }
            Loop_loop1170:
            for {
                var t1171 bool = index__142 < decimal_point__138
                if t1171 {
                    var inline2108 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline2108)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1172 int = compound_old249 + compound_value250
                    index__142 = t1172
                    continue
                } else {
                    break Loop_loop1170
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1188:
            for {
                var t1189 bool = index__143 < decimal_point__138
                if t1189 {
                    var t1190 uint8
                    var inline2113 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1190 = inline2113
                    vec_push__Vec_5uint8(bytes__140, t1190)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1191 int = compound_old253 + compound_value254
                    index__143 = t1191
                    continue
                } else {
                    break Loop_loop1188
                }
            }
            var inline2121 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline2121)
            Loop_loop1182:
            for {
                var t1183 int
                var inline2119 int = _goml_runtime_core_string_len(digits__137)
                t1183 = inline2119
                var t1184 bool = index__143 < t1183
                if t1184 {
                    var t1185 uint8
                    var inline2117 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1185 = inline2117
                    vec_push__Vec_5uint8(bytes__140, t1185)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1186 int = compound_old259 + compound_value260
                    index__143 = t1186
                    continue
                } else {
                    break Loop_loop1182
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        }
    }
}

func parsed_float_bits(value__107 string, mantissa_bits__108 int, exponent_bias__109 int) Tuple2_4bool_6uint64 {
    var parsed__110 ParsedFloat = parse_float_text(value__107)
    var t1287 bool = parsed__110.valid
    var t1288 bool = !t1287
    if t1288 {
        var t1289 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t1289
    } else {
        var t1281 bool = parsed__110.negative
        var jp1198 uint64
        if t1281 {
            var t1286 bool = mantissa_bits__108 == 23
            var jp1283 int
            if t1286 {
                jp1283 = 8
            } else {
                jp1283 = 11
            }
            var t1284 int = mantissa_bits__108 + jp1283
            var t1285_lhs uint64 = 1
            var t1285 uint64 = t1285_lhs << t1284
            jp1198 = t1285
        } else {
            jp1198 = 0
        }
        var t1280 bool = mantissa_bits__108 == 23
        var jp1200 int
        if t1280 {
            jp1200 = 8
        } else {
            jp1200 = 11
        }
        var t1201_lhs uint64 = 1
        var t1201 uint64 = t1201_lhs << jp1200
        var t1202 uint64 = t1201 - 1
        var exponent_mask__112 uint64 = t1202 << mantissa_bits__108
        var t1258 int = parsed__110.special
        var t1259 bool = t1258 == 1
        if t1259 {
            var t1260 uint64 = jp1198 | exponent_mask__112
            var t1261 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t1260,
            }
            return t1261
        } else {
            var t1263 int = parsed__110.special
            var t1264 bool = t1263 == 2
            if t1264 {
                var t1268 int = mantissa_bits__108 - 1
                var t1269_lhs uint64 = 1
                var t1269 uint64 = t1269_lhs << t1268
                var t1270 uint64 = exponent_mask__112 | t1269
                var t1275 bool = mantissa_bits__108 == 52
                var jp1272 uint64
                if t1275 {
                    jp1272 = 1
                } else {
                    jp1272 = 0
                }
                var t1273 uint64 = t1270 | jp1272
                var t1274 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t1273,
                }
                return t1274
            } else {
                var t1277 FloatNatural = parsed__110.numerator
                var t1278 bool
                var inline2126 *_goml_vec_uint32 = t1277.words
                var inline2127 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2126)
                t1278 = inline2127
                if t1278 {
                    var t1279 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp1198,
                    }
                    return t1279
                } else {
                    var t1241 bool = parsed__110.hexadecimal
                    var t1242 bool = !t1241
                    if t1242 {
                        var t1243 int = parsed__110.significant_digits
                        var t1244 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t1243 + t1244
                        var t1257 bool = mantissa_bits__108 == 23
                        var jp1246 int
                        if t1257 {
                            jp1246 = 40
                        } else {
                            jp1246 = 310
                        }
                        var t1256 bool = mantissa_bits__108 == 23
                        var jp1248 int
                        if t1256 {
                            jp1248 = -46
                        } else {
                            jp1248 = -325
                        }
                        var t1250 bool = decimal_position__113 > jp1246
                        if t1250 {
                            var t1251 uint64 = jp1198 | exponent_mask__112
                            var t1252 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t1251,
                            }
                            return t1252
                        } else {
                            var t1254 bool = decimal_position__113 < jp1248
                            if t1254 {
                                var t1255 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp1198,
                                }
                                return t1255
                            } else {
                                var t1237 bool = parsed__110.hexadecimal
                                var t1238 bool = !t1237
                                var jp1232 bool
                                if t1238 {
                                    var t1239 int = parsed__110.decimal_exponent
                                    var t1240 bool = t1239 < 0
                                    jp1232 = t1240
                                } else {
                                    jp1232 = false
                                }
                                var jp1206 FloatNatural
                                if jp1232 {
                                    var t1233 int = parsed__110.decimal_exponent
                                    var t1234 int = 0 - t1233
                                    var t1235 FloatNatural = float_natural_power5(t1234)
                                    jp1206 = t1235
                                } else {
                                    var inline2129 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline2129, 1)
                                    var inline2131 FloatNatural = FloatNatural{
                                        words: inline2129,
                                    }
                                    jp1206 = inline2131
                                }
                                var t1227 bool = parsed__110.hexadecimal
                                var t1228 bool = !t1227
                                var jp1218 bool
                                if t1228 {
                                    var t1229 int = parsed__110.decimal_exponent
                                    var t1230 bool = t1229 > 0
                                    jp1218 = t1230
                                } else {
                                    jp1218 = false
                                }
                                var jp1208 FloatNatural
                                if jp1218 {
                                    var t1219 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t1219)
                                    var count__118 int = 0
                                    Loop_loop1221:
                                    for {
                                        var t1222 int = parsed__110.decimal_exponent
                                        var t1223 bool = count__118 < t1222
                                        if t1223 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t1224 int = compound_old213 + compound_value214
                                            count__118 = t1224
                                            continue
                                        } else {
                                            break Loop_loop1221
                                        }
                                    }
                                    jp1208 = result__117
                                    var t1214 bool = parsed__110.hexadecimal
                                    var jp1210 int
                                    if t1214 {
                                        var t1215 int = parsed__110.binary_exponent
                                        jp1210 = t1215
                                    } else {
                                        var t1216 int = parsed__110.decimal_exponent
                                        jp1210 = t1216
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1208, jp1206, jp1210, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1211 bool = !x219
                                    var t1212 uint64 = jp1198 | x218
                                    var t1213 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1211,
                                        _1: t1212,
                                    }
                                    return t1213
                                } else {
                                    var t1226 FloatNatural = parsed__110.numerator
                                    jp1208 = t1226
                                    var t1214 bool = parsed__110.hexadecimal
                                    var jp1210 int
                                    if t1214 {
                                        var t1215 int = parsed__110.binary_exponent
                                        jp1210 = t1215
                                    } else {
                                        var t1216 int = parsed__110.decimal_exponent
                                        jp1210 = t1216
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1208, jp1206, jp1210, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1211 bool = !x219
                                    var t1212 uint64 = jp1198 | x218
                                    var t1213 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1211,
                                        _1: t1212,
                                    }
                                    return t1213
                                }
                            }
                        }
                    } else {
                        var t1237 bool = parsed__110.hexadecimal
                        var t1238 bool = !t1237
                        var jp1232 bool
                        if t1238 {
                            var t1239 int = parsed__110.decimal_exponent
                            var t1240 bool = t1239 < 0
                            jp1232 = t1240
                        } else {
                            jp1232 = false
                        }
                        var jp1206 FloatNatural
                        if jp1232 {
                            var t1233 int = parsed__110.decimal_exponent
                            var t1234 int = 0 - t1233
                            var t1235 FloatNatural = float_natural_power5(t1234)
                            jp1206 = t1235
                        } else {
                            var inline2129 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline2129, 1)
                            var inline2131 FloatNatural = FloatNatural{
                                words: inline2129,
                            }
                            jp1206 = inline2131
                        }
                        var t1227 bool = parsed__110.hexadecimal
                        var t1228 bool = !t1227
                        var jp1218 bool
                        if t1228 {
                            var t1229 int = parsed__110.decimal_exponent
                            var t1230 bool = t1229 > 0
                            jp1218 = t1230
                        } else {
                            jp1218 = false
                        }
                        var jp1208 FloatNatural
                        if jp1218 {
                            var t1219 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t1219)
                            var count__118 int = 0
                            Loop_loop1221__2:
                            for {
                                var t1222 int = parsed__110.decimal_exponent
                                var t1223 bool = count__118 < t1222
                                if t1223 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t1224 int = compound_old213 + compound_value214
                                    count__118 = t1224
                                    continue
                                } else {
                                    break Loop_loop1221__2
                                }
                            }
                            jp1208 = result__117
                            var t1214 bool = parsed__110.hexadecimal
                            var jp1210 int
                            if t1214 {
                                var t1215 int = parsed__110.binary_exponent
                                jp1210 = t1215
                            } else {
                                var t1216 int = parsed__110.decimal_exponent
                                jp1210 = t1216
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1208, jp1206, jp1210, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1211 bool = !x219
                            var t1212 uint64 = jp1198 | x218
                            var t1213 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1211,
                                _1: t1212,
                            }
                            return t1213
                        } else {
                            var t1226 FloatNatural = parsed__110.numerator
                            jp1208 = t1226
                            var t1214 bool = parsed__110.hexadecimal
                            var jp1210 int
                            if t1214 {
                                var t1215 int = parsed__110.binary_exponent
                                jp1210 = t1215
                            } else {
                                var t1216 int = parsed__110.decimal_exponent
                                jp1210 = t1216
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1208, jp1206, jp1210, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1211 bool = !x219
                            var t1212 uint64 = jp1198 | x218
                            var t1213 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1211,
                                _1: t1212,
                            }
                            return t1213
                        }
                    }
                }
            }
        }
    }
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1311 bool = factor__16 == 0
    if t1311 {
        var t1312 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1312, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1305 uint64 = uint64(uint32(factor__16))
        Loop_loop1298:
        for {
            var t1299 *_goml_vec_uint32 = value__15.words
            var t1300 int
            var inline2135 int = vec_len__Vec_6uint32(t1299)
            t1300 = inline2135
            var t1301 bool = index__18 < t1300
            if t1301 {
                var t1302 *_goml_vec_uint32 = value__15.words
                var t1303 uint32 = vec_get__Vec_6uint32(t1302, index__18)
                var t1304 uint64 = uint64(uint32(t1303))
                var t1306 uint64 = t1304 * t1305
                var product__19 uint64 = t1306 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1308_rhs int = 32
                var t1308 uint64 = product__19 >> t1308_rhs
                carry__17 = t1308
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1309 int = compound_old30 + compound_value31
                index__18 = t1309
                continue
            } else {
                break Loop_loop1298
            }
        }
        var t1294 bool = carry__17 != 0
        if t1294 {
            var t1295 *_goml_vec_uint32 = value__15.words
            var t1296 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1295, t1296)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_natural_zero() FloatNatural {
    var t1315 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1316 FloatNatural = FloatNatural{
        words: t1315,
    }
    return t1316
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline2146 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2147 FloatNatural = FloatNatural{
        words: inline2146,
    }
    result__5 = inline2147
    var index__6 int = 0
    Loop_loop1326:
    for {
        var t1327 *_goml_vec_uint32 = value__4.words
        var t1328 int
        var inline2144 int = vec_len__Vec_6uint32(t1327)
        t1328 = inline2144
        var t1329 bool = index__6 < t1328
        if t1329 {
            var t1330 *_goml_vec_uint32 = result__5.words
            var t1331 *_goml_vec_uint32 = value__4.words
            var t1332 uint32 = vec_get__Vec_6uint32(t1331, index__6)
            vec_push__Vec_6uint32(t1330, t1332)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1333 int = compound_old4 + compound_value5
            index__6 = t1333
            continue
        } else {
            break Loop_loop1326
        }
    }
    return result__5
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1340 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline2149 int = vec_len__Vec_6uint32(t1340)
    index__47 = inline2149
    var t1351 uint64 = uint64(uint32(divisor__45))
    var t1354 uint64 = uint64(uint32(divisor__45))
    Loop_loop1343:
    for {
        var t1344 bool = index__47 > 0
        if t1344 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1345 int = compound_old83 - compound_value84
            index__47 = t1345
            var t1347_rhs int = 32
            var t1347 uint64 = remainder__46 << t1347_rhs
            var t1348 *_goml_vec_uint32 = value__44.words
            var t1349 uint32 = vec_get__Vec_6uint32(t1348, index__47)
            var t1350 uint64 = uint64(uint32(t1349))
            var current__48 uint64 = t1347 | t1350
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1352 uint64 = current__48 / t1351
            var value90 uint32 = uint32(uint64(t1352))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1355 uint64 = current__48 % t1354
            remainder__46 = t1355
            continue
        } else {
            break Loop_loop1343
        }
    }
    float_natural_trim(value__44)
    var t1342 uint32 = uint32(uint64(remainder__46))
    return t1342
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1358 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1358
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline2151 bool = string_is_char_boundary(self__294, start__295)
    var inline2153 bool
    if inline2151 {
        var inline2156 bool = string_is_char_boundary(self__294, end__296)
        inline2153 = inline2156
    } else {
        inline2153 = false
    }
    if inline2153 {
        var inline2154 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline2154
    } else {
        var inline2155 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2155
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1546 bool = string_equals_ascii_case(value__84, "nan")
    if t1546 {
        var t1547 FloatNatural
        var inline2158 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2159 FloatNatural = FloatNatural{
            words: inline2158,
        }
        t1547 = inline2159
        var t1548 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1547,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1548
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1538 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1539 bool = index__85 < t1538
        var jp1533 bool
        if t1539 {
            var t1542 uint8
            var inline2163 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1542 = inline2163
            var t1543 bool = t1542 == 43
            if t1543 {
                jp1533 = true
            } else {
                var t1544 uint8
                var inline2161 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1544 = inline2161
                var t1545 bool = t1544 == 45
                jp1533 = t1545
            }
        } else {
            jp1533 = false
        }
        if jp1533 {
            var t1534 uint8
            var inline2165 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1534 = inline2165
            var t1535 bool = t1534 == 45
            negative__86 = t1535
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1536 int = compound_old140 + compound_value141
            index__85 = t1536
        } else {}
        var t1366 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1366)
        var t1530 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1527 bool
        if t1530 {
            jp1527 = true
        } else {
            var t1531 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1527 = t1531
        }
        if jp1527 {
            var t1528 FloatNatural
            var inline2167 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2168 FloatNatural = FloatNatural{
                words: inline2167,
            }
            t1528 = inline2168
            var t1529 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1528,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1529
        } else {
            var t1521 int = index__85 + 2
            var t1522 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1523 bool = t1521 <= t1522
            var jp1516 bool
            if t1523 {
                var t1524 uint8
                var inline2170 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1524 = inline2170
                var t1525 bool = t1524 == 48
                jp1516 = t1525
            } else {
                jp1516 = false
            }
            var jp1369 bool
            if jp1516 {
                var t1517 int = index__85 + 1
                var t1518 uint8
                var inline2179 uint8 = _goml_runtime_core_string_byte_get(value__84, t1517)
                t1518 = inline2179
                var t1519 uint8
                var inline2172 bool = t1518 >= 65
                var inline2174 bool
                if inline2172 {
                    var inline2177 bool = t1518 <= 90
                    inline2174 = inline2177
                } else {
                    inline2174 = false
                }
                if inline2174 {
                    var inline2175 uint8 = 97 - 65
                    var inline2176 uint8 = t1518 + inline2175
                    t1519 = inline2176
                    var t1520 bool = t1519 == 120
                    jp1369 = t1520
                    if jp1369 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1513 int = compound_old145 + compound_value146
                        index__85 = t1513
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1372 int
                    if jp1369 {
                        jp1372 = 16
                    } else {
                        jp1372 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1466 uint32 = uint32(int(jp1372))
                    Loop_loop1462:
                    for {
                        var t1463 int
                        var inline2193 int = _goml_runtime_core_string_len(value__84)
                        t1463 = inline2193
                        var t1464 bool = index__85 < t1463
                        if t1464 {
                            var current__97 uint8
                            var inline2191 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2191
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1372)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1466)
                                var t1467 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1467)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1478 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1478
                                } else {}
                                var t1476 bool = significant_digits__95 > 0
                                var jp1473 bool
                                if t1476 {
                                    jp1473 = true
                                } else {
                                    var t1477 bool = x151 != 0
                                    jp1473 = t1477
                                }
                                if jp1473 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1474 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1474
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1470 int = compound_old164 + compound_value165
                                index__85 = t1470
                                continue
                            } else {
                                var t1481 bool = current__97 == 95
                                if t1481 {
                                    var t1502 int = index__85 + 1
                                    var t1503 int
                                    var inline2189 int = _goml_runtime_core_string_len(value__84)
                                    t1503 = inline2189
                                    var t1504 bool = t1502 >= t1503
                                    if t1504 {
                                        var inline2181 FloatNatural = float_natural_zero()
                                        var inline2182 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2181,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2182
                                    } else {
                                        var t1483 int = index__85 + 1
                                        var t1484 uint8
                                        var inline2187 uint8 = _goml_runtime_core_string_byte_get(value__84, t1483)
                                        t1484 = inline2187
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1484, jp1372)
                                        var x169 bool = mtmp168._0
                                        var jp1499 bool
                                        if jp1369 {
                                            var t1501 bool = !saw_digit__92
                                            jp1499 = t1501
                                        } else {
                                            jp1499 = false
                                        }
                                        var jp1486 bool
                                        if jp1499 {
                                            var t1500 bool = index__85 == mantissa_start__89
                                            jp1486 = t1500
                                        } else {
                                            jp1486 = false
                                        }
                                        var t1496 bool = !previous_digit__96
                                        var jp1494 bool
                                        if t1496 {
                                            var t1497 bool = !jp1486
                                            jp1494 = t1497
                                        } else {
                                            jp1494 = false
                                        }
                                        var jp1491 bool
                                        if jp1494 {
                                            jp1491 = true
                                        } else {
                                            var t1495 bool = !x169
                                            jp1491 = t1495
                                        }
                                        if jp1491 {
                                            var inline2184 FloatNatural = float_natural_zero()
                                            var inline2185 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2184,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2185
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1488 int = compound_old173 + compound_value174
                                            index__85 = t1488
                                            continue
                                        }
                                    }
                                } else {
                                    var t1511 bool = current__97 == 46
                                    var jp1508 bool
                                    if t1511 {
                                        var t1512 bool = !saw_dot__93
                                        jp1508 = t1512
                                    } else {
                                        jp1508 = false
                                    }
                                    if jp1508 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1509 int = compound_old178 + compound_value179
                                        index__85 = t1509
                                        continue
                                    } else {
                                        break Loop_loop1462
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1462
                        }
                    }
                    var t1460 bool = !saw_digit__92
                    if t1460 {
                        var inline2195 FloatNatural = float_natural_zero()
                        var inline2196 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2195,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2196
                    } else {
                        var jp1376 uint8
                        if jp1369 {
                            jp1376 = 112
                        } else {
                            jp1376 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1455 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1456 bool = index__85 < t1455
                        var jp1393 bool
                        if t1456 {
                            var t1457 uint8
                            var inline2198 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1457 = inline2198
                            var t1458 uint8 = ascii_lower(t1457)
                            var t1459 bool = t1458 == jp1376
                            jp1393 = t1459
                        } else {
                            jp1393 = false
                        }
                        if jp1393 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1394 int = compound_old183 + compound_value184
                            index__85 = t1394
                            var t1445 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1446 bool = index__85 < t1445
                            var jp1440 bool
                            if t1446 {
                                var t1449 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1450 bool = t1449 == 43
                                if t1450 {
                                    jp1440 = true
                                } else {
                                    var t1451 uint8
                                    var inline2200 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1451 = inline2200
                                    var t1452 bool = t1451 == 45
                                    jp1440 = t1452
                                }
                            } else {
                                jp1440 = false
                            }
                            if jp1440 {
                                var t1441 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1442 bool = t1441 == 45
                                exponent_negative__104 = t1442
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1443 int = compound_old187 + compound_value188
                                index__85 = t1443
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1401:
                            for {
                                var t1402 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1403 bool = index__85 < t1402
                                if t1403 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1437 bool = current__106 >= 48
                                    var jp1406 bool
                                    if t1437 {
                                        var t1438 bool = current__106 <= 57
                                        jp1406 = t1438
                                    } else {
                                        jp1406 = false
                                    }
                                    if jp1406 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1410 bool = exponent__103 < 1000000
                                        if t1410 {
                                            var t1411 int = exponent__103 * 10
                                            var t1412 uint8 = current__106 - 48
                                            var t1413 int = int(uint8(t1412))
                                            var t1414 int = t1411 + t1413
                                            exponent__103 = t1414
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1408 int = compound_old196 + compound_value197
                                        index__85 = t1408
                                        continue
                                    } else {
                                        var t1416 bool = current__106 == 95
                                        if t1416 {
                                            var t1433 bool = !previous_digit__96
                                            var jp1429 bool
                                            if t1433 {
                                                jp1429 = true
                                            } else {
                                                var t1434 int = index__85 + 1
                                                var t1435 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1436 bool = t1434 >= t1435
                                                jp1429 = t1436
                                            }
                                            var jp1424 bool
                                            if jp1429 {
                                                jp1424 = true
                                            } else {
                                                var t1430 int = index__85 + 1
                                                var t1431 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1430)
                                                var t1432 bool = t1431 < 48
                                                jp1424 = t1432
                                            }
                                            var jp1421 bool
                                            if jp1424 {
                                                jp1421 = true
                                            } else {
                                                var t1425 int = index__85 + 1
                                                var t1426 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1425)
                                                var t1427 bool = t1426 > 57
                                                jp1421 = t1427
                                            }
                                            if jp1421 {
                                                var t1422 ParsedFloat = invalid_parsed_float()
                                                return t1422
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1418 int = compound_old201 + compound_value202
                                                index__85 = t1418
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1401
                                        }
                                    }
                                } else {
                                    break Loop_loop1401
                                }
                            }
                            var t1399 bool = !exponent_digits__105
                            if t1399 {
                                var t1400 ParsedFloat = invalid_parsed_float()
                                return t1400
                            } else {
                                var t1389 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1390 bool = index__85 != t1389
                                if t1390 {
                                    var t1391 ParsedFloat = invalid_parsed_float()
                                    return t1391
                                } else {
                                    if exponent_negative__104 {
                                        var t1388 int = 0 - exponent__103
                                        exponent__103 = t1388
                                    } else {}
                                    var jp1381 int
                                    if jp1369 {
                                        jp1381 = 0
                                    } else {
                                        var t1387 int = exponent__103 - fraction_digits__94
                                        jp1381 = t1387
                                    }
                                    var jp1383 int
                                    if jp1369 {
                                        var t1385 int = fraction_digits__94 * 4
                                        var t1386 int = exponent__103 - t1385
                                        jp1383 = t1386
                                    } else {
                                        jp1383 = 0
                                    }
                                    var t1384 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1381,
                                        binary_exponent: jp1383,
                                        hexadecimal: jp1369,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1384
                                }
                            }
                        } else {
                            if jp1369 {
                                var t1454 ParsedFloat = invalid_parsed_float()
                                return t1454
                            } else {
                                var t1389 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1390 bool = index__85 != t1389
                                if t1390 {
                                    var t1391 ParsedFloat = invalid_parsed_float()
                                    return t1391
                                } else {
                                    if exponent_negative__104 {
                                        var t1388 int = 0 - exponent__103
                                        exponent__103 = t1388
                                    } else {}
                                    var jp1381 int
                                    if jp1369 {
                                        jp1381 = 0
                                    } else {
                                        var t1387 int = exponent__103 - fraction_digits__94
                                        jp1381 = t1387
                                    }
                                    var jp1383 int
                                    if jp1369 {
                                        var t1385 int = fraction_digits__94 * 4
                                        var t1386 int = exponent__103 - t1385
                                        jp1383 = t1386
                                    } else {
                                        jp1383 = 0
                                    }
                                    var t1384 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1381,
                                        binary_exponent: jp1383,
                                        hexadecimal: jp1369,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1384
                                }
                            }
                        }
                    }
                } else {
                    t1519 = t1518
                    var t1520 bool = t1519 == 120
                    jp1369 = t1520
                    if jp1369 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1513 int = compound_old145 + compound_value146
                        index__85 = t1513
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1372 int
                    if jp1369 {
                        jp1372 = 16
                    } else {
                        jp1372 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1466 uint32 = uint32(int(jp1372))
                    Loop_loop1462__2:
                    for {
                        var t1463 int
                        var inline2193 int = _goml_runtime_core_string_len(value__84)
                        t1463 = inline2193
                        var t1464 bool = index__85 < t1463
                        if t1464 {
                            var current__97 uint8
                            var inline2191 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2191
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1372)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1466)
                                var t1467 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1467)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1478 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1478
                                } else {}
                                var t1476 bool = significant_digits__95 > 0
                                var jp1473 bool
                                if t1476 {
                                    jp1473 = true
                                } else {
                                    var t1477 bool = x151 != 0
                                    jp1473 = t1477
                                }
                                if jp1473 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1474 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1474
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1470 int = compound_old164 + compound_value165
                                index__85 = t1470
                                continue
                            } else {
                                var t1481 bool = current__97 == 95
                                if t1481 {
                                    var t1502 int = index__85 + 1
                                    var t1503 int
                                    var inline2189 int = _goml_runtime_core_string_len(value__84)
                                    t1503 = inline2189
                                    var t1504 bool = t1502 >= t1503
                                    if t1504 {
                                        var inline2181 FloatNatural = float_natural_zero()
                                        var inline2182 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2181,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2182
                                    } else {
                                        var t1483 int = index__85 + 1
                                        var t1484 uint8
                                        var inline2187 uint8 = _goml_runtime_core_string_byte_get(value__84, t1483)
                                        t1484 = inline2187
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1484, jp1372)
                                        var x169 bool = mtmp168._0
                                        var jp1499 bool
                                        if jp1369 {
                                            var t1501 bool = !saw_digit__92
                                            jp1499 = t1501
                                        } else {
                                            jp1499 = false
                                        }
                                        var jp1486 bool
                                        if jp1499 {
                                            var t1500 bool = index__85 == mantissa_start__89
                                            jp1486 = t1500
                                        } else {
                                            jp1486 = false
                                        }
                                        var t1496 bool = !previous_digit__96
                                        var jp1494 bool
                                        if t1496 {
                                            var t1497 bool = !jp1486
                                            jp1494 = t1497
                                        } else {
                                            jp1494 = false
                                        }
                                        var jp1491 bool
                                        if jp1494 {
                                            jp1491 = true
                                        } else {
                                            var t1495 bool = !x169
                                            jp1491 = t1495
                                        }
                                        if jp1491 {
                                            var inline2184 FloatNatural = float_natural_zero()
                                            var inline2185 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2184,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2185
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1488 int = compound_old173 + compound_value174
                                            index__85 = t1488
                                            continue
                                        }
                                    }
                                } else {
                                    var t1511 bool = current__97 == 46
                                    var jp1508 bool
                                    if t1511 {
                                        var t1512 bool = !saw_dot__93
                                        jp1508 = t1512
                                    } else {
                                        jp1508 = false
                                    }
                                    if jp1508 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1509 int = compound_old178 + compound_value179
                                        index__85 = t1509
                                        continue
                                    } else {
                                        break Loop_loop1462__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1462__2
                        }
                    }
                    var t1460 bool = !saw_digit__92
                    if t1460 {
                        var inline2195 FloatNatural = float_natural_zero()
                        var inline2196 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2195,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2196
                    } else {
                        var jp1376 uint8
                        if jp1369 {
                            jp1376 = 112
                        } else {
                            jp1376 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1455 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1456 bool = index__85 < t1455
                        var jp1393 bool
                        if t1456 {
                            var t1457 uint8
                            var inline2198 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1457 = inline2198
                            var t1458 uint8 = ascii_lower(t1457)
                            var t1459 bool = t1458 == jp1376
                            jp1393 = t1459
                        } else {
                            jp1393 = false
                        }
                        if jp1393 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1394 int = compound_old183 + compound_value184
                            index__85 = t1394
                            var t1445 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1446 bool = index__85 < t1445
                            var jp1440 bool
                            if t1446 {
                                var t1449 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1450 bool = t1449 == 43
                                if t1450 {
                                    jp1440 = true
                                } else {
                                    var t1451 uint8
                                    var inline2200 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1451 = inline2200
                                    var t1452 bool = t1451 == 45
                                    jp1440 = t1452
                                }
                            } else {
                                jp1440 = false
                            }
                            if jp1440 {
                                var t1441 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1442 bool = t1441 == 45
                                exponent_negative__104 = t1442
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1443 int = compound_old187 + compound_value188
                                index__85 = t1443
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1401__2:
                            for {
                                var t1402 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1403 bool = index__85 < t1402
                                if t1403 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1437 bool = current__106 >= 48
                                    var jp1406 bool
                                    if t1437 {
                                        var t1438 bool = current__106 <= 57
                                        jp1406 = t1438
                                    } else {
                                        jp1406 = false
                                    }
                                    if jp1406 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1410 bool = exponent__103 < 1000000
                                        if t1410 {
                                            var t1411 int = exponent__103 * 10
                                            var t1412 uint8 = current__106 - 48
                                            var t1413 int = int(uint8(t1412))
                                            var t1414 int = t1411 + t1413
                                            exponent__103 = t1414
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1408 int = compound_old196 + compound_value197
                                        index__85 = t1408
                                        continue
                                    } else {
                                        var t1416 bool = current__106 == 95
                                        if t1416 {
                                            var t1433 bool = !previous_digit__96
                                            var jp1429 bool
                                            if t1433 {
                                                jp1429 = true
                                            } else {
                                                var t1434 int = index__85 + 1
                                                var t1435 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1436 bool = t1434 >= t1435
                                                jp1429 = t1436
                                            }
                                            var jp1424 bool
                                            if jp1429 {
                                                jp1424 = true
                                            } else {
                                                var t1430 int = index__85 + 1
                                                var t1431 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1430)
                                                var t1432 bool = t1431 < 48
                                                jp1424 = t1432
                                            }
                                            var jp1421 bool
                                            if jp1424 {
                                                jp1421 = true
                                            } else {
                                                var t1425 int = index__85 + 1
                                                var t1426 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1425)
                                                var t1427 bool = t1426 > 57
                                                jp1421 = t1427
                                            }
                                            if jp1421 {
                                                var t1422 ParsedFloat = invalid_parsed_float()
                                                return t1422
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1418 int = compound_old201 + compound_value202
                                                index__85 = t1418
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1401__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1401__2
                                }
                            }
                            var t1399 bool = !exponent_digits__105
                            if t1399 {
                                var t1400 ParsedFloat = invalid_parsed_float()
                                return t1400
                            } else {
                                var t1389 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1390 bool = index__85 != t1389
                                if t1390 {
                                    var t1391 ParsedFloat = invalid_parsed_float()
                                    return t1391
                                } else {
                                    if exponent_negative__104 {
                                        var t1388 int = 0 - exponent__103
                                        exponent__103 = t1388
                                    } else {}
                                    var jp1381 int
                                    if jp1369 {
                                        jp1381 = 0
                                    } else {
                                        var t1387 int = exponent__103 - fraction_digits__94
                                        jp1381 = t1387
                                    }
                                    var jp1383 int
                                    if jp1369 {
                                        var t1385 int = fraction_digits__94 * 4
                                        var t1386 int = exponent__103 - t1385
                                        jp1383 = t1386
                                    } else {
                                        jp1383 = 0
                                    }
                                    var t1384 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1381,
                                        binary_exponent: jp1383,
                                        hexadecimal: jp1369,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1384
                                }
                            }
                        } else {
                            if jp1369 {
                                var t1454 ParsedFloat = invalid_parsed_float()
                                return t1454
                            } else {
                                var t1389 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1390 bool = index__85 != t1389
                                if t1390 {
                                    var t1391 ParsedFloat = invalid_parsed_float()
                                    return t1391
                                } else {
                                    if exponent_negative__104 {
                                        var t1388 int = 0 - exponent__103
                                        exponent__103 = t1388
                                    } else {}
                                    var jp1381 int
                                    if jp1369 {
                                        jp1381 = 0
                                    } else {
                                        var t1387 int = exponent__103 - fraction_digits__94
                                        jp1381 = t1387
                                    }
                                    var jp1383 int
                                    if jp1369 {
                                        var t1385 int = fraction_digits__94 * 4
                                        var t1386 int = exponent__103 - t1385
                                        jp1383 = t1386
                                    } else {
                                        jp1383 = 0
                                    }
                                    var t1384 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1381,
                                        binary_exponent: jp1383,
                                        hexadecimal: jp1369,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1384
                                }
                            }
                        }
                    }
                }
            } else {
                jp1369 = false
                if jp1369 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1513 int = compound_old145 + compound_value146
                    index__85 = t1513
                } else {}
                var mantissa_start__89 int = index__85
                var jp1372 int
                if jp1369 {
                    jp1372 = 16
                } else {
                    jp1372 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1466 uint32 = uint32(int(jp1372))
                Loop_loop1462__3:
                for {
                    var t1463 int
                    var inline2193 int = _goml_runtime_core_string_len(value__84)
                    t1463 = inline2193
                    var t1464 bool = index__85 < t1463
                    if t1464 {
                        var current__97 uint8
                        var inline2191 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline2191
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1372)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1466)
                            var t1467 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1467)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1478 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1478
                            } else {}
                            var t1476 bool = significant_digits__95 > 0
                            var jp1473 bool
                            if t1476 {
                                jp1473 = true
                            } else {
                                var t1477 bool = x151 != 0
                                jp1473 = t1477
                            }
                            if jp1473 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1474 int = compound_old160 + compound_value161
                                significant_digits__95 = t1474
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1470 int = compound_old164 + compound_value165
                            index__85 = t1470
                            continue
                        } else {
                            var t1481 bool = current__97 == 95
                            if t1481 {
                                var t1502 int = index__85 + 1
                                var t1503 int
                                var inline2189 int = _goml_runtime_core_string_len(value__84)
                                t1503 = inline2189
                                var t1504 bool = t1502 >= t1503
                                if t1504 {
                                    var inline2181 FloatNatural = float_natural_zero()
                                    var inline2182 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline2181,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline2182
                                } else {
                                    var t1483 int = index__85 + 1
                                    var t1484 uint8
                                    var inline2187 uint8 = _goml_runtime_core_string_byte_get(value__84, t1483)
                                    t1484 = inline2187
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1484, jp1372)
                                    var x169 bool = mtmp168._0
                                    var jp1499 bool
                                    if jp1369 {
                                        var t1501 bool = !saw_digit__92
                                        jp1499 = t1501
                                    } else {
                                        jp1499 = false
                                    }
                                    var jp1486 bool
                                    if jp1499 {
                                        var t1500 bool = index__85 == mantissa_start__89
                                        jp1486 = t1500
                                    } else {
                                        jp1486 = false
                                    }
                                    var t1496 bool = !previous_digit__96
                                    var jp1494 bool
                                    if t1496 {
                                        var t1497 bool = !jp1486
                                        jp1494 = t1497
                                    } else {
                                        jp1494 = false
                                    }
                                    var jp1491 bool
                                    if jp1494 {
                                        jp1491 = true
                                    } else {
                                        var t1495 bool = !x169
                                        jp1491 = t1495
                                    }
                                    if jp1491 {
                                        var inline2184 FloatNatural = float_natural_zero()
                                        var inline2185 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2184,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2185
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1488 int = compound_old173 + compound_value174
                                        index__85 = t1488
                                        continue
                                    }
                                }
                            } else {
                                var t1511 bool = current__97 == 46
                                var jp1508 bool
                                if t1511 {
                                    var t1512 bool = !saw_dot__93
                                    jp1508 = t1512
                                } else {
                                    jp1508 = false
                                }
                                if jp1508 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1509 int = compound_old178 + compound_value179
                                    index__85 = t1509
                                    continue
                                } else {
                                    break Loop_loop1462__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1462__3
                    }
                }
                var t1460 bool = !saw_digit__92
                if t1460 {
                    var inline2195 FloatNatural = float_natural_zero()
                    var inline2196 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline2195,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline2196
                } else {
                    var jp1376 uint8
                    if jp1369 {
                        jp1376 = 112
                    } else {
                        jp1376 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1455 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1456 bool = index__85 < t1455
                    var jp1393 bool
                    if t1456 {
                        var t1457 uint8
                        var inline2198 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1457 = inline2198
                        var t1458 uint8 = ascii_lower(t1457)
                        var t1459 bool = t1458 == jp1376
                        jp1393 = t1459
                    } else {
                        jp1393 = false
                    }
                    if jp1393 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1394 int = compound_old183 + compound_value184
                        index__85 = t1394
                        var t1445 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1446 bool = index__85 < t1445
                        var jp1440 bool
                        if t1446 {
                            var t1449 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1450 bool = t1449 == 43
                            if t1450 {
                                jp1440 = true
                            } else {
                                var t1451 uint8
                                var inline2200 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1451 = inline2200
                                var t1452 bool = t1451 == 45
                                jp1440 = t1452
                            }
                        } else {
                            jp1440 = false
                        }
                        if jp1440 {
                            var t1441 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1442 bool = t1441 == 45
                            exponent_negative__104 = t1442
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1443 int = compound_old187 + compound_value188
                            index__85 = t1443
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1401__3:
                        for {
                            var t1402 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1403 bool = index__85 < t1402
                            if t1403 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1437 bool = current__106 >= 48
                                var jp1406 bool
                                if t1437 {
                                    var t1438 bool = current__106 <= 57
                                    jp1406 = t1438
                                } else {
                                    jp1406 = false
                                }
                                if jp1406 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1410 bool = exponent__103 < 1000000
                                    if t1410 {
                                        var t1411 int = exponent__103 * 10
                                        var t1412 uint8 = current__106 - 48
                                        var t1413 int = int(uint8(t1412))
                                        var t1414 int = t1411 + t1413
                                        exponent__103 = t1414
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1408 int = compound_old196 + compound_value197
                                    index__85 = t1408
                                    continue
                                } else {
                                    var t1416 bool = current__106 == 95
                                    if t1416 {
                                        var t1433 bool = !previous_digit__96
                                        var jp1429 bool
                                        if t1433 {
                                            jp1429 = true
                                        } else {
                                            var t1434 int = index__85 + 1
                                            var t1435 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1436 bool = t1434 >= t1435
                                            jp1429 = t1436
                                        }
                                        var jp1424 bool
                                        if jp1429 {
                                            jp1424 = true
                                        } else {
                                            var t1430 int = index__85 + 1
                                            var t1431 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1430)
                                            var t1432 bool = t1431 < 48
                                            jp1424 = t1432
                                        }
                                        var jp1421 bool
                                        if jp1424 {
                                            jp1421 = true
                                        } else {
                                            var t1425 int = index__85 + 1
                                            var t1426 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1425)
                                            var t1427 bool = t1426 > 57
                                            jp1421 = t1427
                                        }
                                        if jp1421 {
                                            var t1422 ParsedFloat = invalid_parsed_float()
                                            return t1422
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1418 int = compound_old201 + compound_value202
                                            index__85 = t1418
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1401__3
                                    }
                                }
                            } else {
                                break Loop_loop1401__3
                            }
                        }
                        var t1399 bool = !exponent_digits__105
                        if t1399 {
                            var t1400 ParsedFloat = invalid_parsed_float()
                            return t1400
                        } else {
                            var t1389 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1390 bool = index__85 != t1389
                            if t1390 {
                                var t1391 ParsedFloat = invalid_parsed_float()
                                return t1391
                            } else {
                                if exponent_negative__104 {
                                    var t1388 int = 0 - exponent__103
                                    exponent__103 = t1388
                                } else {}
                                var jp1381 int
                                if jp1369 {
                                    jp1381 = 0
                                } else {
                                    var t1387 int = exponent__103 - fraction_digits__94
                                    jp1381 = t1387
                                }
                                var jp1383 int
                                if jp1369 {
                                    var t1385 int = fraction_digits__94 * 4
                                    var t1386 int = exponent__103 - t1385
                                    jp1383 = t1386
                                } else {
                                    jp1383 = 0
                                }
                                var t1384 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1381,
                                    binary_exponent: jp1383,
                                    hexadecimal: jp1369,
                                    significant_digits: significant_digits__95,
                                }
                                return t1384
                            }
                        }
                    } else {
                        if jp1369 {
                            var t1454 ParsedFloat = invalid_parsed_float()
                            return t1454
                        } else {
                            var t1389 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1390 bool = index__85 != t1389
                            if t1390 {
                                var t1391 ParsedFloat = invalid_parsed_float()
                                return t1391
                            } else {
                                if exponent_negative__104 {
                                    var t1388 int = 0 - exponent__103
                                    exponent__103 = t1388
                                } else {}
                                var jp1381 int
                                if jp1369 {
                                    jp1381 = 0
                                } else {
                                    var t1387 int = exponent__103 - fraction_digits__94
                                    jp1381 = t1387
                                }
                                var jp1383 int
                                if jp1369 {
                                    var t1385 int = fraction_digits__94 * 4
                                    var t1386 int = exponent__103 - t1385
                                    jp1383 = t1386
                                } else {
                                    jp1383 = 0
                                }
                                var t1384 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1381,
                                    binary_exponent: jp1383,
                                    hexadecimal: jp1369,
                                    significant_digits: significant_digits__95,
                                }
                                return t1384
                            }
                        }
                    }
                }
            }
        }
    }
}

func float_natural_power5(exponent__25 int) FloatNatural {
    var result__26 FloatNatural
    var inline2202 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline2202, 1)
    var inline2204 FloatNatural = FloatNatural{
        words: inline2202,
    }
    result__26 = inline2204
    var count__27 int = 0
    Loop_loop1552:
    for {
        var t1553 bool = count__27 < exponent__25
        if t1553 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1554 int = compound_old46 + compound_value47
            count__27 = t1554
            continue
        } else {
            break Loop_loop1552
        }
    }
    return result__26
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1641 bool
    var inline2206 *_goml_vec_uint32 = numerator__65.words
    var inline2207 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2206)
    t1641 = inline2207
    if t1641 {
        var t1642 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1642
    } else {
        var t1638 bool = binary_shift__67 >= 0
        var jp1563 FloatNatural
        if t1638 {
            var t1639 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1563 = t1639
        } else {
            var t1640 FloatNatural = float_natural_copy(numerator__65)
            jp1563 = t1640
        }
        var t1634 bool = binary_shift__67 >= 0
        var jp1565 FloatNatural
        if t1634 {
            var t1635 FloatNatural = float_natural_copy(denominator__66)
            jp1565 = t1635
        } else {
            var t1636 int = 0 - binary_shift__67
            var t1637 FloatNatural = float_natural_shift_left(denominator__66, t1636)
            jp1565 = t1637
        }
        var t1566 int = float_natural_bit_length(jp1563)
        var t1567 int = float_natural_bit_length(jp1565)
        var exponent__72 int = t1566 - t1567
        var t1628 bool = exponent__72 >= 0
        var jp1569 int
        if t1628 {
            var t1629 FloatNatural = float_natural_shift_left(jp1565, exponent__72)
            var t1630 int = float_natural_compare(jp1563, t1629)
            jp1569 = t1630
        } else {
            var t1631 int = 0 - exponent__72
            var t1632 FloatNatural = float_natural_shift_left(jp1563, t1631)
            var t1633 int = float_natural_compare(t1632, jp1565)
            jp1569 = t1633
        }
        var t1625 bool = jp1569 < 0
        if t1625 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1626 int = compound_old120 - compound_value121
            exponent__72 = t1626
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1619 bool = exponent__72 > exponent_bias__69
        if t1619 {
            var t1620 int = exponent_bias__69 + exponent_bias__69
            var t1621 int = t1620 + 1
            var t1622 uint64 = uint64(int(t1621))
            var t1623 uint64 = t1622 << mantissa_bits__68
            var t1624 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1623,
                _1: true,
            }
            return t1624
        } else {
            var t1614 bool = exponent__72 < minimum_exponent__74
            var jp1573 uint64
            if t1614 {
                var t1615 int = mantissa_bits__68 - minimum_exponent__74
                var t1616 uint64 = float_rational_quotient(jp1563, jp1565, t1615)
                jp1573 = t1616
            } else {
                var t1617 int = mantissa_bits__68 - exponent__72
                var t1618 uint64 = float_rational_quotient(jp1563, jp1565, t1617)
                jp1573 = t1618
            }
            var mantissa__76 uint64 = jp1573
            var t1576 bool = exponent__72 < minimum_exponent__74
            if t1576 {
                var t1579 bool = mantissa__76 == 0
                if t1579 {
                    var t1580 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1580
                } else {
                    var t1583_lhs uint64 = 1
                    var t1583 uint64 = t1583_lhs << mantissa_bits__68
                    var t1584 bool = mantissa__76 >= t1583
                    if t1584 {
                        var t1585_lhs uint64 = 1
                        var t1585 uint64 = t1585_lhs << mantissa_bits__68
                        var t1586_lhs uint64 = 1
                        var t1586 uint64 = t1586_lhs << mantissa_bits__68
                        var t1587 uint64 = mantissa__76 - t1586
                        var t1588 uint64 = t1585 | t1587
                        var t1589 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1588,
                            _1: false,
                        }
                        return t1589
                    } else {
                        var t1590 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1590
                    }
                }
            } else {
                var t1607 int = mantissa_bits__68 + 1
                var t1608_lhs uint64 = 1
                var t1608 uint64 = t1608_lhs << t1607
                var t1609 bool = mantissa__76 >= t1608
                if t1609 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1610 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1610
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1612 int = compound_old128 + compound_value129
                    exponent__72 = t1612
                } else {}
                var t1594 bool = exponent__72 > exponent_bias__69
                if t1594 {
                    var t1595 int = exponent_bias__69 + exponent_bias__69
                    var t1596 int = t1595 + 1
                    var t1597 uint64 = uint64(int(t1596))
                    var t1598 uint64 = t1597 << mantissa_bits__68
                    var t1599 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1598,
                        _1: true,
                    }
                    return t1599
                } else {
                    var t1600 int = exponent__72 + exponent_bias__69
                    var t1601 uint64 = uint64(int(t1600))
                    var t1602 uint64 = t1601 << mantissa_bits__68
                    var t1603_lhs uint64 = 1
                    var t1603 uint64 = t1603_lhs << mantissa_bits__68
                    var t1604 uint64 = mantissa__76 - t1603
                    var t1605 uint64 = t1602 | t1604
                    var t1606 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1605,
                        _1: false,
                    }
                    return t1606
                }
            }
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1647 int = vec_len__Vec_6uint32(self__528)
    var t1648 bool = t1647 == 0
    return t1648
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1651:
    for {
        var t1659 *_goml_vec_uint32 = value__7.words
        var t1660 bool
        var inline2218 int = vec_len__Vec_6uint32(t1659)
        var inline2219 bool = inline2218 == 0
        t1660 = inline2219
        var t1661 bool = !t1660
        var jp1653 bool
        if t1661 {
            var t1662 *_goml_vec_uint32 = value__7.words
            var t1663 *_goml_vec_uint32 = value__7.words
            var t1664 int
            var inline2212 int = vec_len__Vec_6uint32(t1663)
            t1664 = inline2212
            var t1665 int = t1664 - 1
            var t1666 uint32 = vec_get__Vec_6uint32(t1662, t1665)
            var t1667 bool = t1666 == 0
            jp1653 = t1667
        } else {
            jp1653 = false
        }
        if jp1653 {
            var t1654 *_goml_vec_uint32 = value__7.words
            var t1655 *_goml_vec_uint32 = value__7.words
            var t1656 int
            var inline2216 int = vec_len__Vec_6uint32(t1655)
            t1656 = inline2216
            var t1657 int = t1656 - 1
            vec_truncate__Vec_6uint32(t1654, t1657)
            continue
        } else {
            break Loop_loop1651
        }
    }
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1676 bool = string_is_char_boundary(value__274, start__275)
    var jp1673 bool
    if t1676 {
        var t1677 bool = string_is_char_boundary(value__274, end__276)
        jp1673 = t1677
    } else {
        jp1673 = false
    }
    if jp1673 {
        var t1674 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1674
    } else {
        var t1675 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1675
    }
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1692 int
    var inline2236 int = _goml_runtime_core_string_len(value__78)
    t1692 = inline2236
    var t1693 int
    var inline2234 int = _goml_runtime_core_string_len(expected__79)
    t1693 = inline2234
    var t1694 bool = t1692 != t1693
    if t1694 {
        return false
    } else {
        var index__80 int = 0
        var inline2226 uint8 = 97 - 65
        Loop_loop1682:
        for {
            var t1683 int
            var inline2232 int = _goml_runtime_core_string_len(value__78)
            t1683 = inline2232
            var t1684 bool = index__80 < t1683
            if t1684 {
                var t1688 uint8
                var inline2230 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1688 = inline2230
                var t1689 uint8
                var inline2223 bool = t1688 >= 65
                var inline2225 bool
                if inline2223 {
                    var inline2228 bool = t1688 <= 90
                    inline2225 = inline2228
                } else {
                    inline2225 = false
                }
                if inline2225 {
                    var inline2227 uint8 = t1688 + inline2226
                    t1689 = inline2227
                    var t1690 uint8
                    var inline2221 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1690 = inline2221
                    var t1691 bool = t1689 != t1690
                    if t1691 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1686 int = compound_old134 + compound_value135
                        index__80 = t1686
                        continue
                    }
                } else {
                    t1689 = t1688
                    var t1690 uint8
                    var inline2221 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1690 = inline2221
                    var t1691 bool = t1689 != t1690
                    if t1691 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1686 int = compound_old134 + compound_value135
                        index__80 = t1686
                        continue
                    }
                }
            } else {
                break Loop_loop1682
            }
        }
        return true
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1703 bool = value__77 >= 65
    var jp1700 bool
    if t1703 {
        var t1704 bool = value__77 <= 90
        jp1700 = t1704
    } else {
        jp1700 = false
    }
    if jp1700 {
        var t1701 uint8 = 97 - 65
        var t1702 uint8 = value__77 + t1701
        return t1702
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1731 bool = value__81 >= 48
    var jp1715 bool
    if t1731 {
        var t1732 bool = value__81 <= 57
        jp1715 = t1732
    } else {
        jp1715 = false
    }
    var jp1708 int
    if jp1715 {
        var t1716 uint8 = value__81 - 48
        var t1717 int = int(uint8(t1716))
        jp1708 = t1717
        var t1711 bool = jp1708 < base__82
        if t1711 {
            var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1708,
            }
            return t1712
        } else {
            var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1713
        }
    } else {
        var t1727 uint8
        var inline2252 bool = value__81 >= 65
        var inline2254 bool
        if inline2252 {
            var inline2257 bool = value__81 <= 90
            inline2254 = inline2257
        } else {
            inline2254 = false
        }
        if inline2254 {
            var inline2255 uint8 = 97 - 65
            var inline2256 uint8 = value__81 + inline2255
            t1727 = inline2256
            var t1728 bool = t1727 >= 97
            var jp1721 bool
            if t1728 {
                var t1729 uint8
                var inline2238 bool = value__81 >= 65
                var inline2240 bool
                if inline2238 {
                    var inline2243 bool = value__81 <= 90
                    inline2240 = inline2243
                } else {
                    inline2240 = false
                }
                if inline2240 {
                    var inline2241 uint8 = 97 - 65
                    var inline2242 uint8 = value__81 + inline2241
                    t1729 = inline2242
                    var t1730 bool = t1729 <= 102
                    jp1721 = t1730
                    if jp1721 {
                        var t1722 uint8
                        var inline2245 bool = value__81 >= 65
                        var inline2247 bool
                        if inline2245 {
                            var inline2250 bool = value__81 <= 90
                            inline2247 = inline2250
                        } else {
                            inline2247 = false
                        }
                        if inline2247 {
                            var inline2248 uint8 = 97 - 65
                            var inline2249 uint8 = value__81 + inline2248
                            t1722 = inline2249
                            var t1723 uint8 = t1722 - 97
                            var t1724 uint8 = t1723 + 10
                            var t1725 int = int(uint8(t1724))
                            jp1708 = t1725
                            var t1711 bool = jp1708 < base__82
                            if t1711 {
                                var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1708,
                                }
                                return t1712
                            } else {
                                var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1713
                            }
                        } else {
                            t1722 = value__81
                            var t1723 uint8 = t1722 - 97
                            var t1724 uint8 = t1723 + 10
                            var t1725 int = int(uint8(t1724))
                            jp1708 = t1725
                            var t1711 bool = jp1708 < base__82
                            if t1711 {
                                var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1708,
                                }
                                return t1712
                            } else {
                                var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1713
                            }
                        }
                    } else {
                        var t1726 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1726
                    }
                } else {
                    t1729 = value__81
                    var t1730 bool = t1729 <= 102
                    jp1721 = t1730
                    if jp1721 {
                        var t1722 uint8
                        var inline2245 bool = value__81 >= 65
                        var inline2247 bool
                        if inline2245 {
                            var inline2250 bool = value__81 <= 90
                            inline2247 = inline2250
                        } else {
                            inline2247 = false
                        }
                        if inline2247 {
                            var inline2248 uint8 = 97 - 65
                            var inline2249 uint8 = value__81 + inline2248
                            t1722 = inline2249
                            var t1723 uint8 = t1722 - 97
                            var t1724 uint8 = t1723 + 10
                            var t1725 int = int(uint8(t1724))
                            jp1708 = t1725
                            var t1711 bool = jp1708 < base__82
                            if t1711 {
                                var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1708,
                                }
                                return t1712
                            } else {
                                var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1713
                            }
                        } else {
                            t1722 = value__81
                            var t1723 uint8 = t1722 - 97
                            var t1724 uint8 = t1723 + 10
                            var t1725 int = int(uint8(t1724))
                            jp1708 = t1725
                            var t1711 bool = jp1708 < base__82
                            if t1711 {
                                var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1708,
                                }
                                return t1712
                            } else {
                                var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1713
                            }
                        }
                    } else {
                        var t1726 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1726
                    }
                }
            } else {
                jp1721 = false
                if jp1721 {
                    var t1722 uint8
                    var inline2245 bool = value__81 >= 65
                    var inline2247 bool
                    if inline2245 {
                        var inline2250 bool = value__81 <= 90
                        inline2247 = inline2250
                    } else {
                        inline2247 = false
                    }
                    if inline2247 {
                        var inline2248 uint8 = 97 - 65
                        var inline2249 uint8 = value__81 + inline2248
                        t1722 = inline2249
                        var t1723 uint8 = t1722 - 97
                        var t1724 uint8 = t1723 + 10
                        var t1725 int = int(uint8(t1724))
                        jp1708 = t1725
                        var t1711 bool = jp1708 < base__82
                        if t1711 {
                            var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1708,
                            }
                            return t1712
                        } else {
                            var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1713
                        }
                    } else {
                        t1722 = value__81
                        var t1723 uint8 = t1722 - 97
                        var t1724 uint8 = t1723 + 10
                        var t1725 int = int(uint8(t1724))
                        jp1708 = t1725
                        var t1711 bool = jp1708 < base__82
                        if t1711 {
                            var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1708,
                            }
                            return t1712
                        } else {
                            var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1713
                        }
                    }
                } else {
                    var t1726 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1726
                }
            }
        } else {
            t1727 = value__81
            var t1728 bool = t1727 >= 97
            var jp1721 bool
            if t1728 {
                var t1729 uint8
                var inline2238 bool = value__81 >= 65
                var inline2240 bool
                if inline2238 {
                    var inline2243 bool = value__81 <= 90
                    inline2240 = inline2243
                } else {
                    inline2240 = false
                }
                if inline2240 {
                    var inline2241 uint8 = 97 - 65
                    var inline2242 uint8 = value__81 + inline2241
                    t1729 = inline2242
                    var t1730 bool = t1729 <= 102
                    jp1721 = t1730
                    if jp1721 {
                        var t1722 uint8
                        var inline2245 bool = value__81 >= 65
                        var inline2247 bool
                        if inline2245 {
                            var inline2250 bool = value__81 <= 90
                            inline2247 = inline2250
                        } else {
                            inline2247 = false
                        }
                        if inline2247 {
                            var inline2248 uint8 = 97 - 65
                            var inline2249 uint8 = value__81 + inline2248
                            t1722 = inline2249
                            var t1723 uint8 = t1722 - 97
                            var t1724 uint8 = t1723 + 10
                            var t1725 int = int(uint8(t1724))
                            jp1708 = t1725
                            var t1711 bool = jp1708 < base__82
                            if t1711 {
                                var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1708,
                                }
                                return t1712
                            } else {
                                var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1713
                            }
                        } else {
                            t1722 = value__81
                            var t1723 uint8 = t1722 - 97
                            var t1724 uint8 = t1723 + 10
                            var t1725 int = int(uint8(t1724))
                            jp1708 = t1725
                            var t1711 bool = jp1708 < base__82
                            if t1711 {
                                var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1708,
                                }
                                return t1712
                            } else {
                                var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1713
                            }
                        }
                    } else {
                        var t1726 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1726
                    }
                } else {
                    t1729 = value__81
                    var t1730 bool = t1729 <= 102
                    jp1721 = t1730
                    if jp1721 {
                        var t1722 uint8
                        var inline2245 bool = value__81 >= 65
                        var inline2247 bool
                        if inline2245 {
                            var inline2250 bool = value__81 <= 90
                            inline2247 = inline2250
                        } else {
                            inline2247 = false
                        }
                        if inline2247 {
                            var inline2248 uint8 = 97 - 65
                            var inline2249 uint8 = value__81 + inline2248
                            t1722 = inline2249
                            var t1723 uint8 = t1722 - 97
                            var t1724 uint8 = t1723 + 10
                            var t1725 int = int(uint8(t1724))
                            jp1708 = t1725
                            var t1711 bool = jp1708 < base__82
                            if t1711 {
                                var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1708,
                                }
                                return t1712
                            } else {
                                var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1713
                            }
                        } else {
                            t1722 = value__81
                            var t1723 uint8 = t1722 - 97
                            var t1724 uint8 = t1723 + 10
                            var t1725 int = int(uint8(t1724))
                            jp1708 = t1725
                            var t1711 bool = jp1708 < base__82
                            if t1711 {
                                var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1708,
                                }
                                return t1712
                            } else {
                                var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1713
                            }
                        }
                    } else {
                        var t1726 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1726
                    }
                }
            } else {
                jp1721 = false
                if jp1721 {
                    var t1722 uint8
                    var inline2245 bool = value__81 >= 65
                    var inline2247 bool
                    if inline2245 {
                        var inline2250 bool = value__81 <= 90
                        inline2247 = inline2250
                    } else {
                        inline2247 = false
                    }
                    if inline2247 {
                        var inline2248 uint8 = 97 - 65
                        var inline2249 uint8 = value__81 + inline2248
                        t1722 = inline2249
                        var t1723 uint8 = t1722 - 97
                        var t1724 uint8 = t1723 + 10
                        var t1725 int = int(uint8(t1724))
                        jp1708 = t1725
                        var t1711 bool = jp1708 < base__82
                        if t1711 {
                            var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1708,
                            }
                            return t1712
                        } else {
                            var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1713
                        }
                    } else {
                        t1722 = value__81
                        var t1723 uint8 = t1722 - 97
                        var t1724 uint8 = t1723 + 10
                        var t1725 int = int(uint8(t1724))
                        jp1708 = t1725
                        var t1711 bool = jp1708 < base__82
                        if t1711 {
                            var t1712 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1708,
                            }
                            return t1712
                        } else {
                            var t1713 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1713
                        }
                    }
                } else {
                    var t1726 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1726
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1735:
    for {
        var t1736 bool = carry__22 != 0
        if t1736 {
            var t1745 *_goml_vec_uint32 = value__20.words
            var t1746 int
            var inline2262 int = vec_len__Vec_6uint32(t1745)
            t1746 = inline2262
            var t1747 bool = index__23 == t1746
            if t1747 {
                var t1748 *_goml_vec_uint32 = value__20.words
                var inline2259 uint32 = 0
                vec_push__Vec_6uint32(t1748, inline2259)
            } else {}
            var t1738 *_goml_vec_uint32 = value__20.words
            var t1739 uint32 = vec_get__Vec_6uint32(t1738, index__23)
            var t1740 uint64 = uint64(uint32(t1739))
            var sum__24 uint64 = t1740 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1742_rhs int = 32
            var t1742 uint64 = sum__24 >> t1742_rhs
            carry__22 = t1742
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1743 int = compound_old42 + compound_value43
            index__23 = t1743
            continue
        } else {
            break Loop_loop1735
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1752 FloatNatural
    var inline2264 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2265 FloatNatural = FloatNatural{
        words: inline2264,
    }
    t1752 = inline2265
    var t1753 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1752,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1753
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1773 *_goml_vec_uint32 = value__9.words
    var t1774 bool
    var inline2271 int = vec_len__Vec_6uint32(t1773)
    var inline2272 bool = inline2271 == 0
    t1774 = inline2272
    if t1774 {
        return 0
    } else {
        var t1757 *_goml_vec_uint32 = value__9.words
        var t1758 *_goml_vec_uint32 = value__9.words
        var t1759 int
        var inline2269 int = vec_len__Vec_6uint32(t1758)
        t1759 = inline2269
        var t1760 int = t1759 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1757, t1760)
        var bits__11 int = 0
        Loop_loop1767:
        for {
            var t1768 bool = high__10 != 0
            if t1768 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1769 uint32 = compound_old9 >> compound_value10
                high__10 = t1769
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1771 int = compound_old12 + compound_value13
                bits__11 = t1771
                continue
            } else {
                break Loop_loop1767
            }
        }
        var t1762 *_goml_vec_uint32 = value__9.words
        var t1763 int
        var inline2267 int = vec_len__Vec_6uint32(t1762)
        t1763 = inline2267
        var t1764 int = t1763 - 1
        var t1765 int = t1764 * 32
        var t1766 int = t1765 + bits__11
        return t1766
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1796 *_goml_vec_uint32 = left__12.words
    var t1797 int
    var inline2282 int = vec_len__Vec_6uint32(t1796)
    t1797 = inline2282
    var t1798 *_goml_vec_uint32 = right__13.words
    var t1799 int
    var inline2280 int = vec_len__Vec_6uint32(t1798)
    t1799 = inline2280
    var t1800 bool = t1797 < t1799
    if t1800 {
        return -1
    } else {
        var t1802 *_goml_vec_uint32 = left__12.words
        var t1803 int
        var inline2276 int = vec_len__Vec_6uint32(t1802)
        t1803 = inline2276
        var t1804 *_goml_vec_uint32 = right__13.words
        var t1805 int
        var inline2274 int = vec_len__Vec_6uint32(t1804)
        t1805 = inline2274
        var t1806 bool = t1803 > t1805
        if t1806 {
            return 1
        } else {
            var t1778 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2278 int = vec_len__Vec_6uint32(t1778)
            index__14 = inline2278
            Loop_loop1780:
            for {
                var t1781 bool = index__14 > 0
                if t1781 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1782 int = compound_old17 - compound_value18
                    index__14 = t1782
                    var t1785 *_goml_vec_uint32 = left__12.words
                    var t1786 uint32 = vec_get__Vec_6uint32(t1785, index__14)
                    var t1787 *_goml_vec_uint32 = right__13.words
                    var t1788 uint32 = vec_get__Vec_6uint32(t1787, index__14)
                    var t1789 bool = t1786 < t1788
                    if t1789 {
                        return -1
                    } else {
                        var t1791 *_goml_vec_uint32 = left__12.words
                        var t1792 uint32 = vec_get__Vec_6uint32(t1791, index__14)
                        var t1793 *_goml_vec_uint32 = right__13.words
                        var t1794 uint32 = vec_get__Vec_6uint32(t1793, index__14)
                        var t1795 bool = t1792 > t1794
                        if t1795 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1780
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1842 bool = shift__57 >= 0
    var jp1810 FloatNatural
    if t1842 {
        var t1843 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1810 = t1843
    } else {
        var t1844 FloatNatural = float_natural_copy(numerator__55)
        jp1810 = t1844
    }
    var t1838 bool = shift__57 >= 0
    var jp1812 FloatNatural
    if t1838 {
        var t1839 FloatNatural = float_natural_copy(denominator__56)
        jp1812 = t1839
    } else {
        var t1840 int = 0 - shift__57
        var t1841 FloatNatural = float_natural_shift_left(denominator__56, t1840)
        jp1812 = t1841
    }
    var quotient__60 uint64 = 0
    Loop_loop1825:
    for {
        var t1826 int = float_natural_compare(jp1810, jp1812)
        var t1827 bool = t1826 >= 0
        if t1827 {
            var t1828 int = float_natural_bit_length(jp1810)
            var t1829 int = float_natural_bit_length(jp1812)
            var offset__61 int = t1828 - t1829
            var part__62 FloatNatural = float_natural_shift_left(jp1812, offset__61)
            var t1833 int = float_natural_compare(jp1810, part__62)
            var t1834 bool = t1833 < 0
            if t1834 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1835 int = compound_old105 - compound_value106
                offset__61 = t1835
                var t1837 FloatNatural = float_natural_shift_left(jp1812, offset__61)
                part__62 = t1837
            } else {}
            float_natural_subtract(jp1810, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1831 uint64 = compound_old111 | compound_value112
            quotient__60 = t1831
            continue
        } else {
            break Loop_loop1825
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1810, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1812)
    var t1819 bool = rounding__64 > 0
    var jp1816 bool
    if t1819 {
        jp1816 = true
    } else {
        var t1822 bool = rounding__64 == 0
        if t1822 {
            var t1823_rhs uint64 = 1
            var t1823 uint64 = quotient__60 & t1823_rhs
            var t1824 bool = t1823 == 1
            jp1816 = t1824
        } else {
            jp1816 = false
        }
    }
    if jp1816 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1817 uint64 = compound_old115 + compound_value116
        quotient__60 = t1817
    } else {}
    return quotient__60
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(self__531 *_goml_vec_uint32, len__532 int) struct{} {
    vec_truncate__Vec_6uint32(self__531, len__532)
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1860 bool = index__269 < 0
    var jp1852 bool
    if t1860 {
        jp1852 = true
    } else {
        var t1861 int
        var inline2284 int = _goml_runtime_core_string_len(value__268)
        t1861 = inline2284
        var t1862 bool = index__269 > t1861
        jp1852 = t1862
    }
    if jp1852 {
        return false
    } else {
        var t1855 int
        var inline2288 int = _goml_runtime_core_string_len(value__268)
        t1855 = inline2288
        var t1856 bool = index__269 == t1855
        if t1856 {
            return true
        } else {
            var t1857 uint8
            var inline2286 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1857 = inline2286
            var t1858_rhs uint8 = 192
            var t1858 uint8 = t1857 & t1858_rhs
            var t1859 bool = t1858 != 128
            return t1859
        }
    }
}

func float_natural_subtract(value__37 FloatNatural, other__38 FloatNatural) struct{} {
    var base__39 uint64 = 4294967296
    var borrow__40 uint64 = 0
    var index__41 int = 0
    Loop_loop1866:
    for {
        var t1867 *_goml_vec_uint32 = value__37.words
        var t1868 int
        var inline2292 int = vec_len__Vec_6uint32(t1867)
        t1868 = inline2292
        var t1869 bool = index__41 < t1868
        if t1869 {
            var t1883 *_goml_vec_uint32 = other__38.words
            var t1884 int
            var inline2290 int = vec_len__Vec_6uint32(t1883)
            t1884 = inline2290
            var t1885 bool = index__41 < t1884
            var jp1871 uint64
            if t1885 {
                var t1886 *_goml_vec_uint32 = other__38.words
                var t1887 uint32 = vec_get__Vec_6uint32(t1886, index__41)
                var t1888 uint64 = uint64(uint32(t1887))
                jp1871 = t1888
            } else {
                jp1871 = 0
            }
            var right__42 uint64 = jp1871 + borrow__40
            var t1872 *_goml_vec_uint32 = value__37.words
            var t1873 uint32 = vec_get__Vec_6uint32(t1872, index__41)
            var left__43 uint64 = uint64(uint32(t1873))
            var t1877 bool = left__43 >= right__42
            if t1877 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1878 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1878))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1880 uint64 = base__39 + left__43
                var t1881 uint64 = t1880 - right__42
                var value75 uint32 = uint32(uint64(t1881))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1875 int = compound_old79 + compound_value80
            index__41 = t1875
            continue
        } else {
            break Loop_loop1866
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func main() {
    main0()
}
