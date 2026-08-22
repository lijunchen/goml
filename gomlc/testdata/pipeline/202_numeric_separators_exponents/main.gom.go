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

func _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(arg0 float64) uint64 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float64bits(arg0)
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
    var integer__1 int32 = 1000
    var unsigned__2 uint64 = 4294967296
    var float__3 float64 = 125
    var small__4 float32 = 2.5
    var inline1875 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(integer__1)
    _goml_runtime_core_string_println(inline1875)
    var inline1872 string = _goml_m_trait__impl_i_ToString_i_u64_i_to__string(unsigned__2)
    _goml_runtime_core_string_println(inline1872)
    var inline1869 string = _goml_m_trait__impl_i_ToString_i_f64_i_to__string(float__3)
    _goml_runtime_core_string_println(inline1869)
    var inline1866 string = _goml_m_trait__impl_i_ToString_i_f32_i_to__string(small__4)
    _goml_runtime_core_string_println(inline1866)
    switch integer__1 {
    case 1000:
        var inline1858 string = "matched"
        var inline1859 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1858)
        _goml_runtime_core_string_println(inline1859)
        return struct{}{}
    default:
        var inline1862 string = "missed"
        var inline1863 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1862)
        _goml_runtime_core_string_println(inline1863)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline1887 int64 = int64(int32(self__407))
    var inline1888 string = signed_decimal_string(inline1887)
    return inline1888
}

func _goml_m_trait__impl_i_ToString_i_u64_i_to__string(self__412 uint64) string {
    var inline1890 string = decimal_string(self__412)
    return inline1890
}

func _goml_m_trait__impl_i_ToString_i_f64_i_to__string(self__414 float64) string {
    var inline1892 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(self__414)
    var inline1893 string = format_float_bits(inline1892, 52, 11, 1023)
    return inline1893
}

func _goml_m_trait__impl_i_ToString_i_f32_i_to__string(self__413 float32) string {
    var inline1895 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(self__413)
    var inline1896 uint64 = uint64(uint32(inline1895))
    var inline1897 string = format_float_bits(inline1896, 23, 8, 127)
    return inline1897
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t855 bool = value__214 < 0
    if t855 {
        var t856 uint64 = uint64(int64(value__214))
        var t857 uint64 = 0 - t856
        var t858 string = decimal_string(t857)
        var t859 string = "-" + t858
        return t859
    } else {
        var t860 uint64 = uint64(int64(value__214))
        var t861 string = decimal_string(t860)
        return t861
    }
}

func decimal_string(value__208 uint64) string {
    var t884 bool = value__208 == 0
    if t884 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop877:
        for {
            var t878 bool = remaining__210 > 0
            if t878 {
                var t879_rhs uint64 = 10
                var t879 uint64 = remaining__210 % t879_rhs
                var t880 uint8 = uint8(uint64(t879))
                var t881 uint8 = t880 + 48
                vec_push__Vec_5uint8(reversed__209, t881)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t882 uint64 = compound_old353 / compound_value354
                remaining__210 = t882
                continue
            } else {
                break Loop_loop877
            }
        }
        var t866 int
        var inline1915 int = vec_len__Vec_5uint8(reversed__209)
        t866 = inline1915
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t866)
        var offset__212 int = 0
        Loop_loop868:
        for {
            var t869 int
            var inline1913 int = vec_len__Vec_5uint8(reversed__209)
            t869 = inline1913
            var t870 bool = offset__212 < t869
            if t870 {
                var t871 int
                var inline1911 int = vec_len__Vec_5uint8(reversed__209)
                t871 = inline1911
                var t872 int = t871 - offset__212
                var t873 int = t872 - 1
                var t874 uint8 = vec_get__Vec_5uint8(reversed__209, t873)
                vec_push__Vec_5uint8(bytes__211, t874)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t875 int = compound_old358 + compound_value359
                offset__212 = t875
                continue
            } else {
                break Loop_loop868
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t887 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t887
    var t888 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t888 != 0
    var t889_lhs uint64 = 1
    var t889 uint64 = t889_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t889 - 1
    var t890 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t890 & exponent_mask__166
    var t891_lhs uint64 = 1
    var t891 uint64 = t891_lhs << mantissa_bits__161
    var t892 uint64 = t891 - 1
    var fraction__168 uint64 = bits__160 & t892
    var t956 bool = exponent__167 == exponent_mask__166
    if t956 {
        var t958 bool = fraction__168 == 0
        if t958 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t964 bool = exponent__167 == 0
        var jp962 bool
        if t964 {
            var t965 bool = fraction__168 == 0
            jp962 = t965
        } else {
            jp962 = false
        }
        if jp962 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t953 bool = exponent__167 == 0
            var jp895 uint64
            if t953 {
                jp895 = fraction__168
            } else {
                var t954_lhs uint64 = 1
                var t954 uint64 = t954_lhs << mantissa_bits__161
                var t955 uint64 = fraction__168 | t954
                jp895 = t955
            }
            var t947 bool = exponent__167 == 0
            var jp897 int
            if t947 {
                var t948 int = 1 - exponent_bias__163
                var t949 int = t948 - mantissa_bits__161
                jp897 = t949
            } else {
                var t950 int = int(uint64(exponent__167))
                var t951 int = t950 - exponent_bias__163
                var t952 int = t951 - mantissa_bits__161
                jp897 = t952
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp895)
            var t902 bool = jp897 >= 0
            var jp899 int
            if t902 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp897)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t921 bool = mantissa_bits__161 == 23
                var jp904 int
                if t921 {
                    jp904 = 9
                } else {
                    jp904 = 17
                }
                var t918 int
                var inline1923 int = _goml_runtime_core_string_len(digits__173)
                t918 = inline1923
                var t919 bool = t918 < jp904
                var jp906 int
                if t919 {
                    var inline1917 int = _goml_runtime_core_string_len(digits__173)
                    jp906 = inline1917
                } else {
                    jp906 = jp904
                }
                var count__176 int = 1
                Loop_loop909:
                for {
                    var t910 bool = count__176 <= jp906
                    if t910 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t911 int
                        var inline1919 int = _goml_runtime_core_string_len(digits__173)
                        t911 = inline1919
                        var jp913 int
                        if x319 {
                            jp913 = 1
                        } else {
                            jp913 = 0
                        }
                        var point__180 int = t911 + jp913
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t917 bool = x322 == bits__160
                        if t917 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t915 int = compound_old324 + compound_value325
                            count__176 = t915
                            continue
                        }
                    } else {
                        break Loop_loop909
                    }
                }
                var inline1921 int = _goml_runtime_core_string_len(digits__173)
                jp899 = inline1921
                var t900 string = float_natural_decimal(exact_value__171)
                var t901 string = fixed_float_text(t900, jp899, negative__165)
                return t901
            } else {
                var count__183 int = 0
                var t943 int = 0 - jp897
                Loop_loop942:
                for {
                    var t944 bool = count__183 < t943
                    if t944 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t945 int = compound_old329 + compound_value330
                        count__183 = t945
                        continue
                    } else {
                        break Loop_loop942
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t923 int
                var inline1929 int = _goml_runtime_core_string_len(digits__184)
                t923 = inline1929
                var point__185 int = t923 + jp897
                var t941 bool = mantissa_bits__161 == 23
                var jp925 int
                if t941 {
                    jp925 = 9
                } else {
                    jp925 = 17
                }
                var t938 int
                var inline1927 int = _goml_runtime_core_string_len(digits__184)
                t938 = inline1927
                var t939 bool = t938 < jp925
                var jp927 int
                if t939 {
                    var inline1925 int = _goml_runtime_core_string_len(digits__184)
                    jp927 = inline1925
                } else {
                    jp927 = jp925
                }
                count__183 = 1
                Loop_loop929:
                for {
                    var t930 bool = count__183 <= jp927
                    if t930 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp932 int
                        if x336 {
                            jp932 = 1
                        } else {
                            jp932 = 0
                        }
                        var t933 int = point__185 + jp932
                        var candidate__191 string = fixed_float_text(rounded__190, t933, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t937 bool = x339 == bits__160
                        if t937 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t935 int = compound_old341 + compound_value342
                            count__183 = t935
                            continue
                        }
                    } else {
                        break Loop_loop929
                    }
                }
                jp899 = point__185
                var t900 string = float_natural_decimal(exact_value__171)
                var t901 string = fixed_float_text(t900, jp899, negative__165)
                return t901
            }
        }
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline1935 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline1936 FloatNatural = FloatNatural{
        words: inline1935,
    }
    result__2 = inline1936
    var t974 bool = value__1 != 0
    if t974 {
        var t975 *_goml_vec_uint32 = result__2.words
        var t976 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t975, t976)
        var t977_rhs int = 32
        var t977 uint64 = value__1 >> t977_rhs
        var high__3 uint32 = uint32(uint64(t977))
        var t979 bool = high__3 != 0
        if t979 {
            var t980 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t980, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t1009 bool
    var inline1953 *_goml_vec_uint32 = value__28.words
    var inline1954 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1953)
    t1009 = inline1954
    if t1009 {
        var inline1938 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline1939 FloatNatural = FloatNatural{
            words: inline1938,
        }
        return inline1939
    } else {
        var t1012 bool = bits__29 == 0
        if t1012 {
            var t1013 FloatNatural = float_natural_copy(value__28)
            return t1013
        } else {
            var result__30 FloatNatural
            var inline1950 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline1951 FloatNatural = FloatNatural{
                words: inline1950,
            }
            result__30 = inline1951
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop1004:
            for {
                var t1005 bool = index__33 < word_shift__31
                if t1005 {
                    var t1006 *_goml_vec_uint32 = result__30.words
                    var inline1941 uint32 = 0
                    vec_push__Vec_6uint32(t1006, inline1941)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t1007 int = compound_old52 + compound_value53
                    index__33 = t1007
                    continue
                } else {
                    break Loop_loop1004
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop992:
            for {
                var t993 *_goml_vec_uint32 = value__28.words
                var t994 int
                var inline1946 int = vec_len__Vec_6uint32(t993)
                t994 = inline1946
                var t995 bool = index__33 < t994
                if t995 {
                    var t996 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t996, index__33)
                    var t997 uint64 = uint64(uint32(word__35))
                    var t998 uint64 = t997 << bit_shift__32
                    var shifted__36 uint64 = t998 | carry__34
                    var t999 *_goml_vec_uint32 = result__30.words
                    var t1000 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t999, t1000)
                    var t1001_rhs int = 32
                    var t1001 uint64 = shifted__36 >> t1001_rhs
                    carry__34 = t1001
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t1002 int = compound_old59 + compound_value60
                    index__33 = t1002
                    continue
                } else {
                    break Loop_loop992
                }
            }
            var t988 bool = carry__34 != 0
            if t988 {
                var t989 *_goml_vec_uint32 = result__30.words
                var t990 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t989, t990)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t1036 bool
    var inline1969 *_goml_vec_uint32 = value__49.words
    var inline1970 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1969)
    t1036 = inline1970
    if t1036 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop1029:
        for {
            var t1030 bool
            var inline1958 *_goml_vec_uint32 = current__50.words
            var inline1959 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1958)
            t1030 = inline1959
            var t1031 bool = !t1030
            if t1031 {
                var t1032 uint32 = float_natural_divide_small(current__50, 10)
                var t1033 uint8 = uint8(uint32(t1032))
                var t1034 uint8 = t1033 + 48
                vec_push__Vec_5uint8(reversed__51, t1034)
                continue
            } else {
                break Loop_loop1029
            }
        }
        var t1018 int
        var inline1967 int = vec_len__Vec_5uint8(reversed__51)
        t1018 = inline1967
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1018)
        var offset__53 int = 0
        Loop_loop1020:
        for {
            var t1021 int
            var inline1965 int = vec_len__Vec_5uint8(reversed__51)
            t1021 = inline1965
            var t1022 bool = offset__53 < t1021
            if t1022 {
                var t1023 int
                var inline1963 int = vec_len__Vec_5uint8(reversed__51)
                t1023 = inline1963
                var t1024 int = t1023 - offset__53
                var t1025 int = t1024 - 1
                var t1026 uint8 = vec_get__Vec_5uint8(reversed__51, t1025)
                vec_push__Vec_5uint8(output__52, t1026)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t1027 int = compound_old98 + compound_value99
                offset__53 = t1027
                continue
            } else {
                break Loop_loop1020
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1039 int = _goml_runtime_core_string_len(self__289)
    return t1039
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t1042 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1042)
    var index__148 int = 0
    Loop_loop1097:
    for {
        var t1098 bool = index__148 < count__146
        if t1098 {
            var t1099 uint8
            var inline1974 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1099 = inline1974
            vec_push__Vec_5uint8(output__147, t1099)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1100 int = compound_old267 + compound_value268
            index__148 = t1100
            continue
        } else {
            break Loop_loop1097
        }
    }
    var t1094 int
    var inline1995 int = _goml_runtime_core_string_len(exact__145)
    t1094 = inline1995
    var t1095 bool = count__146 == t1094
    if t1095 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1096 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1096
    } else {
        var next__150 uint8
        var inline1993 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline1993
        var trailing__151 bool = false
        var t1045 int = count__146 + 1
        index__148 = t1045
        Loop_loop1086:
        for {
            var t1087 int
            var inline1978 int = _goml_runtime_core_string_len(exact__145)
            t1087 = inline1978
            var t1088 bool = index__148 < t1087
            if t1088 {
                var t1092 uint8
                var inline1976 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1092 = inline1976
                var t1093 bool = t1092 != 48
                if t1093 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1090 int = compound_old278 + compound_value279
                index__148 = t1090
                continue
            } else {
                break Loop_loop1086
            }
        }
        var t1074 bool = next__150 > 53
        var jp1048 bool
        if t1074 {
            jp1048 = true
        } else {
            var t1077 bool = next__150 == 53
            if t1077 {
                if trailing__151 {
                    jp1048 = true
                } else {
                    var t1080 int
                    var inline1980 int = vec_len__Vec_5uint8(output__147)
                    t1080 = inline1980
                    var t1081 int = t1080 - 1
                    var t1082 uint8 = vec_get__Vec_5uint8(output__147, t1081)
                    var t1083 uint8 = t1082 - 48
                    var t1084_rhs uint8 = 2
                    var t1084 uint8 = t1083 % t1084_rhs
                    var t1085 bool = t1084 == 1
                    jp1048 = t1085
                }
            } else {
                jp1048 = false
            }
        }
        if jp1048 {
            var index__153 int
            var inline1991 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline1991
            Loop_loop1062:
            for {
                var t1063 bool = index__153 > 0
                if t1063 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1064 int = compound_old282 - compound_value283
                    index__153 = t1064
                    var t1067 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1068 bool = t1067 < 57
                    if t1068 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1069 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1069)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1071 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1071
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1062
                }
            }
            var t1052 int
            var inline1989 int = vec_len__Vec_5uint8(output__147)
            t1052 = inline1989
            var t1053 int = t1052 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1053)
            var inline1986 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline1986)
            index__153 = 0
            Loop_loop1056:
            for {
                var t1057 int
                var inline1984 int = vec_len__Vec_5uint8(output__147)
                t1057 = inline1984
                var t1058 bool = index__153 < t1057
                if t1058 {
                    var t1059 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t1059)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t1060 int = compound_old302 + compound_value303
                    index__153 = t1060
                    continue
                } else {
                    break Loop_loop1056
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t1055 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t1055
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1073 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1073
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline2002 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline2002
    Loop_loop1106:
    for {
        var t1111 bool = length__159 > 1
        var jp1108 bool
        if t1111 {
            var t1112 int = length__159 - 1
            var t1113 uint8
            var inline1997 uint8 = _goml_runtime_core_string_byte_get(value__158, t1112)
            t1113 = inline1997
            var t1114 bool = t1113 == 48
            jp1108 = t1114
        } else {
            jp1108 = false
        }
        if jp1108 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1109 int = compound_old312 - compound_value313
            length__159 = t1109
            continue
        } else {
            break Loop_loop1106
        }
    }
    var inline1999 int = 0
    var inline2000 string = string_byte_slice(value__158, inline1999, length__159)
    return inline2000
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline2004 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline2004)
    } else {}
    var t1119 bool = decimal_point__138 <= 0
    if t1119 {
        var inline2019 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline2019)
        var inline2016 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline2016)
        var index__141 int = 0
        var t1129 int = 0 - decimal_point__138
        Loop_loop1128:
        for {
            var t1130 bool = index__141 < t1129
            if t1130 {
                var inline2007 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline2007)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1131 int = compound_old234 + compound_value235
                index__141 = t1131
                continue
            } else {
                break Loop_loop1128
            }
        }
        index__141 = 0
        Loop_loop1122:
        for {
            var t1123 int
            var inline2014 int = _goml_runtime_core_string_len(digits__137)
            t1123 = inline2014
            var t1124 bool = index__141 < t1123
            if t1124 {
                var t1125 uint8
                var inline2012 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1125 = inline2012
                vec_push__Vec_5uint8(bytes__140, t1125)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1126 int = compound_old240 + compound_value241
                index__141 = t1126
                continue
            } else {
                break Loop_loop1122
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1134 int
        var inline2044 int = _goml_runtime_core_string_len(digits__137)
        t1134 = inline2044
        var t1135 bool = decimal_point__138 >= t1134
        if t1135 {
            var index__142 int = 0
            Loop_loop1142:
            for {
                var t1143 int
                var inline2026 int = _goml_runtime_core_string_len(digits__137)
                t1143 = inline2026
                var t1144 bool = index__142 < t1143
                if t1144 {
                    var t1145 uint8
                    var inline2024 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1145 = inline2024
                    vec_push__Vec_5uint8(bytes__140, t1145)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1146 int = compound_old244 + compound_value245
                    index__142 = t1146
                    continue
                } else {
                    break Loop_loop1142
                }
            }
            Loop_loop1138:
            for {
                var t1139 bool = index__142 < decimal_point__138
                if t1139 {
                    var inline2028 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline2028)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1140 int = compound_old249 + compound_value250
                    index__142 = t1140
                    continue
                } else {
                    break Loop_loop1138
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1156:
            for {
                var t1157 bool = index__143 < decimal_point__138
                if t1157 {
                    var t1158 uint8
                    var inline2033 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1158 = inline2033
                    vec_push__Vec_5uint8(bytes__140, t1158)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1159 int = compound_old253 + compound_value254
                    index__143 = t1159
                    continue
                } else {
                    break Loop_loop1156
                }
            }
            var inline2041 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline2041)
            Loop_loop1150:
            for {
                var t1151 int
                var inline2039 int = _goml_runtime_core_string_len(digits__137)
                t1151 = inline2039
                var t1152 bool = index__143 < t1151
                if t1152 {
                    var t1153 uint8
                    var inline2037 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1153 = inline2037
                    vec_push__Vec_5uint8(bytes__140, t1153)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1154 int = compound_old259 + compound_value260
                    index__143 = t1154
                    continue
                } else {
                    break Loop_loop1150
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
    var t1255 bool = parsed__110.valid
    var t1256 bool = !t1255
    if t1256 {
        var t1257 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t1257
    } else {
        var t1249 bool = parsed__110.negative
        var jp1166 uint64
        if t1249 {
            var t1254 bool = mantissa_bits__108 == 23
            var jp1251 int
            if t1254 {
                jp1251 = 8
            } else {
                jp1251 = 11
            }
            var t1252 int = mantissa_bits__108 + jp1251
            var t1253_lhs uint64 = 1
            var t1253 uint64 = t1253_lhs << t1252
            jp1166 = t1253
        } else {
            jp1166 = 0
        }
        var t1248 bool = mantissa_bits__108 == 23
        var jp1168 int
        if t1248 {
            jp1168 = 8
        } else {
            jp1168 = 11
        }
        var t1169_lhs uint64 = 1
        var t1169 uint64 = t1169_lhs << jp1168
        var t1170 uint64 = t1169 - 1
        var exponent_mask__112 uint64 = t1170 << mantissa_bits__108
        var t1226 int = parsed__110.special
        var t1227 bool = t1226 == 1
        if t1227 {
            var t1228 uint64 = jp1166 | exponent_mask__112
            var t1229 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t1228,
            }
            return t1229
        } else {
            var t1231 int = parsed__110.special
            var t1232 bool = t1231 == 2
            if t1232 {
                var t1236 int = mantissa_bits__108 - 1
                var t1237_lhs uint64 = 1
                var t1237 uint64 = t1237_lhs << t1236
                var t1238 uint64 = exponent_mask__112 | t1237
                var t1243 bool = mantissa_bits__108 == 52
                var jp1240 uint64
                if t1243 {
                    jp1240 = 1
                } else {
                    jp1240 = 0
                }
                var t1241 uint64 = t1238 | jp1240
                var t1242 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t1241,
                }
                return t1242
            } else {
                var t1245 FloatNatural = parsed__110.numerator
                var t1246 bool
                var inline2046 *_goml_vec_uint32 = t1245.words
                var inline2047 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2046)
                t1246 = inline2047
                if t1246 {
                    var t1247 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp1166,
                    }
                    return t1247
                } else {
                    var t1209 bool = parsed__110.hexadecimal
                    var t1210 bool = !t1209
                    if t1210 {
                        var t1211 int = parsed__110.significant_digits
                        var t1212 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t1211 + t1212
                        var t1225 bool = mantissa_bits__108 == 23
                        var jp1214 int
                        if t1225 {
                            jp1214 = 40
                        } else {
                            jp1214 = 310
                        }
                        var t1224 bool = mantissa_bits__108 == 23
                        var jp1216 int
                        if t1224 {
                            jp1216 = -46
                        } else {
                            jp1216 = -325
                        }
                        var t1218 bool = decimal_position__113 > jp1214
                        if t1218 {
                            var t1219 uint64 = jp1166 | exponent_mask__112
                            var t1220 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t1219,
                            }
                            return t1220
                        } else {
                            var t1222 bool = decimal_position__113 < jp1216
                            if t1222 {
                                var t1223 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp1166,
                                }
                                return t1223
                            } else {
                                var t1205 bool = parsed__110.hexadecimal
                                var t1206 bool = !t1205
                                var jp1200 bool
                                if t1206 {
                                    var t1207 int = parsed__110.decimal_exponent
                                    var t1208 bool = t1207 < 0
                                    jp1200 = t1208
                                } else {
                                    jp1200 = false
                                }
                                var jp1174 FloatNatural
                                if jp1200 {
                                    var t1201 int = parsed__110.decimal_exponent
                                    var t1202 int = 0 - t1201
                                    var t1203 FloatNatural = float_natural_power5(t1202)
                                    jp1174 = t1203
                                } else {
                                    var inline2049 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline2049, 1)
                                    var inline2051 FloatNatural = FloatNatural{
                                        words: inline2049,
                                    }
                                    jp1174 = inline2051
                                }
                                var t1195 bool = parsed__110.hexadecimal
                                var t1196 bool = !t1195
                                var jp1186 bool
                                if t1196 {
                                    var t1197 int = parsed__110.decimal_exponent
                                    var t1198 bool = t1197 > 0
                                    jp1186 = t1198
                                } else {
                                    jp1186 = false
                                }
                                var jp1176 FloatNatural
                                if jp1186 {
                                    var t1187 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t1187)
                                    var count__118 int = 0
                                    Loop_loop1189:
                                    for {
                                        var t1190 int = parsed__110.decimal_exponent
                                        var t1191 bool = count__118 < t1190
                                        if t1191 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t1192 int = compound_old213 + compound_value214
                                            count__118 = t1192
                                            continue
                                        } else {
                                            break Loop_loop1189
                                        }
                                    }
                                    jp1176 = result__117
                                    var t1182 bool = parsed__110.hexadecimal
                                    var jp1178 int
                                    if t1182 {
                                        var t1183 int = parsed__110.binary_exponent
                                        jp1178 = t1183
                                    } else {
                                        var t1184 int = parsed__110.decimal_exponent
                                        jp1178 = t1184
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1176, jp1174, jp1178, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1179 bool = !x219
                                    var t1180 uint64 = jp1166 | x218
                                    var t1181 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1179,
                                        _1: t1180,
                                    }
                                    return t1181
                                } else {
                                    var t1194 FloatNatural = parsed__110.numerator
                                    jp1176 = t1194
                                    var t1182 bool = parsed__110.hexadecimal
                                    var jp1178 int
                                    if t1182 {
                                        var t1183 int = parsed__110.binary_exponent
                                        jp1178 = t1183
                                    } else {
                                        var t1184 int = parsed__110.decimal_exponent
                                        jp1178 = t1184
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1176, jp1174, jp1178, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1179 bool = !x219
                                    var t1180 uint64 = jp1166 | x218
                                    var t1181 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1179,
                                        _1: t1180,
                                    }
                                    return t1181
                                }
                            }
                        }
                    } else {
                        var t1205 bool = parsed__110.hexadecimal
                        var t1206 bool = !t1205
                        var jp1200 bool
                        if t1206 {
                            var t1207 int = parsed__110.decimal_exponent
                            var t1208 bool = t1207 < 0
                            jp1200 = t1208
                        } else {
                            jp1200 = false
                        }
                        var jp1174 FloatNatural
                        if jp1200 {
                            var t1201 int = parsed__110.decimal_exponent
                            var t1202 int = 0 - t1201
                            var t1203 FloatNatural = float_natural_power5(t1202)
                            jp1174 = t1203
                        } else {
                            var inline2049 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline2049, 1)
                            var inline2051 FloatNatural = FloatNatural{
                                words: inline2049,
                            }
                            jp1174 = inline2051
                        }
                        var t1195 bool = parsed__110.hexadecimal
                        var t1196 bool = !t1195
                        var jp1186 bool
                        if t1196 {
                            var t1197 int = parsed__110.decimal_exponent
                            var t1198 bool = t1197 > 0
                            jp1186 = t1198
                        } else {
                            jp1186 = false
                        }
                        var jp1176 FloatNatural
                        if jp1186 {
                            var t1187 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t1187)
                            var count__118 int = 0
                            Loop_loop1189__2:
                            for {
                                var t1190 int = parsed__110.decimal_exponent
                                var t1191 bool = count__118 < t1190
                                if t1191 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t1192 int = compound_old213 + compound_value214
                                    count__118 = t1192
                                    continue
                                } else {
                                    break Loop_loop1189__2
                                }
                            }
                            jp1176 = result__117
                            var t1182 bool = parsed__110.hexadecimal
                            var jp1178 int
                            if t1182 {
                                var t1183 int = parsed__110.binary_exponent
                                jp1178 = t1183
                            } else {
                                var t1184 int = parsed__110.decimal_exponent
                                jp1178 = t1184
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1176, jp1174, jp1178, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1179 bool = !x219
                            var t1180 uint64 = jp1166 | x218
                            var t1181 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1179,
                                _1: t1180,
                            }
                            return t1181
                        } else {
                            var t1194 FloatNatural = parsed__110.numerator
                            jp1176 = t1194
                            var t1182 bool = parsed__110.hexadecimal
                            var jp1178 int
                            if t1182 {
                                var t1183 int = parsed__110.binary_exponent
                                jp1178 = t1183
                            } else {
                                var t1184 int = parsed__110.decimal_exponent
                                jp1178 = t1184
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1176, jp1174, jp1178, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1179 bool = !x219
                            var t1180 uint64 = jp1166 | x218
                            var t1181 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1179,
                                _1: t1180,
                            }
                            return t1181
                        }
                    }
                }
            }
        }
    }
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1279 bool = factor__16 == 0
    if t1279 {
        var t1280 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1280, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1273 uint64 = uint64(uint32(factor__16))
        Loop_loop1266:
        for {
            var t1267 *_goml_vec_uint32 = value__15.words
            var t1268 int
            var inline2055 int = vec_len__Vec_6uint32(t1267)
            t1268 = inline2055
            var t1269 bool = index__18 < t1268
            if t1269 {
                var t1270 *_goml_vec_uint32 = value__15.words
                var t1271 uint32 = vec_get__Vec_6uint32(t1270, index__18)
                var t1272 uint64 = uint64(uint32(t1271))
                var t1274 uint64 = t1272 * t1273
                var product__19 uint64 = t1274 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1276_rhs int = 32
                var t1276 uint64 = product__19 >> t1276_rhs
                carry__17 = t1276
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1277 int = compound_old30 + compound_value31
                index__18 = t1277
                continue
            } else {
                break Loop_loop1266
            }
        }
        var t1262 bool = carry__17 != 0
        if t1262 {
            var t1263 *_goml_vec_uint32 = value__15.words
            var t1264 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1263, t1264)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_natural_zero() FloatNatural {
    var t1283 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1284 FloatNatural = FloatNatural{
        words: t1283,
    }
    return t1284
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline2066 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2067 FloatNatural = FloatNatural{
        words: inline2066,
    }
    result__5 = inline2067
    var index__6 int = 0
    Loop_loop1294:
    for {
        var t1295 *_goml_vec_uint32 = value__4.words
        var t1296 int
        var inline2064 int = vec_len__Vec_6uint32(t1295)
        t1296 = inline2064
        var t1297 bool = index__6 < t1296
        if t1297 {
            var t1298 *_goml_vec_uint32 = result__5.words
            var t1299 *_goml_vec_uint32 = value__4.words
            var t1300 uint32 = vec_get__Vec_6uint32(t1299, index__6)
            vec_push__Vec_6uint32(t1298, t1300)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1301 int = compound_old4 + compound_value5
            index__6 = t1301
            continue
        } else {
            break Loop_loop1294
        }
    }
    return result__5
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1308 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline2069 int = vec_len__Vec_6uint32(t1308)
    index__47 = inline2069
    var t1319 uint64 = uint64(uint32(divisor__45))
    var t1322 uint64 = uint64(uint32(divisor__45))
    Loop_loop1311:
    for {
        var t1312 bool = index__47 > 0
        if t1312 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1313 int = compound_old83 - compound_value84
            index__47 = t1313
            var t1315_rhs int = 32
            var t1315 uint64 = remainder__46 << t1315_rhs
            var t1316 *_goml_vec_uint32 = value__44.words
            var t1317 uint32 = vec_get__Vec_6uint32(t1316, index__47)
            var t1318 uint64 = uint64(uint32(t1317))
            var current__48 uint64 = t1315 | t1318
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1320 uint64 = current__48 / t1319
            var value90 uint32 = uint32(uint64(t1320))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1323 uint64 = current__48 % t1322
            remainder__46 = t1323
            continue
        } else {
            break Loop_loop1311
        }
    }
    float_natural_trim(value__44)
    var t1310 uint32 = uint32(uint64(remainder__46))
    return t1310
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1326 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1326
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline2071 bool = string_is_char_boundary(self__294, start__295)
    var inline2073 bool
    if inline2071 {
        var inline2076 bool = string_is_char_boundary(self__294, end__296)
        inline2073 = inline2076
    } else {
        inline2073 = false
    }
    if inline2073 {
        var inline2074 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline2074
    } else {
        var inline2075 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2075
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1514 bool = string_equals_ascii_case(value__84, "nan")
    if t1514 {
        var t1515 FloatNatural
        var inline2078 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2079 FloatNatural = FloatNatural{
            words: inline2078,
        }
        t1515 = inline2079
        var t1516 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1515,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1516
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1506 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1507 bool = index__85 < t1506
        var jp1501 bool
        if t1507 {
            var t1510 uint8
            var inline2083 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1510 = inline2083
            var t1511 bool = t1510 == 43
            if t1511 {
                jp1501 = true
            } else {
                var t1512 uint8
                var inline2081 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1512 = inline2081
                var t1513 bool = t1512 == 45
                jp1501 = t1513
            }
        } else {
            jp1501 = false
        }
        if jp1501 {
            var t1502 uint8
            var inline2085 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1502 = inline2085
            var t1503 bool = t1502 == 45
            negative__86 = t1503
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1504 int = compound_old140 + compound_value141
            index__85 = t1504
        } else {}
        var t1334 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1334)
        var t1498 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1495 bool
        if t1498 {
            jp1495 = true
        } else {
            var t1499 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1495 = t1499
        }
        if jp1495 {
            var t1496 FloatNatural
            var inline2087 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2088 FloatNatural = FloatNatural{
                words: inline2087,
            }
            t1496 = inline2088
            var t1497 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1496,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1497
        } else {
            var t1489 int = index__85 + 2
            var t1490 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1491 bool = t1489 <= t1490
            var jp1484 bool
            if t1491 {
                var t1492 uint8
                var inline2090 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1492 = inline2090
                var t1493 bool = t1492 == 48
                jp1484 = t1493
            } else {
                jp1484 = false
            }
            var jp1337 bool
            if jp1484 {
                var t1485 int = index__85 + 1
                var t1486 uint8
                var inline2099 uint8 = _goml_runtime_core_string_byte_get(value__84, t1485)
                t1486 = inline2099
                var t1487 uint8
                var inline2092 bool = t1486 >= 65
                var inline2094 bool
                if inline2092 {
                    var inline2097 bool = t1486 <= 90
                    inline2094 = inline2097
                } else {
                    inline2094 = false
                }
                if inline2094 {
                    var inline2095 uint8 = 97 - 65
                    var inline2096 uint8 = t1486 + inline2095
                    t1487 = inline2096
                    var t1488 bool = t1487 == 120
                    jp1337 = t1488
                    if jp1337 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1481 int = compound_old145 + compound_value146
                        index__85 = t1481
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1340 int
                    if jp1337 {
                        jp1340 = 16
                    } else {
                        jp1340 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1434 uint32 = uint32(int(jp1340))
                    Loop_loop1430:
                    for {
                        var t1431 int
                        var inline2113 int = _goml_runtime_core_string_len(value__84)
                        t1431 = inline2113
                        var t1432 bool = index__85 < t1431
                        if t1432 {
                            var current__97 uint8
                            var inline2111 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2111
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1340)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1434)
                                var t1435 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1435)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1446 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1446
                                } else {}
                                var t1444 bool = significant_digits__95 > 0
                                var jp1441 bool
                                if t1444 {
                                    jp1441 = true
                                } else {
                                    var t1445 bool = x151 != 0
                                    jp1441 = t1445
                                }
                                if jp1441 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1442 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1442
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1438 int = compound_old164 + compound_value165
                                index__85 = t1438
                                continue
                            } else {
                                var t1449 bool = current__97 == 95
                                if t1449 {
                                    var t1470 int = index__85 + 1
                                    var t1471 int
                                    var inline2109 int = _goml_runtime_core_string_len(value__84)
                                    t1471 = inline2109
                                    var t1472 bool = t1470 >= t1471
                                    if t1472 {
                                        var inline2101 FloatNatural = float_natural_zero()
                                        var inline2102 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2101,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2102
                                    } else {
                                        var t1451 int = index__85 + 1
                                        var t1452 uint8
                                        var inline2107 uint8 = _goml_runtime_core_string_byte_get(value__84, t1451)
                                        t1452 = inline2107
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1452, jp1340)
                                        var x169 bool = mtmp168._0
                                        var jp1467 bool
                                        if jp1337 {
                                            var t1469 bool = !saw_digit__92
                                            jp1467 = t1469
                                        } else {
                                            jp1467 = false
                                        }
                                        var jp1454 bool
                                        if jp1467 {
                                            var t1468 bool = index__85 == mantissa_start__89
                                            jp1454 = t1468
                                        } else {
                                            jp1454 = false
                                        }
                                        var t1464 bool = !previous_digit__96
                                        var jp1462 bool
                                        if t1464 {
                                            var t1465 bool = !jp1454
                                            jp1462 = t1465
                                        } else {
                                            jp1462 = false
                                        }
                                        var jp1459 bool
                                        if jp1462 {
                                            jp1459 = true
                                        } else {
                                            var t1463 bool = !x169
                                            jp1459 = t1463
                                        }
                                        if jp1459 {
                                            var inline2104 FloatNatural = float_natural_zero()
                                            var inline2105 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2104,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2105
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1456 int = compound_old173 + compound_value174
                                            index__85 = t1456
                                            continue
                                        }
                                    }
                                } else {
                                    var t1479 bool = current__97 == 46
                                    var jp1476 bool
                                    if t1479 {
                                        var t1480 bool = !saw_dot__93
                                        jp1476 = t1480
                                    } else {
                                        jp1476 = false
                                    }
                                    if jp1476 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1477 int = compound_old178 + compound_value179
                                        index__85 = t1477
                                        continue
                                    } else {
                                        break Loop_loop1430
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1430
                        }
                    }
                    var t1428 bool = !saw_digit__92
                    if t1428 {
                        var inline2115 FloatNatural = float_natural_zero()
                        var inline2116 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2115,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2116
                    } else {
                        var jp1344 uint8
                        if jp1337 {
                            jp1344 = 112
                        } else {
                            jp1344 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1424 bool = index__85 < t1423
                        var jp1361 bool
                        if t1424 {
                            var t1425 uint8
                            var inline2118 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1425 = inline2118
                            var t1426 uint8 = ascii_lower(t1425)
                            var t1427 bool = t1426 == jp1344
                            jp1361 = t1427
                        } else {
                            jp1361 = false
                        }
                        if jp1361 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1362 int = compound_old183 + compound_value184
                            index__85 = t1362
                            var t1413 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1414 bool = index__85 < t1413
                            var jp1408 bool
                            if t1414 {
                                var t1417 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1418 bool = t1417 == 43
                                if t1418 {
                                    jp1408 = true
                                } else {
                                    var t1419 uint8
                                    var inline2120 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1419 = inline2120
                                    var t1420 bool = t1419 == 45
                                    jp1408 = t1420
                                }
                            } else {
                                jp1408 = false
                            }
                            if jp1408 {
                                var t1409 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1410 bool = t1409 == 45
                                exponent_negative__104 = t1410
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1411 int = compound_old187 + compound_value188
                                index__85 = t1411
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1369:
                            for {
                                var t1370 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1371 bool = index__85 < t1370
                                if t1371 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1405 bool = current__106 >= 48
                                    var jp1374 bool
                                    if t1405 {
                                        var t1406 bool = current__106 <= 57
                                        jp1374 = t1406
                                    } else {
                                        jp1374 = false
                                    }
                                    if jp1374 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1378 bool = exponent__103 < 1000000
                                        if t1378 {
                                            var t1379 int = exponent__103 * 10
                                            var t1380 uint8 = current__106 - 48
                                            var t1381 int = int(uint8(t1380))
                                            var t1382 int = t1379 + t1381
                                            exponent__103 = t1382
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1376 int = compound_old196 + compound_value197
                                        index__85 = t1376
                                        continue
                                    } else {
                                        var t1384 bool = current__106 == 95
                                        if t1384 {
                                            var t1401 bool = !previous_digit__96
                                            var jp1397 bool
                                            if t1401 {
                                                jp1397 = true
                                            } else {
                                                var t1402 int = index__85 + 1
                                                var t1403 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1404 bool = t1402 >= t1403
                                                jp1397 = t1404
                                            }
                                            var jp1392 bool
                                            if jp1397 {
                                                jp1392 = true
                                            } else {
                                                var t1398 int = index__85 + 1
                                                var t1399 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1398)
                                                var t1400 bool = t1399 < 48
                                                jp1392 = t1400
                                            }
                                            var jp1389 bool
                                            if jp1392 {
                                                jp1389 = true
                                            } else {
                                                var t1393 int = index__85 + 1
                                                var t1394 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1393)
                                                var t1395 bool = t1394 > 57
                                                jp1389 = t1395
                                            }
                                            if jp1389 {
                                                var t1390 ParsedFloat = invalid_parsed_float()
                                                return t1390
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1386 int = compound_old201 + compound_value202
                                                index__85 = t1386
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1369
                                        }
                                    }
                                } else {
                                    break Loop_loop1369
                                }
                            }
                            var t1367 bool = !exponent_digits__105
                            if t1367 {
                                var t1368 ParsedFloat = invalid_parsed_float()
                                return t1368
                            } else {
                                var t1357 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1358 bool = index__85 != t1357
                                if t1358 {
                                    var t1359 ParsedFloat = invalid_parsed_float()
                                    return t1359
                                } else {
                                    if exponent_negative__104 {
                                        var t1356 int = 0 - exponent__103
                                        exponent__103 = t1356
                                    } else {}
                                    var jp1349 int
                                    if jp1337 {
                                        jp1349 = 0
                                    } else {
                                        var t1355 int = exponent__103 - fraction_digits__94
                                        jp1349 = t1355
                                    }
                                    var jp1351 int
                                    if jp1337 {
                                        var t1353 int = fraction_digits__94 * 4
                                        var t1354 int = exponent__103 - t1353
                                        jp1351 = t1354
                                    } else {
                                        jp1351 = 0
                                    }
                                    var t1352 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1349,
                                        binary_exponent: jp1351,
                                        hexadecimal: jp1337,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1352
                                }
                            }
                        } else {
                            if jp1337 {
                                var t1422 ParsedFloat = invalid_parsed_float()
                                return t1422
                            } else {
                                var t1357 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1358 bool = index__85 != t1357
                                if t1358 {
                                    var t1359 ParsedFloat = invalid_parsed_float()
                                    return t1359
                                } else {
                                    if exponent_negative__104 {
                                        var t1356 int = 0 - exponent__103
                                        exponent__103 = t1356
                                    } else {}
                                    var jp1349 int
                                    if jp1337 {
                                        jp1349 = 0
                                    } else {
                                        var t1355 int = exponent__103 - fraction_digits__94
                                        jp1349 = t1355
                                    }
                                    var jp1351 int
                                    if jp1337 {
                                        var t1353 int = fraction_digits__94 * 4
                                        var t1354 int = exponent__103 - t1353
                                        jp1351 = t1354
                                    } else {
                                        jp1351 = 0
                                    }
                                    var t1352 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1349,
                                        binary_exponent: jp1351,
                                        hexadecimal: jp1337,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1352
                                }
                            }
                        }
                    }
                } else {
                    t1487 = t1486
                    var t1488 bool = t1487 == 120
                    jp1337 = t1488
                    if jp1337 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1481 int = compound_old145 + compound_value146
                        index__85 = t1481
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1340 int
                    if jp1337 {
                        jp1340 = 16
                    } else {
                        jp1340 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1434 uint32 = uint32(int(jp1340))
                    Loop_loop1430__2:
                    for {
                        var t1431 int
                        var inline2113 int = _goml_runtime_core_string_len(value__84)
                        t1431 = inline2113
                        var t1432 bool = index__85 < t1431
                        if t1432 {
                            var current__97 uint8
                            var inline2111 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2111
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1340)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1434)
                                var t1435 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1435)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1446 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1446
                                } else {}
                                var t1444 bool = significant_digits__95 > 0
                                var jp1441 bool
                                if t1444 {
                                    jp1441 = true
                                } else {
                                    var t1445 bool = x151 != 0
                                    jp1441 = t1445
                                }
                                if jp1441 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1442 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1442
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1438 int = compound_old164 + compound_value165
                                index__85 = t1438
                                continue
                            } else {
                                var t1449 bool = current__97 == 95
                                if t1449 {
                                    var t1470 int = index__85 + 1
                                    var t1471 int
                                    var inline2109 int = _goml_runtime_core_string_len(value__84)
                                    t1471 = inline2109
                                    var t1472 bool = t1470 >= t1471
                                    if t1472 {
                                        var inline2101 FloatNatural = float_natural_zero()
                                        var inline2102 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2101,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2102
                                    } else {
                                        var t1451 int = index__85 + 1
                                        var t1452 uint8
                                        var inline2107 uint8 = _goml_runtime_core_string_byte_get(value__84, t1451)
                                        t1452 = inline2107
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1452, jp1340)
                                        var x169 bool = mtmp168._0
                                        var jp1467 bool
                                        if jp1337 {
                                            var t1469 bool = !saw_digit__92
                                            jp1467 = t1469
                                        } else {
                                            jp1467 = false
                                        }
                                        var jp1454 bool
                                        if jp1467 {
                                            var t1468 bool = index__85 == mantissa_start__89
                                            jp1454 = t1468
                                        } else {
                                            jp1454 = false
                                        }
                                        var t1464 bool = !previous_digit__96
                                        var jp1462 bool
                                        if t1464 {
                                            var t1465 bool = !jp1454
                                            jp1462 = t1465
                                        } else {
                                            jp1462 = false
                                        }
                                        var jp1459 bool
                                        if jp1462 {
                                            jp1459 = true
                                        } else {
                                            var t1463 bool = !x169
                                            jp1459 = t1463
                                        }
                                        if jp1459 {
                                            var inline2104 FloatNatural = float_natural_zero()
                                            var inline2105 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2104,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2105
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1456 int = compound_old173 + compound_value174
                                            index__85 = t1456
                                            continue
                                        }
                                    }
                                } else {
                                    var t1479 bool = current__97 == 46
                                    var jp1476 bool
                                    if t1479 {
                                        var t1480 bool = !saw_dot__93
                                        jp1476 = t1480
                                    } else {
                                        jp1476 = false
                                    }
                                    if jp1476 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1477 int = compound_old178 + compound_value179
                                        index__85 = t1477
                                        continue
                                    } else {
                                        break Loop_loop1430__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1430__2
                        }
                    }
                    var t1428 bool = !saw_digit__92
                    if t1428 {
                        var inline2115 FloatNatural = float_natural_zero()
                        var inline2116 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2115,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2116
                    } else {
                        var jp1344 uint8
                        if jp1337 {
                            jp1344 = 112
                        } else {
                            jp1344 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1424 bool = index__85 < t1423
                        var jp1361 bool
                        if t1424 {
                            var t1425 uint8
                            var inline2118 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1425 = inline2118
                            var t1426 uint8 = ascii_lower(t1425)
                            var t1427 bool = t1426 == jp1344
                            jp1361 = t1427
                        } else {
                            jp1361 = false
                        }
                        if jp1361 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1362 int = compound_old183 + compound_value184
                            index__85 = t1362
                            var t1413 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1414 bool = index__85 < t1413
                            var jp1408 bool
                            if t1414 {
                                var t1417 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1418 bool = t1417 == 43
                                if t1418 {
                                    jp1408 = true
                                } else {
                                    var t1419 uint8
                                    var inline2120 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1419 = inline2120
                                    var t1420 bool = t1419 == 45
                                    jp1408 = t1420
                                }
                            } else {
                                jp1408 = false
                            }
                            if jp1408 {
                                var t1409 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1410 bool = t1409 == 45
                                exponent_negative__104 = t1410
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1411 int = compound_old187 + compound_value188
                                index__85 = t1411
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1369__2:
                            for {
                                var t1370 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1371 bool = index__85 < t1370
                                if t1371 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1405 bool = current__106 >= 48
                                    var jp1374 bool
                                    if t1405 {
                                        var t1406 bool = current__106 <= 57
                                        jp1374 = t1406
                                    } else {
                                        jp1374 = false
                                    }
                                    if jp1374 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1378 bool = exponent__103 < 1000000
                                        if t1378 {
                                            var t1379 int = exponent__103 * 10
                                            var t1380 uint8 = current__106 - 48
                                            var t1381 int = int(uint8(t1380))
                                            var t1382 int = t1379 + t1381
                                            exponent__103 = t1382
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1376 int = compound_old196 + compound_value197
                                        index__85 = t1376
                                        continue
                                    } else {
                                        var t1384 bool = current__106 == 95
                                        if t1384 {
                                            var t1401 bool = !previous_digit__96
                                            var jp1397 bool
                                            if t1401 {
                                                jp1397 = true
                                            } else {
                                                var t1402 int = index__85 + 1
                                                var t1403 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1404 bool = t1402 >= t1403
                                                jp1397 = t1404
                                            }
                                            var jp1392 bool
                                            if jp1397 {
                                                jp1392 = true
                                            } else {
                                                var t1398 int = index__85 + 1
                                                var t1399 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1398)
                                                var t1400 bool = t1399 < 48
                                                jp1392 = t1400
                                            }
                                            var jp1389 bool
                                            if jp1392 {
                                                jp1389 = true
                                            } else {
                                                var t1393 int = index__85 + 1
                                                var t1394 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1393)
                                                var t1395 bool = t1394 > 57
                                                jp1389 = t1395
                                            }
                                            if jp1389 {
                                                var t1390 ParsedFloat = invalid_parsed_float()
                                                return t1390
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1386 int = compound_old201 + compound_value202
                                                index__85 = t1386
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1369__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1369__2
                                }
                            }
                            var t1367 bool = !exponent_digits__105
                            if t1367 {
                                var t1368 ParsedFloat = invalid_parsed_float()
                                return t1368
                            } else {
                                var t1357 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1358 bool = index__85 != t1357
                                if t1358 {
                                    var t1359 ParsedFloat = invalid_parsed_float()
                                    return t1359
                                } else {
                                    if exponent_negative__104 {
                                        var t1356 int = 0 - exponent__103
                                        exponent__103 = t1356
                                    } else {}
                                    var jp1349 int
                                    if jp1337 {
                                        jp1349 = 0
                                    } else {
                                        var t1355 int = exponent__103 - fraction_digits__94
                                        jp1349 = t1355
                                    }
                                    var jp1351 int
                                    if jp1337 {
                                        var t1353 int = fraction_digits__94 * 4
                                        var t1354 int = exponent__103 - t1353
                                        jp1351 = t1354
                                    } else {
                                        jp1351 = 0
                                    }
                                    var t1352 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1349,
                                        binary_exponent: jp1351,
                                        hexadecimal: jp1337,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1352
                                }
                            }
                        } else {
                            if jp1337 {
                                var t1422 ParsedFloat = invalid_parsed_float()
                                return t1422
                            } else {
                                var t1357 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1358 bool = index__85 != t1357
                                if t1358 {
                                    var t1359 ParsedFloat = invalid_parsed_float()
                                    return t1359
                                } else {
                                    if exponent_negative__104 {
                                        var t1356 int = 0 - exponent__103
                                        exponent__103 = t1356
                                    } else {}
                                    var jp1349 int
                                    if jp1337 {
                                        jp1349 = 0
                                    } else {
                                        var t1355 int = exponent__103 - fraction_digits__94
                                        jp1349 = t1355
                                    }
                                    var jp1351 int
                                    if jp1337 {
                                        var t1353 int = fraction_digits__94 * 4
                                        var t1354 int = exponent__103 - t1353
                                        jp1351 = t1354
                                    } else {
                                        jp1351 = 0
                                    }
                                    var t1352 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1349,
                                        binary_exponent: jp1351,
                                        hexadecimal: jp1337,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1352
                                }
                            }
                        }
                    }
                }
            } else {
                jp1337 = false
                if jp1337 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1481 int = compound_old145 + compound_value146
                    index__85 = t1481
                } else {}
                var mantissa_start__89 int = index__85
                var jp1340 int
                if jp1337 {
                    jp1340 = 16
                } else {
                    jp1340 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1434 uint32 = uint32(int(jp1340))
                Loop_loop1430__3:
                for {
                    var t1431 int
                    var inline2113 int = _goml_runtime_core_string_len(value__84)
                    t1431 = inline2113
                    var t1432 bool = index__85 < t1431
                    if t1432 {
                        var current__97 uint8
                        var inline2111 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline2111
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1340)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1434)
                            var t1435 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1435)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1446 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1446
                            } else {}
                            var t1444 bool = significant_digits__95 > 0
                            var jp1441 bool
                            if t1444 {
                                jp1441 = true
                            } else {
                                var t1445 bool = x151 != 0
                                jp1441 = t1445
                            }
                            if jp1441 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1442 int = compound_old160 + compound_value161
                                significant_digits__95 = t1442
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1438 int = compound_old164 + compound_value165
                            index__85 = t1438
                            continue
                        } else {
                            var t1449 bool = current__97 == 95
                            if t1449 {
                                var t1470 int = index__85 + 1
                                var t1471 int
                                var inline2109 int = _goml_runtime_core_string_len(value__84)
                                t1471 = inline2109
                                var t1472 bool = t1470 >= t1471
                                if t1472 {
                                    var inline2101 FloatNatural = float_natural_zero()
                                    var inline2102 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline2101,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline2102
                                } else {
                                    var t1451 int = index__85 + 1
                                    var t1452 uint8
                                    var inline2107 uint8 = _goml_runtime_core_string_byte_get(value__84, t1451)
                                    t1452 = inline2107
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1452, jp1340)
                                    var x169 bool = mtmp168._0
                                    var jp1467 bool
                                    if jp1337 {
                                        var t1469 bool = !saw_digit__92
                                        jp1467 = t1469
                                    } else {
                                        jp1467 = false
                                    }
                                    var jp1454 bool
                                    if jp1467 {
                                        var t1468 bool = index__85 == mantissa_start__89
                                        jp1454 = t1468
                                    } else {
                                        jp1454 = false
                                    }
                                    var t1464 bool = !previous_digit__96
                                    var jp1462 bool
                                    if t1464 {
                                        var t1465 bool = !jp1454
                                        jp1462 = t1465
                                    } else {
                                        jp1462 = false
                                    }
                                    var jp1459 bool
                                    if jp1462 {
                                        jp1459 = true
                                    } else {
                                        var t1463 bool = !x169
                                        jp1459 = t1463
                                    }
                                    if jp1459 {
                                        var inline2104 FloatNatural = float_natural_zero()
                                        var inline2105 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2104,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2105
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1456 int = compound_old173 + compound_value174
                                        index__85 = t1456
                                        continue
                                    }
                                }
                            } else {
                                var t1479 bool = current__97 == 46
                                var jp1476 bool
                                if t1479 {
                                    var t1480 bool = !saw_dot__93
                                    jp1476 = t1480
                                } else {
                                    jp1476 = false
                                }
                                if jp1476 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1477 int = compound_old178 + compound_value179
                                    index__85 = t1477
                                    continue
                                } else {
                                    break Loop_loop1430__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1430__3
                    }
                }
                var t1428 bool = !saw_digit__92
                if t1428 {
                    var inline2115 FloatNatural = float_natural_zero()
                    var inline2116 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline2115,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline2116
                } else {
                    var jp1344 uint8
                    if jp1337 {
                        jp1344 = 112
                    } else {
                        jp1344 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1424 bool = index__85 < t1423
                    var jp1361 bool
                    if t1424 {
                        var t1425 uint8
                        var inline2118 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1425 = inline2118
                        var t1426 uint8 = ascii_lower(t1425)
                        var t1427 bool = t1426 == jp1344
                        jp1361 = t1427
                    } else {
                        jp1361 = false
                    }
                    if jp1361 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1362 int = compound_old183 + compound_value184
                        index__85 = t1362
                        var t1413 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1414 bool = index__85 < t1413
                        var jp1408 bool
                        if t1414 {
                            var t1417 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1418 bool = t1417 == 43
                            if t1418 {
                                jp1408 = true
                            } else {
                                var t1419 uint8
                                var inline2120 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1419 = inline2120
                                var t1420 bool = t1419 == 45
                                jp1408 = t1420
                            }
                        } else {
                            jp1408 = false
                        }
                        if jp1408 {
                            var t1409 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1410 bool = t1409 == 45
                            exponent_negative__104 = t1410
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1411 int = compound_old187 + compound_value188
                            index__85 = t1411
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1369__3:
                        for {
                            var t1370 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1371 bool = index__85 < t1370
                            if t1371 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1405 bool = current__106 >= 48
                                var jp1374 bool
                                if t1405 {
                                    var t1406 bool = current__106 <= 57
                                    jp1374 = t1406
                                } else {
                                    jp1374 = false
                                }
                                if jp1374 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1378 bool = exponent__103 < 1000000
                                    if t1378 {
                                        var t1379 int = exponent__103 * 10
                                        var t1380 uint8 = current__106 - 48
                                        var t1381 int = int(uint8(t1380))
                                        var t1382 int = t1379 + t1381
                                        exponent__103 = t1382
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1376 int = compound_old196 + compound_value197
                                    index__85 = t1376
                                    continue
                                } else {
                                    var t1384 bool = current__106 == 95
                                    if t1384 {
                                        var t1401 bool = !previous_digit__96
                                        var jp1397 bool
                                        if t1401 {
                                            jp1397 = true
                                        } else {
                                            var t1402 int = index__85 + 1
                                            var t1403 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1404 bool = t1402 >= t1403
                                            jp1397 = t1404
                                        }
                                        var jp1392 bool
                                        if jp1397 {
                                            jp1392 = true
                                        } else {
                                            var t1398 int = index__85 + 1
                                            var t1399 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1398)
                                            var t1400 bool = t1399 < 48
                                            jp1392 = t1400
                                        }
                                        var jp1389 bool
                                        if jp1392 {
                                            jp1389 = true
                                        } else {
                                            var t1393 int = index__85 + 1
                                            var t1394 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1393)
                                            var t1395 bool = t1394 > 57
                                            jp1389 = t1395
                                        }
                                        if jp1389 {
                                            var t1390 ParsedFloat = invalid_parsed_float()
                                            return t1390
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1386 int = compound_old201 + compound_value202
                                            index__85 = t1386
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1369__3
                                    }
                                }
                            } else {
                                break Loop_loop1369__3
                            }
                        }
                        var t1367 bool = !exponent_digits__105
                        if t1367 {
                            var t1368 ParsedFloat = invalid_parsed_float()
                            return t1368
                        } else {
                            var t1357 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1358 bool = index__85 != t1357
                            if t1358 {
                                var t1359 ParsedFloat = invalid_parsed_float()
                                return t1359
                            } else {
                                if exponent_negative__104 {
                                    var t1356 int = 0 - exponent__103
                                    exponent__103 = t1356
                                } else {}
                                var jp1349 int
                                if jp1337 {
                                    jp1349 = 0
                                } else {
                                    var t1355 int = exponent__103 - fraction_digits__94
                                    jp1349 = t1355
                                }
                                var jp1351 int
                                if jp1337 {
                                    var t1353 int = fraction_digits__94 * 4
                                    var t1354 int = exponent__103 - t1353
                                    jp1351 = t1354
                                } else {
                                    jp1351 = 0
                                }
                                var t1352 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1349,
                                    binary_exponent: jp1351,
                                    hexadecimal: jp1337,
                                    significant_digits: significant_digits__95,
                                }
                                return t1352
                            }
                        }
                    } else {
                        if jp1337 {
                            var t1422 ParsedFloat = invalid_parsed_float()
                            return t1422
                        } else {
                            var t1357 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1358 bool = index__85 != t1357
                            if t1358 {
                                var t1359 ParsedFloat = invalid_parsed_float()
                                return t1359
                            } else {
                                if exponent_negative__104 {
                                    var t1356 int = 0 - exponent__103
                                    exponent__103 = t1356
                                } else {}
                                var jp1349 int
                                if jp1337 {
                                    jp1349 = 0
                                } else {
                                    var t1355 int = exponent__103 - fraction_digits__94
                                    jp1349 = t1355
                                }
                                var jp1351 int
                                if jp1337 {
                                    var t1353 int = fraction_digits__94 * 4
                                    var t1354 int = exponent__103 - t1353
                                    jp1351 = t1354
                                } else {
                                    jp1351 = 0
                                }
                                var t1352 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1349,
                                    binary_exponent: jp1351,
                                    hexadecimal: jp1337,
                                    significant_digits: significant_digits__95,
                                }
                                return t1352
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
    var inline2122 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline2122, 1)
    var inline2124 FloatNatural = FloatNatural{
        words: inline2122,
    }
    result__26 = inline2124
    var count__27 int = 0
    Loop_loop1520:
    for {
        var t1521 bool = count__27 < exponent__25
        if t1521 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1522 int = compound_old46 + compound_value47
            count__27 = t1522
            continue
        } else {
            break Loop_loop1520
        }
    }
    return result__26
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1609 bool
    var inline2126 *_goml_vec_uint32 = numerator__65.words
    var inline2127 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2126)
    t1609 = inline2127
    if t1609 {
        var t1610 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1610
    } else {
        var t1606 bool = binary_shift__67 >= 0
        var jp1531 FloatNatural
        if t1606 {
            var t1607 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1531 = t1607
        } else {
            var t1608 FloatNatural = float_natural_copy(numerator__65)
            jp1531 = t1608
        }
        var t1602 bool = binary_shift__67 >= 0
        var jp1533 FloatNatural
        if t1602 {
            var t1603 FloatNatural = float_natural_copy(denominator__66)
            jp1533 = t1603
        } else {
            var t1604 int = 0 - binary_shift__67
            var t1605 FloatNatural = float_natural_shift_left(denominator__66, t1604)
            jp1533 = t1605
        }
        var t1534 int = float_natural_bit_length(jp1531)
        var t1535 int = float_natural_bit_length(jp1533)
        var exponent__72 int = t1534 - t1535
        var t1596 bool = exponent__72 >= 0
        var jp1537 int
        if t1596 {
            var t1597 FloatNatural = float_natural_shift_left(jp1533, exponent__72)
            var t1598 int = float_natural_compare(jp1531, t1597)
            jp1537 = t1598
        } else {
            var t1599 int = 0 - exponent__72
            var t1600 FloatNatural = float_natural_shift_left(jp1531, t1599)
            var t1601 int = float_natural_compare(t1600, jp1533)
            jp1537 = t1601
        }
        var t1593 bool = jp1537 < 0
        if t1593 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1594 int = compound_old120 - compound_value121
            exponent__72 = t1594
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1587 bool = exponent__72 > exponent_bias__69
        if t1587 {
            var t1588 int = exponent_bias__69 + exponent_bias__69
            var t1589 int = t1588 + 1
            var t1590 uint64 = uint64(int(t1589))
            var t1591 uint64 = t1590 << mantissa_bits__68
            var t1592 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1591,
                _1: true,
            }
            return t1592
        } else {
            var t1582 bool = exponent__72 < minimum_exponent__74
            var jp1541 uint64
            if t1582 {
                var t1583 int = mantissa_bits__68 - minimum_exponent__74
                var t1584 uint64 = float_rational_quotient(jp1531, jp1533, t1583)
                jp1541 = t1584
            } else {
                var t1585 int = mantissa_bits__68 - exponent__72
                var t1586 uint64 = float_rational_quotient(jp1531, jp1533, t1585)
                jp1541 = t1586
            }
            var mantissa__76 uint64 = jp1541
            var t1544 bool = exponent__72 < minimum_exponent__74
            if t1544 {
                var t1547 bool = mantissa__76 == 0
                if t1547 {
                    var t1548 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1548
                } else {
                    var t1551_lhs uint64 = 1
                    var t1551 uint64 = t1551_lhs << mantissa_bits__68
                    var t1552 bool = mantissa__76 >= t1551
                    if t1552 {
                        var t1553_lhs uint64 = 1
                        var t1553 uint64 = t1553_lhs << mantissa_bits__68
                        var t1554_lhs uint64 = 1
                        var t1554 uint64 = t1554_lhs << mantissa_bits__68
                        var t1555 uint64 = mantissa__76 - t1554
                        var t1556 uint64 = t1553 | t1555
                        var t1557 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1556,
                            _1: false,
                        }
                        return t1557
                    } else {
                        var t1558 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1558
                    }
                }
            } else {
                var t1575 int = mantissa_bits__68 + 1
                var t1576_lhs uint64 = 1
                var t1576 uint64 = t1576_lhs << t1575
                var t1577 bool = mantissa__76 >= t1576
                if t1577 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1578 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1578
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1580 int = compound_old128 + compound_value129
                    exponent__72 = t1580
                } else {}
                var t1562 bool = exponent__72 > exponent_bias__69
                if t1562 {
                    var t1563 int = exponent_bias__69 + exponent_bias__69
                    var t1564 int = t1563 + 1
                    var t1565 uint64 = uint64(int(t1564))
                    var t1566 uint64 = t1565 << mantissa_bits__68
                    var t1567 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1566,
                        _1: true,
                    }
                    return t1567
                } else {
                    var t1568 int = exponent__72 + exponent_bias__69
                    var t1569 uint64 = uint64(int(t1568))
                    var t1570 uint64 = t1569 << mantissa_bits__68
                    var t1571_lhs uint64 = 1
                    var t1571 uint64 = t1571_lhs << mantissa_bits__68
                    var t1572 uint64 = mantissa__76 - t1571
                    var t1573 uint64 = t1570 | t1572
                    var t1574 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1573,
                        _1: false,
                    }
                    return t1574
                }
            }
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1615 int = vec_len__Vec_6uint32(self__528)
    var t1616 bool = t1615 == 0
    return t1616
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1619:
    for {
        var t1627 *_goml_vec_uint32 = value__7.words
        var t1628 bool
        var inline2138 int = vec_len__Vec_6uint32(t1627)
        var inline2139 bool = inline2138 == 0
        t1628 = inline2139
        var t1629 bool = !t1628
        var jp1621 bool
        if t1629 {
            var t1630 *_goml_vec_uint32 = value__7.words
            var t1631 *_goml_vec_uint32 = value__7.words
            var t1632 int
            var inline2132 int = vec_len__Vec_6uint32(t1631)
            t1632 = inline2132
            var t1633 int = t1632 - 1
            var t1634 uint32 = vec_get__Vec_6uint32(t1630, t1633)
            var t1635 bool = t1634 == 0
            jp1621 = t1635
        } else {
            jp1621 = false
        }
        if jp1621 {
            var t1622 *_goml_vec_uint32 = value__7.words
            var t1623 *_goml_vec_uint32 = value__7.words
            var t1624 int
            var inline2136 int = vec_len__Vec_6uint32(t1623)
            t1624 = inline2136
            var t1625 int = t1624 - 1
            vec_truncate__Vec_6uint32(t1622, t1625)
            continue
        } else {
            break Loop_loop1619
        }
    }
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1644 bool = string_is_char_boundary(value__274, start__275)
    var jp1641 bool
    if t1644 {
        var t1645 bool = string_is_char_boundary(value__274, end__276)
        jp1641 = t1645
    } else {
        jp1641 = false
    }
    if jp1641 {
        var t1642 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1642
    } else {
        var t1643 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1643
    }
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1660 int
    var inline2156 int = _goml_runtime_core_string_len(value__78)
    t1660 = inline2156
    var t1661 int
    var inline2154 int = _goml_runtime_core_string_len(expected__79)
    t1661 = inline2154
    var t1662 bool = t1660 != t1661
    if t1662 {
        return false
    } else {
        var index__80 int = 0
        var inline2146 uint8 = 97 - 65
        Loop_loop1650:
        for {
            var t1651 int
            var inline2152 int = _goml_runtime_core_string_len(value__78)
            t1651 = inline2152
            var t1652 bool = index__80 < t1651
            if t1652 {
                var t1656 uint8
                var inline2150 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1656 = inline2150
                var t1657 uint8
                var inline2143 bool = t1656 >= 65
                var inline2145 bool
                if inline2143 {
                    var inline2148 bool = t1656 <= 90
                    inline2145 = inline2148
                } else {
                    inline2145 = false
                }
                if inline2145 {
                    var inline2147 uint8 = t1656 + inline2146
                    t1657 = inline2147
                    var t1658 uint8
                    var inline2141 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1658 = inline2141
                    var t1659 bool = t1657 != t1658
                    if t1659 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1654 int = compound_old134 + compound_value135
                        index__80 = t1654
                        continue
                    }
                } else {
                    t1657 = t1656
                    var t1658 uint8
                    var inline2141 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1658 = inline2141
                    var t1659 bool = t1657 != t1658
                    if t1659 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1654 int = compound_old134 + compound_value135
                        index__80 = t1654
                        continue
                    }
                }
            } else {
                break Loop_loop1650
            }
        }
        return true
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1671 bool = value__77 >= 65
    var jp1668 bool
    if t1671 {
        var t1672 bool = value__77 <= 90
        jp1668 = t1672
    } else {
        jp1668 = false
    }
    if jp1668 {
        var t1669 uint8 = 97 - 65
        var t1670 uint8 = value__77 + t1669
        return t1670
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1699 bool = value__81 >= 48
    var jp1683 bool
    if t1699 {
        var t1700 bool = value__81 <= 57
        jp1683 = t1700
    } else {
        jp1683 = false
    }
    var jp1676 int
    if jp1683 {
        var t1684 uint8 = value__81 - 48
        var t1685 int = int(uint8(t1684))
        jp1676 = t1685
        var t1679 bool = jp1676 < base__82
        if t1679 {
            var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1676,
            }
            return t1680
        } else {
            var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1681
        }
    } else {
        var t1695 uint8
        var inline2172 bool = value__81 >= 65
        var inline2174 bool
        if inline2172 {
            var inline2177 bool = value__81 <= 90
            inline2174 = inline2177
        } else {
            inline2174 = false
        }
        if inline2174 {
            var inline2175 uint8 = 97 - 65
            var inline2176 uint8 = value__81 + inline2175
            t1695 = inline2176
            var t1696 bool = t1695 >= 97
            var jp1689 bool
            if t1696 {
                var t1697 uint8
                var inline2158 bool = value__81 >= 65
                var inline2160 bool
                if inline2158 {
                    var inline2163 bool = value__81 <= 90
                    inline2160 = inline2163
                } else {
                    inline2160 = false
                }
                if inline2160 {
                    var inline2161 uint8 = 97 - 65
                    var inline2162 uint8 = value__81 + inline2161
                    t1697 = inline2162
                    var t1698 bool = t1697 <= 102
                    jp1689 = t1698
                    if jp1689 {
                        var t1690 uint8
                        var inline2165 bool = value__81 >= 65
                        var inline2167 bool
                        if inline2165 {
                            var inline2170 bool = value__81 <= 90
                            inline2167 = inline2170
                        } else {
                            inline2167 = false
                        }
                        if inline2167 {
                            var inline2168 uint8 = 97 - 65
                            var inline2169 uint8 = value__81 + inline2168
                            t1690 = inline2169
                            var t1691 uint8 = t1690 - 97
                            var t1692 uint8 = t1691 + 10
                            var t1693 int = int(uint8(t1692))
                            jp1676 = t1693
                            var t1679 bool = jp1676 < base__82
                            if t1679 {
                                var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1676,
                                }
                                return t1680
                            } else {
                                var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1681
                            }
                        } else {
                            t1690 = value__81
                            var t1691 uint8 = t1690 - 97
                            var t1692 uint8 = t1691 + 10
                            var t1693 int = int(uint8(t1692))
                            jp1676 = t1693
                            var t1679 bool = jp1676 < base__82
                            if t1679 {
                                var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1676,
                                }
                                return t1680
                            } else {
                                var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1681
                            }
                        }
                    } else {
                        var t1694 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1694
                    }
                } else {
                    t1697 = value__81
                    var t1698 bool = t1697 <= 102
                    jp1689 = t1698
                    if jp1689 {
                        var t1690 uint8
                        var inline2165 bool = value__81 >= 65
                        var inline2167 bool
                        if inline2165 {
                            var inline2170 bool = value__81 <= 90
                            inline2167 = inline2170
                        } else {
                            inline2167 = false
                        }
                        if inline2167 {
                            var inline2168 uint8 = 97 - 65
                            var inline2169 uint8 = value__81 + inline2168
                            t1690 = inline2169
                            var t1691 uint8 = t1690 - 97
                            var t1692 uint8 = t1691 + 10
                            var t1693 int = int(uint8(t1692))
                            jp1676 = t1693
                            var t1679 bool = jp1676 < base__82
                            if t1679 {
                                var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1676,
                                }
                                return t1680
                            } else {
                                var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1681
                            }
                        } else {
                            t1690 = value__81
                            var t1691 uint8 = t1690 - 97
                            var t1692 uint8 = t1691 + 10
                            var t1693 int = int(uint8(t1692))
                            jp1676 = t1693
                            var t1679 bool = jp1676 < base__82
                            if t1679 {
                                var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1676,
                                }
                                return t1680
                            } else {
                                var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1681
                            }
                        }
                    } else {
                        var t1694 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1694
                    }
                }
            } else {
                jp1689 = false
                if jp1689 {
                    var t1690 uint8
                    var inline2165 bool = value__81 >= 65
                    var inline2167 bool
                    if inline2165 {
                        var inline2170 bool = value__81 <= 90
                        inline2167 = inline2170
                    } else {
                        inline2167 = false
                    }
                    if inline2167 {
                        var inline2168 uint8 = 97 - 65
                        var inline2169 uint8 = value__81 + inline2168
                        t1690 = inline2169
                        var t1691 uint8 = t1690 - 97
                        var t1692 uint8 = t1691 + 10
                        var t1693 int = int(uint8(t1692))
                        jp1676 = t1693
                        var t1679 bool = jp1676 < base__82
                        if t1679 {
                            var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1676,
                            }
                            return t1680
                        } else {
                            var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1681
                        }
                    } else {
                        t1690 = value__81
                        var t1691 uint8 = t1690 - 97
                        var t1692 uint8 = t1691 + 10
                        var t1693 int = int(uint8(t1692))
                        jp1676 = t1693
                        var t1679 bool = jp1676 < base__82
                        if t1679 {
                            var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1676,
                            }
                            return t1680
                        } else {
                            var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1681
                        }
                    }
                } else {
                    var t1694 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1694
                }
            }
        } else {
            t1695 = value__81
            var t1696 bool = t1695 >= 97
            var jp1689 bool
            if t1696 {
                var t1697 uint8
                var inline2158 bool = value__81 >= 65
                var inline2160 bool
                if inline2158 {
                    var inline2163 bool = value__81 <= 90
                    inline2160 = inline2163
                } else {
                    inline2160 = false
                }
                if inline2160 {
                    var inline2161 uint8 = 97 - 65
                    var inline2162 uint8 = value__81 + inline2161
                    t1697 = inline2162
                    var t1698 bool = t1697 <= 102
                    jp1689 = t1698
                    if jp1689 {
                        var t1690 uint8
                        var inline2165 bool = value__81 >= 65
                        var inline2167 bool
                        if inline2165 {
                            var inline2170 bool = value__81 <= 90
                            inline2167 = inline2170
                        } else {
                            inline2167 = false
                        }
                        if inline2167 {
                            var inline2168 uint8 = 97 - 65
                            var inline2169 uint8 = value__81 + inline2168
                            t1690 = inline2169
                            var t1691 uint8 = t1690 - 97
                            var t1692 uint8 = t1691 + 10
                            var t1693 int = int(uint8(t1692))
                            jp1676 = t1693
                            var t1679 bool = jp1676 < base__82
                            if t1679 {
                                var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1676,
                                }
                                return t1680
                            } else {
                                var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1681
                            }
                        } else {
                            t1690 = value__81
                            var t1691 uint8 = t1690 - 97
                            var t1692 uint8 = t1691 + 10
                            var t1693 int = int(uint8(t1692))
                            jp1676 = t1693
                            var t1679 bool = jp1676 < base__82
                            if t1679 {
                                var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1676,
                                }
                                return t1680
                            } else {
                                var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1681
                            }
                        }
                    } else {
                        var t1694 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1694
                    }
                } else {
                    t1697 = value__81
                    var t1698 bool = t1697 <= 102
                    jp1689 = t1698
                    if jp1689 {
                        var t1690 uint8
                        var inline2165 bool = value__81 >= 65
                        var inline2167 bool
                        if inline2165 {
                            var inline2170 bool = value__81 <= 90
                            inline2167 = inline2170
                        } else {
                            inline2167 = false
                        }
                        if inline2167 {
                            var inline2168 uint8 = 97 - 65
                            var inline2169 uint8 = value__81 + inline2168
                            t1690 = inline2169
                            var t1691 uint8 = t1690 - 97
                            var t1692 uint8 = t1691 + 10
                            var t1693 int = int(uint8(t1692))
                            jp1676 = t1693
                            var t1679 bool = jp1676 < base__82
                            if t1679 {
                                var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1676,
                                }
                                return t1680
                            } else {
                                var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1681
                            }
                        } else {
                            t1690 = value__81
                            var t1691 uint8 = t1690 - 97
                            var t1692 uint8 = t1691 + 10
                            var t1693 int = int(uint8(t1692))
                            jp1676 = t1693
                            var t1679 bool = jp1676 < base__82
                            if t1679 {
                                var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1676,
                                }
                                return t1680
                            } else {
                                var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1681
                            }
                        }
                    } else {
                        var t1694 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1694
                    }
                }
            } else {
                jp1689 = false
                if jp1689 {
                    var t1690 uint8
                    var inline2165 bool = value__81 >= 65
                    var inline2167 bool
                    if inline2165 {
                        var inline2170 bool = value__81 <= 90
                        inline2167 = inline2170
                    } else {
                        inline2167 = false
                    }
                    if inline2167 {
                        var inline2168 uint8 = 97 - 65
                        var inline2169 uint8 = value__81 + inline2168
                        t1690 = inline2169
                        var t1691 uint8 = t1690 - 97
                        var t1692 uint8 = t1691 + 10
                        var t1693 int = int(uint8(t1692))
                        jp1676 = t1693
                        var t1679 bool = jp1676 < base__82
                        if t1679 {
                            var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1676,
                            }
                            return t1680
                        } else {
                            var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1681
                        }
                    } else {
                        t1690 = value__81
                        var t1691 uint8 = t1690 - 97
                        var t1692 uint8 = t1691 + 10
                        var t1693 int = int(uint8(t1692))
                        jp1676 = t1693
                        var t1679 bool = jp1676 < base__82
                        if t1679 {
                            var t1680 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1676,
                            }
                            return t1680
                        } else {
                            var t1681 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1681
                        }
                    }
                } else {
                    var t1694 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1694
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1703:
    for {
        var t1704 bool = carry__22 != 0
        if t1704 {
            var t1713 *_goml_vec_uint32 = value__20.words
            var t1714 int
            var inline2182 int = vec_len__Vec_6uint32(t1713)
            t1714 = inline2182
            var t1715 bool = index__23 == t1714
            if t1715 {
                var t1716 *_goml_vec_uint32 = value__20.words
                var inline2179 uint32 = 0
                vec_push__Vec_6uint32(t1716, inline2179)
            } else {}
            var t1706 *_goml_vec_uint32 = value__20.words
            var t1707 uint32 = vec_get__Vec_6uint32(t1706, index__23)
            var t1708 uint64 = uint64(uint32(t1707))
            var sum__24 uint64 = t1708 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1710_rhs int = 32
            var t1710 uint64 = sum__24 >> t1710_rhs
            carry__22 = t1710
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1711 int = compound_old42 + compound_value43
            index__23 = t1711
            continue
        } else {
            break Loop_loop1703
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1720 FloatNatural
    var inline2184 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2185 FloatNatural = FloatNatural{
        words: inline2184,
    }
    t1720 = inline2185
    var t1721 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1720,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1721
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1741 *_goml_vec_uint32 = value__9.words
    var t1742 bool
    var inline2191 int = vec_len__Vec_6uint32(t1741)
    var inline2192 bool = inline2191 == 0
    t1742 = inline2192
    if t1742 {
        return 0
    } else {
        var t1725 *_goml_vec_uint32 = value__9.words
        var t1726 *_goml_vec_uint32 = value__9.words
        var t1727 int
        var inline2189 int = vec_len__Vec_6uint32(t1726)
        t1727 = inline2189
        var t1728 int = t1727 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1725, t1728)
        var bits__11 int = 0
        Loop_loop1735:
        for {
            var t1736 bool = high__10 != 0
            if t1736 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1737 uint32 = compound_old9 >> compound_value10
                high__10 = t1737
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1739 int = compound_old12 + compound_value13
                bits__11 = t1739
                continue
            } else {
                break Loop_loop1735
            }
        }
        var t1730 *_goml_vec_uint32 = value__9.words
        var t1731 int
        var inline2187 int = vec_len__Vec_6uint32(t1730)
        t1731 = inline2187
        var t1732 int = t1731 - 1
        var t1733 int = t1732 * 32
        var t1734 int = t1733 + bits__11
        return t1734
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1764 *_goml_vec_uint32 = left__12.words
    var t1765 int
    var inline2202 int = vec_len__Vec_6uint32(t1764)
    t1765 = inline2202
    var t1766 *_goml_vec_uint32 = right__13.words
    var t1767 int
    var inline2200 int = vec_len__Vec_6uint32(t1766)
    t1767 = inline2200
    var t1768 bool = t1765 < t1767
    if t1768 {
        return -1
    } else {
        var t1770 *_goml_vec_uint32 = left__12.words
        var t1771 int
        var inline2196 int = vec_len__Vec_6uint32(t1770)
        t1771 = inline2196
        var t1772 *_goml_vec_uint32 = right__13.words
        var t1773 int
        var inline2194 int = vec_len__Vec_6uint32(t1772)
        t1773 = inline2194
        var t1774 bool = t1771 > t1773
        if t1774 {
            return 1
        } else {
            var t1746 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2198 int = vec_len__Vec_6uint32(t1746)
            index__14 = inline2198
            Loop_loop1748:
            for {
                var t1749 bool = index__14 > 0
                if t1749 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1750 int = compound_old17 - compound_value18
                    index__14 = t1750
                    var t1753 *_goml_vec_uint32 = left__12.words
                    var t1754 uint32 = vec_get__Vec_6uint32(t1753, index__14)
                    var t1755 *_goml_vec_uint32 = right__13.words
                    var t1756 uint32 = vec_get__Vec_6uint32(t1755, index__14)
                    var t1757 bool = t1754 < t1756
                    if t1757 {
                        return -1
                    } else {
                        var t1759 *_goml_vec_uint32 = left__12.words
                        var t1760 uint32 = vec_get__Vec_6uint32(t1759, index__14)
                        var t1761 *_goml_vec_uint32 = right__13.words
                        var t1762 uint32 = vec_get__Vec_6uint32(t1761, index__14)
                        var t1763 bool = t1760 > t1762
                        if t1763 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1748
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1810 bool = shift__57 >= 0
    var jp1778 FloatNatural
    if t1810 {
        var t1811 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1778 = t1811
    } else {
        var t1812 FloatNatural = float_natural_copy(numerator__55)
        jp1778 = t1812
    }
    var t1806 bool = shift__57 >= 0
    var jp1780 FloatNatural
    if t1806 {
        var t1807 FloatNatural = float_natural_copy(denominator__56)
        jp1780 = t1807
    } else {
        var t1808 int = 0 - shift__57
        var t1809 FloatNatural = float_natural_shift_left(denominator__56, t1808)
        jp1780 = t1809
    }
    var quotient__60 uint64 = 0
    Loop_loop1793:
    for {
        var t1794 int = float_natural_compare(jp1778, jp1780)
        var t1795 bool = t1794 >= 0
        if t1795 {
            var t1796 int = float_natural_bit_length(jp1778)
            var t1797 int = float_natural_bit_length(jp1780)
            var offset__61 int = t1796 - t1797
            var part__62 FloatNatural = float_natural_shift_left(jp1780, offset__61)
            var t1801 int = float_natural_compare(jp1778, part__62)
            var t1802 bool = t1801 < 0
            if t1802 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1803 int = compound_old105 - compound_value106
                offset__61 = t1803
                var t1805 FloatNatural = float_natural_shift_left(jp1780, offset__61)
                part__62 = t1805
            } else {}
            float_natural_subtract(jp1778, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1799 uint64 = compound_old111 | compound_value112
            quotient__60 = t1799
            continue
        } else {
            break Loop_loop1793
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1778, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1780)
    var t1787 bool = rounding__64 > 0
    var jp1784 bool
    if t1787 {
        jp1784 = true
    } else {
        var t1790 bool = rounding__64 == 0
        if t1790 {
            var t1791_rhs uint64 = 1
            var t1791 uint64 = quotient__60 & t1791_rhs
            var t1792 bool = t1791 == 1
            jp1784 = t1792
        } else {
            jp1784 = false
        }
    }
    if jp1784 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1785 uint64 = compound_old115 + compound_value116
        quotient__60 = t1785
    } else {}
    return quotient__60
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(self__531 *_goml_vec_uint32, len__532 int) struct{} {
    vec_truncate__Vec_6uint32(self__531, len__532)
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1828 bool = index__269 < 0
    var jp1820 bool
    if t1828 {
        jp1820 = true
    } else {
        var t1829 int
        var inline2204 int = _goml_runtime_core_string_len(value__268)
        t1829 = inline2204
        var t1830 bool = index__269 > t1829
        jp1820 = t1830
    }
    if jp1820 {
        return false
    } else {
        var t1823 int
        var inline2208 int = _goml_runtime_core_string_len(value__268)
        t1823 = inline2208
        var t1824 bool = index__269 == t1823
        if t1824 {
            return true
        } else {
            var t1825 uint8
            var inline2206 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1825 = inline2206
            var t1826_rhs uint8 = 192
            var t1826 uint8 = t1825 & t1826_rhs
            var t1827 bool = t1826 != 128
            return t1827
        }
    }
}

func float_natural_subtract(value__37 FloatNatural, other__38 FloatNatural) struct{} {
    var base__39 uint64 = 4294967296
    var borrow__40 uint64 = 0
    var index__41 int = 0
    Loop_loop1834:
    for {
        var t1835 *_goml_vec_uint32 = value__37.words
        var t1836 int
        var inline2212 int = vec_len__Vec_6uint32(t1835)
        t1836 = inline2212
        var t1837 bool = index__41 < t1836
        if t1837 {
            var t1851 *_goml_vec_uint32 = other__38.words
            var t1852 int
            var inline2210 int = vec_len__Vec_6uint32(t1851)
            t1852 = inline2210
            var t1853 bool = index__41 < t1852
            var jp1839 uint64
            if t1853 {
                var t1854 *_goml_vec_uint32 = other__38.words
                var t1855 uint32 = vec_get__Vec_6uint32(t1854, index__41)
                var t1856 uint64 = uint64(uint32(t1855))
                jp1839 = t1856
            } else {
                jp1839 = 0
            }
            var right__42 uint64 = jp1839 + borrow__40
            var t1840 *_goml_vec_uint32 = value__37.words
            var t1841 uint32 = vec_get__Vec_6uint32(t1840, index__41)
            var left__43 uint64 = uint64(uint32(t1841))
            var t1845 bool = left__43 >= right__42
            if t1845 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1846 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1846))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1848 uint64 = base__39 + left__43
                var t1849 uint64 = t1848 - right__42
                var value75 uint32 = uint32(uint64(t1849))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1843 int = compound_old79 + compound_value80
            index__41 = t1843
            continue
        } else {
            break Loop_loop1834
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func main() {
    main0()
}
