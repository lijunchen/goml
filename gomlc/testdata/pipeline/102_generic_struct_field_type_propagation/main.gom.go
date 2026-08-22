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

type Pair__u8__f32 struct {
    first uint8
    second float32
}

type Ordering int32

func main0() struct{} {
    var t799 uint8 = 10
    var t800 string
    var inline1827 string = __goml_builtin_uint8_to_string(t799)
    t800 = inline1827
    var inline1824 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t800)
    _goml_runtime_core_string_println(inline1824)
    var t801 float32 = 3.140000104904175
    var t802 string
    var inline1822 string = __goml_builtin_float32_to_string(t801)
    t802 = inline1822
    var inline1819 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t802)
    _goml_runtime_core_string_println(inline1819)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_uint8_to_string(value__228 uint8) string {
    var t816 uint64 = uint64(uint8(value__228))
    var t817 string = decimal_string(t816)
    return t817
}

func __goml_builtin_float32_to_string(value__194 float32) string {
    var t820 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(value__194)
    var t821 uint64 = uint64(uint32(t820))
    var t822 string = format_float_bits(t821, 23, 8, 127)
    return t822
}

func decimal_string(value__208 uint64) string {
    var t845 bool = value__208 == 0
    if t845 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop838:
        for {
            var t839 bool = remaining__210 > 0
            if t839 {
                var t840_rhs uint64 = 10
                var t840 uint64 = remaining__210 % t840_rhs
                var t841 uint8 = uint8(uint64(t840))
                var t842 uint8 = t841 + 48
                vec_push__Vec_5uint8(reversed__209, t842)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t843 uint64 = compound_old353 / compound_value354
                remaining__210 = t843
                continue
            } else {
                break Loop_loop838
            }
        }
        var t827 int
        var inline1845 int = vec_len__Vec_5uint8(reversed__209)
        t827 = inline1845
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t827)
        var offset__212 int = 0
        Loop_loop829:
        for {
            var t830 int
            var inline1843 int = vec_len__Vec_5uint8(reversed__209)
            t830 = inline1843
            var t831 bool = offset__212 < t830
            if t831 {
                var t832 int
                var inline1841 int = vec_len__Vec_5uint8(reversed__209)
                t832 = inline1841
                var t833 int = t832 - offset__212
                var t834 int = t833 - 1
                var t835 uint8 = vec_get__Vec_5uint8(reversed__209, t834)
                vec_push__Vec_5uint8(bytes__211, t835)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t836 int = compound_old358 + compound_value359
                offset__212 = t836
                continue
            } else {
                break Loop_loop829
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t848 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t848
    var t849 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t849 != 0
    var t850_lhs uint64 = 1
    var t850 uint64 = t850_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t850 - 1
    var t851 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t851 & exponent_mask__166
    var t852_lhs uint64 = 1
    var t852 uint64 = t852_lhs << mantissa_bits__161
    var t853 uint64 = t852 - 1
    var fraction__168 uint64 = bits__160 & t853
    var t917 bool = exponent__167 == exponent_mask__166
    if t917 {
        var t919 bool = fraction__168 == 0
        if t919 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t925 bool = exponent__167 == 0
        var jp923 bool
        if t925 {
            var t926 bool = fraction__168 == 0
            jp923 = t926
        } else {
            jp923 = false
        }
        if jp923 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t914 bool = exponent__167 == 0
            var jp856 uint64
            if t914 {
                jp856 = fraction__168
            } else {
                var t915_lhs uint64 = 1
                var t915 uint64 = t915_lhs << mantissa_bits__161
                var t916 uint64 = fraction__168 | t915
                jp856 = t916
            }
            var t908 bool = exponent__167 == 0
            var jp858 int
            if t908 {
                var t909 int = 1 - exponent_bias__163
                var t910 int = t909 - mantissa_bits__161
                jp858 = t910
            } else {
                var t911 int = int(uint64(exponent__167))
                var t912 int = t911 - exponent_bias__163
                var t913 int = t912 - mantissa_bits__161
                jp858 = t913
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp856)
            var t863 bool = jp858 >= 0
            var jp860 int
            if t863 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp858)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t882 bool = mantissa_bits__161 == 23
                var jp865 int
                if t882 {
                    jp865 = 9
                } else {
                    jp865 = 17
                }
                var t879 int
                var inline1853 int = _goml_runtime_core_string_len(digits__173)
                t879 = inline1853
                var t880 bool = t879 < jp865
                var jp867 int
                if t880 {
                    var inline1847 int = _goml_runtime_core_string_len(digits__173)
                    jp867 = inline1847
                } else {
                    jp867 = jp865
                }
                var count__176 int = 1
                Loop_loop870:
                for {
                    var t871 bool = count__176 <= jp867
                    if t871 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t872 int
                        var inline1849 int = _goml_runtime_core_string_len(digits__173)
                        t872 = inline1849
                        var jp874 int
                        if x319 {
                            jp874 = 1
                        } else {
                            jp874 = 0
                        }
                        var point__180 int = t872 + jp874
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t878 bool = x322 == bits__160
                        if t878 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t876 int = compound_old324 + compound_value325
                            count__176 = t876
                            continue
                        }
                    } else {
                        break Loop_loop870
                    }
                }
                var inline1851 int = _goml_runtime_core_string_len(digits__173)
                jp860 = inline1851
                var t861 string = float_natural_decimal(exact_value__171)
                var t862 string = fixed_float_text(t861, jp860, negative__165)
                return t862
            } else {
                var count__183 int = 0
                var t904 int = 0 - jp858
                Loop_loop903:
                for {
                    var t905 bool = count__183 < t904
                    if t905 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t906 int = compound_old329 + compound_value330
                        count__183 = t906
                        continue
                    } else {
                        break Loop_loop903
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t884 int
                var inline1859 int = _goml_runtime_core_string_len(digits__184)
                t884 = inline1859
                var point__185 int = t884 + jp858
                var t902 bool = mantissa_bits__161 == 23
                var jp886 int
                if t902 {
                    jp886 = 9
                } else {
                    jp886 = 17
                }
                var t899 int
                var inline1857 int = _goml_runtime_core_string_len(digits__184)
                t899 = inline1857
                var t900 bool = t899 < jp886
                var jp888 int
                if t900 {
                    var inline1855 int = _goml_runtime_core_string_len(digits__184)
                    jp888 = inline1855
                } else {
                    jp888 = jp886
                }
                count__183 = 1
                Loop_loop890:
                for {
                    var t891 bool = count__183 <= jp888
                    if t891 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp893 int
                        if x336 {
                            jp893 = 1
                        } else {
                            jp893 = 0
                        }
                        var t894 int = point__185 + jp893
                        var candidate__191 string = fixed_float_text(rounded__190, t894, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t898 bool = x339 == bits__160
                        if t898 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t896 int = compound_old341 + compound_value342
                            count__183 = t896
                            continue
                        }
                    } else {
                        break Loop_loop890
                    }
                }
                jp860 = point__185
                var t861 string = float_natural_decimal(exact_value__171)
                var t862 string = fixed_float_text(t861, jp860, negative__165)
                return t862
            }
        }
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline1865 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline1866 FloatNatural = FloatNatural{
        words: inline1865,
    }
    result__2 = inline1866
    var t935 bool = value__1 != 0
    if t935 {
        var t936 *_goml_vec_uint32 = result__2.words
        var t937 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t936, t937)
        var t938_rhs int = 32
        var t938 uint64 = value__1 >> t938_rhs
        var high__3 uint32 = uint32(uint64(t938))
        var t940 bool = high__3 != 0
        if t940 {
            var t941 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t941, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t970 bool
    var inline1883 *_goml_vec_uint32 = value__28.words
    var inline1884 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1883)
    t970 = inline1884
    if t970 {
        var inline1868 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline1869 FloatNatural = FloatNatural{
            words: inline1868,
        }
        return inline1869
    } else {
        var t973 bool = bits__29 == 0
        if t973 {
            var t974 FloatNatural = float_natural_copy(value__28)
            return t974
        } else {
            var result__30 FloatNatural
            var inline1880 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline1881 FloatNatural = FloatNatural{
                words: inline1880,
            }
            result__30 = inline1881
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop965:
            for {
                var t966 bool = index__33 < word_shift__31
                if t966 {
                    var t967 *_goml_vec_uint32 = result__30.words
                    var inline1871 uint32 = 0
                    vec_push__Vec_6uint32(t967, inline1871)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t968 int = compound_old52 + compound_value53
                    index__33 = t968
                    continue
                } else {
                    break Loop_loop965
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop953:
            for {
                var t954 *_goml_vec_uint32 = value__28.words
                var t955 int
                var inline1876 int = vec_len__Vec_6uint32(t954)
                t955 = inline1876
                var t956 bool = index__33 < t955
                if t956 {
                    var t957 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t957, index__33)
                    var t958 uint64 = uint64(uint32(word__35))
                    var t959 uint64 = t958 << bit_shift__32
                    var shifted__36 uint64 = t959 | carry__34
                    var t960 *_goml_vec_uint32 = result__30.words
                    var t961 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t960, t961)
                    var t962_rhs int = 32
                    var t962 uint64 = shifted__36 >> t962_rhs
                    carry__34 = t962
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t963 int = compound_old59 + compound_value60
                    index__33 = t963
                    continue
                } else {
                    break Loop_loop953
                }
            }
            var t949 bool = carry__34 != 0
            if t949 {
                var t950 *_goml_vec_uint32 = result__30.words
                var t951 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t950, t951)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t997 bool
    var inline1899 *_goml_vec_uint32 = value__49.words
    var inline1900 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1899)
    t997 = inline1900
    if t997 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop990:
        for {
            var t991 bool
            var inline1888 *_goml_vec_uint32 = current__50.words
            var inline1889 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1888)
            t991 = inline1889
            var t992 bool = !t991
            if t992 {
                var t993 uint32 = float_natural_divide_small(current__50, 10)
                var t994 uint8 = uint8(uint32(t993))
                var t995 uint8 = t994 + 48
                vec_push__Vec_5uint8(reversed__51, t995)
                continue
            } else {
                break Loop_loop990
            }
        }
        var t979 int
        var inline1897 int = vec_len__Vec_5uint8(reversed__51)
        t979 = inline1897
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t979)
        var offset__53 int = 0
        Loop_loop981:
        for {
            var t982 int
            var inline1895 int = vec_len__Vec_5uint8(reversed__51)
            t982 = inline1895
            var t983 bool = offset__53 < t982
            if t983 {
                var t984 int
                var inline1893 int = vec_len__Vec_5uint8(reversed__51)
                t984 = inline1893
                var t985 int = t984 - offset__53
                var t986 int = t985 - 1
                var t987 uint8 = vec_get__Vec_5uint8(reversed__51, t986)
                vec_push__Vec_5uint8(output__52, t987)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t988 int = compound_old98 + compound_value99
                offset__53 = t988
                continue
            } else {
                break Loop_loop981
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1000 int = _goml_runtime_core_string_len(self__289)
    return t1000
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t1003 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1003)
    var index__148 int = 0
    Loop_loop1058:
    for {
        var t1059 bool = index__148 < count__146
        if t1059 {
            var t1060 uint8
            var inline1904 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1060 = inline1904
            vec_push__Vec_5uint8(output__147, t1060)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1061 int = compound_old267 + compound_value268
            index__148 = t1061
            continue
        } else {
            break Loop_loop1058
        }
    }
    var t1055 int
    var inline1925 int = _goml_runtime_core_string_len(exact__145)
    t1055 = inline1925
    var t1056 bool = count__146 == t1055
    if t1056 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1057 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1057
    } else {
        var next__150 uint8
        var inline1923 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline1923
        var trailing__151 bool = false
        var t1006 int = count__146 + 1
        index__148 = t1006
        Loop_loop1047:
        for {
            var t1048 int
            var inline1908 int = _goml_runtime_core_string_len(exact__145)
            t1048 = inline1908
            var t1049 bool = index__148 < t1048
            if t1049 {
                var t1053 uint8
                var inline1906 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1053 = inline1906
                var t1054 bool = t1053 != 48
                if t1054 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1051 int = compound_old278 + compound_value279
                index__148 = t1051
                continue
            } else {
                break Loop_loop1047
            }
        }
        var t1035 bool = next__150 > 53
        var jp1009 bool
        if t1035 {
            jp1009 = true
        } else {
            var t1038 bool = next__150 == 53
            if t1038 {
                if trailing__151 {
                    jp1009 = true
                } else {
                    var t1041 int
                    var inline1910 int = vec_len__Vec_5uint8(output__147)
                    t1041 = inline1910
                    var t1042 int = t1041 - 1
                    var t1043 uint8 = vec_get__Vec_5uint8(output__147, t1042)
                    var t1044 uint8 = t1043 - 48
                    var t1045_rhs uint8 = 2
                    var t1045 uint8 = t1044 % t1045_rhs
                    var t1046 bool = t1045 == 1
                    jp1009 = t1046
                }
            } else {
                jp1009 = false
            }
        }
        if jp1009 {
            var index__153 int
            var inline1921 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline1921
            Loop_loop1023:
            for {
                var t1024 bool = index__153 > 0
                if t1024 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1025 int = compound_old282 - compound_value283
                    index__153 = t1025
                    var t1028 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1029 bool = t1028 < 57
                    if t1029 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1030 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1030)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1032 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1032
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1023
                }
            }
            var t1013 int
            var inline1919 int = vec_len__Vec_5uint8(output__147)
            t1013 = inline1919
            var t1014 int = t1013 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1014)
            var inline1916 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline1916)
            index__153 = 0
            Loop_loop1017:
            for {
                var t1018 int
                var inline1914 int = vec_len__Vec_5uint8(output__147)
                t1018 = inline1914
                var t1019 bool = index__153 < t1018
                if t1019 {
                    var t1020 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t1020)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t1021 int = compound_old302 + compound_value303
                    index__153 = t1021
                    continue
                } else {
                    break Loop_loop1017
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t1016 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t1016
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1034 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1034
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline1932 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline1932
    Loop_loop1067:
    for {
        var t1072 bool = length__159 > 1
        var jp1069 bool
        if t1072 {
            var t1073 int = length__159 - 1
            var t1074 uint8
            var inline1927 uint8 = _goml_runtime_core_string_byte_get(value__158, t1073)
            t1074 = inline1927
            var t1075 bool = t1074 == 48
            jp1069 = t1075
        } else {
            jp1069 = false
        }
        if jp1069 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1070 int = compound_old312 - compound_value313
            length__159 = t1070
            continue
        } else {
            break Loop_loop1067
        }
    }
    var inline1929 int = 0
    var inline1930 string = string_byte_slice(value__158, inline1929, length__159)
    return inline1930
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline1934 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline1934)
    } else {}
    var t1080 bool = decimal_point__138 <= 0
    if t1080 {
        var inline1949 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline1949)
        var inline1946 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline1946)
        var index__141 int = 0
        var t1090 int = 0 - decimal_point__138
        Loop_loop1089:
        for {
            var t1091 bool = index__141 < t1090
            if t1091 {
                var inline1937 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline1937)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1092 int = compound_old234 + compound_value235
                index__141 = t1092
                continue
            } else {
                break Loop_loop1089
            }
        }
        index__141 = 0
        Loop_loop1083:
        for {
            var t1084 int
            var inline1944 int = _goml_runtime_core_string_len(digits__137)
            t1084 = inline1944
            var t1085 bool = index__141 < t1084
            if t1085 {
                var t1086 uint8
                var inline1942 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1086 = inline1942
                vec_push__Vec_5uint8(bytes__140, t1086)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1087 int = compound_old240 + compound_value241
                index__141 = t1087
                continue
            } else {
                break Loop_loop1083
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1095 int
        var inline1974 int = _goml_runtime_core_string_len(digits__137)
        t1095 = inline1974
        var t1096 bool = decimal_point__138 >= t1095
        if t1096 {
            var index__142 int = 0
            Loop_loop1103:
            for {
                var t1104 int
                var inline1956 int = _goml_runtime_core_string_len(digits__137)
                t1104 = inline1956
                var t1105 bool = index__142 < t1104
                if t1105 {
                    var t1106 uint8
                    var inline1954 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1106 = inline1954
                    vec_push__Vec_5uint8(bytes__140, t1106)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1107 int = compound_old244 + compound_value245
                    index__142 = t1107
                    continue
                } else {
                    break Loop_loop1103
                }
            }
            Loop_loop1099:
            for {
                var t1100 bool = index__142 < decimal_point__138
                if t1100 {
                    var inline1958 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline1958)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1101 int = compound_old249 + compound_value250
                    index__142 = t1101
                    continue
                } else {
                    break Loop_loop1099
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1117:
            for {
                var t1118 bool = index__143 < decimal_point__138
                if t1118 {
                    var t1119 uint8
                    var inline1963 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1119 = inline1963
                    vec_push__Vec_5uint8(bytes__140, t1119)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1120 int = compound_old253 + compound_value254
                    index__143 = t1120
                    continue
                } else {
                    break Loop_loop1117
                }
            }
            var inline1971 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline1971)
            Loop_loop1111:
            for {
                var t1112 int
                var inline1969 int = _goml_runtime_core_string_len(digits__137)
                t1112 = inline1969
                var t1113 bool = index__143 < t1112
                if t1113 {
                    var t1114 uint8
                    var inline1967 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1114 = inline1967
                    vec_push__Vec_5uint8(bytes__140, t1114)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1115 int = compound_old259 + compound_value260
                    index__143 = t1115
                    continue
                } else {
                    break Loop_loop1111
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
    var t1216 bool = parsed__110.valid
    var t1217 bool = !t1216
    if t1217 {
        var t1218 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t1218
    } else {
        var t1210 bool = parsed__110.negative
        var jp1127 uint64
        if t1210 {
            var t1215 bool = mantissa_bits__108 == 23
            var jp1212 int
            if t1215 {
                jp1212 = 8
            } else {
                jp1212 = 11
            }
            var t1213 int = mantissa_bits__108 + jp1212
            var t1214_lhs uint64 = 1
            var t1214 uint64 = t1214_lhs << t1213
            jp1127 = t1214
        } else {
            jp1127 = 0
        }
        var t1209 bool = mantissa_bits__108 == 23
        var jp1129 int
        if t1209 {
            jp1129 = 8
        } else {
            jp1129 = 11
        }
        var t1130_lhs uint64 = 1
        var t1130 uint64 = t1130_lhs << jp1129
        var t1131 uint64 = t1130 - 1
        var exponent_mask__112 uint64 = t1131 << mantissa_bits__108
        var t1187 int = parsed__110.special
        var t1188 bool = t1187 == 1
        if t1188 {
            var t1189 uint64 = jp1127 | exponent_mask__112
            var t1190 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t1189,
            }
            return t1190
        } else {
            var t1192 int = parsed__110.special
            var t1193 bool = t1192 == 2
            if t1193 {
                var t1197 int = mantissa_bits__108 - 1
                var t1198_lhs uint64 = 1
                var t1198 uint64 = t1198_lhs << t1197
                var t1199 uint64 = exponent_mask__112 | t1198
                var t1204 bool = mantissa_bits__108 == 52
                var jp1201 uint64
                if t1204 {
                    jp1201 = 1
                } else {
                    jp1201 = 0
                }
                var t1202 uint64 = t1199 | jp1201
                var t1203 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t1202,
                }
                return t1203
            } else {
                var t1206 FloatNatural = parsed__110.numerator
                var t1207 bool
                var inline1976 *_goml_vec_uint32 = t1206.words
                var inline1977 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1976)
                t1207 = inline1977
                if t1207 {
                    var t1208 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp1127,
                    }
                    return t1208
                } else {
                    var t1170 bool = parsed__110.hexadecimal
                    var t1171 bool = !t1170
                    if t1171 {
                        var t1172 int = parsed__110.significant_digits
                        var t1173 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t1172 + t1173
                        var t1186 bool = mantissa_bits__108 == 23
                        var jp1175 int
                        if t1186 {
                            jp1175 = 40
                        } else {
                            jp1175 = 310
                        }
                        var t1185 bool = mantissa_bits__108 == 23
                        var jp1177 int
                        if t1185 {
                            jp1177 = -46
                        } else {
                            jp1177 = -325
                        }
                        var t1179 bool = decimal_position__113 > jp1175
                        if t1179 {
                            var t1180 uint64 = jp1127 | exponent_mask__112
                            var t1181 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t1180,
                            }
                            return t1181
                        } else {
                            var t1183 bool = decimal_position__113 < jp1177
                            if t1183 {
                                var t1184 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp1127,
                                }
                                return t1184
                            } else {
                                var t1166 bool = parsed__110.hexadecimal
                                var t1167 bool = !t1166
                                var jp1161 bool
                                if t1167 {
                                    var t1168 int = parsed__110.decimal_exponent
                                    var t1169 bool = t1168 < 0
                                    jp1161 = t1169
                                } else {
                                    jp1161 = false
                                }
                                var jp1135 FloatNatural
                                if jp1161 {
                                    var t1162 int = parsed__110.decimal_exponent
                                    var t1163 int = 0 - t1162
                                    var t1164 FloatNatural = float_natural_power5(t1163)
                                    jp1135 = t1164
                                } else {
                                    var inline1979 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline1979, 1)
                                    var inline1981 FloatNatural = FloatNatural{
                                        words: inline1979,
                                    }
                                    jp1135 = inline1981
                                }
                                var t1156 bool = parsed__110.hexadecimal
                                var t1157 bool = !t1156
                                var jp1147 bool
                                if t1157 {
                                    var t1158 int = parsed__110.decimal_exponent
                                    var t1159 bool = t1158 > 0
                                    jp1147 = t1159
                                } else {
                                    jp1147 = false
                                }
                                var jp1137 FloatNatural
                                if jp1147 {
                                    var t1148 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t1148)
                                    var count__118 int = 0
                                    Loop_loop1150:
                                    for {
                                        var t1151 int = parsed__110.decimal_exponent
                                        var t1152 bool = count__118 < t1151
                                        if t1152 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t1153 int = compound_old213 + compound_value214
                                            count__118 = t1153
                                            continue
                                        } else {
                                            break Loop_loop1150
                                        }
                                    }
                                    jp1137 = result__117
                                    var t1143 bool = parsed__110.hexadecimal
                                    var jp1139 int
                                    if t1143 {
                                        var t1144 int = parsed__110.binary_exponent
                                        jp1139 = t1144
                                    } else {
                                        var t1145 int = parsed__110.decimal_exponent
                                        jp1139 = t1145
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1137, jp1135, jp1139, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1140 bool = !x219
                                    var t1141 uint64 = jp1127 | x218
                                    var t1142 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1140,
                                        _1: t1141,
                                    }
                                    return t1142
                                } else {
                                    var t1155 FloatNatural = parsed__110.numerator
                                    jp1137 = t1155
                                    var t1143 bool = parsed__110.hexadecimal
                                    var jp1139 int
                                    if t1143 {
                                        var t1144 int = parsed__110.binary_exponent
                                        jp1139 = t1144
                                    } else {
                                        var t1145 int = parsed__110.decimal_exponent
                                        jp1139 = t1145
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1137, jp1135, jp1139, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1140 bool = !x219
                                    var t1141 uint64 = jp1127 | x218
                                    var t1142 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1140,
                                        _1: t1141,
                                    }
                                    return t1142
                                }
                            }
                        }
                    } else {
                        var t1166 bool = parsed__110.hexadecimal
                        var t1167 bool = !t1166
                        var jp1161 bool
                        if t1167 {
                            var t1168 int = parsed__110.decimal_exponent
                            var t1169 bool = t1168 < 0
                            jp1161 = t1169
                        } else {
                            jp1161 = false
                        }
                        var jp1135 FloatNatural
                        if jp1161 {
                            var t1162 int = parsed__110.decimal_exponent
                            var t1163 int = 0 - t1162
                            var t1164 FloatNatural = float_natural_power5(t1163)
                            jp1135 = t1164
                        } else {
                            var inline1979 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline1979, 1)
                            var inline1981 FloatNatural = FloatNatural{
                                words: inline1979,
                            }
                            jp1135 = inline1981
                        }
                        var t1156 bool = parsed__110.hexadecimal
                        var t1157 bool = !t1156
                        var jp1147 bool
                        if t1157 {
                            var t1158 int = parsed__110.decimal_exponent
                            var t1159 bool = t1158 > 0
                            jp1147 = t1159
                        } else {
                            jp1147 = false
                        }
                        var jp1137 FloatNatural
                        if jp1147 {
                            var t1148 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t1148)
                            var count__118 int = 0
                            Loop_loop1150__2:
                            for {
                                var t1151 int = parsed__110.decimal_exponent
                                var t1152 bool = count__118 < t1151
                                if t1152 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t1153 int = compound_old213 + compound_value214
                                    count__118 = t1153
                                    continue
                                } else {
                                    break Loop_loop1150__2
                                }
                            }
                            jp1137 = result__117
                            var t1143 bool = parsed__110.hexadecimal
                            var jp1139 int
                            if t1143 {
                                var t1144 int = parsed__110.binary_exponent
                                jp1139 = t1144
                            } else {
                                var t1145 int = parsed__110.decimal_exponent
                                jp1139 = t1145
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1137, jp1135, jp1139, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1140 bool = !x219
                            var t1141 uint64 = jp1127 | x218
                            var t1142 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1140,
                                _1: t1141,
                            }
                            return t1142
                        } else {
                            var t1155 FloatNatural = parsed__110.numerator
                            jp1137 = t1155
                            var t1143 bool = parsed__110.hexadecimal
                            var jp1139 int
                            if t1143 {
                                var t1144 int = parsed__110.binary_exponent
                                jp1139 = t1144
                            } else {
                                var t1145 int = parsed__110.decimal_exponent
                                jp1139 = t1145
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1137, jp1135, jp1139, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1140 bool = !x219
                            var t1141 uint64 = jp1127 | x218
                            var t1142 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1140,
                                _1: t1141,
                            }
                            return t1142
                        }
                    }
                }
            }
        }
    }
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1240 bool = factor__16 == 0
    if t1240 {
        var t1241 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1241, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1234 uint64 = uint64(uint32(factor__16))
        Loop_loop1227:
        for {
            var t1228 *_goml_vec_uint32 = value__15.words
            var t1229 int
            var inline1985 int = vec_len__Vec_6uint32(t1228)
            t1229 = inline1985
            var t1230 bool = index__18 < t1229
            if t1230 {
                var t1231 *_goml_vec_uint32 = value__15.words
                var t1232 uint32 = vec_get__Vec_6uint32(t1231, index__18)
                var t1233 uint64 = uint64(uint32(t1232))
                var t1235 uint64 = t1233 * t1234
                var product__19 uint64 = t1235 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1237_rhs int = 32
                var t1237 uint64 = product__19 >> t1237_rhs
                carry__17 = t1237
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1238 int = compound_old30 + compound_value31
                index__18 = t1238
                continue
            } else {
                break Loop_loop1227
            }
        }
        var t1223 bool = carry__17 != 0
        if t1223 {
            var t1224 *_goml_vec_uint32 = value__15.words
            var t1225 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1224, t1225)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_natural_zero() FloatNatural {
    var t1244 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1245 FloatNatural = FloatNatural{
        words: t1244,
    }
    return t1245
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline1996 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline1997 FloatNatural = FloatNatural{
        words: inline1996,
    }
    result__5 = inline1997
    var index__6 int = 0
    Loop_loop1255:
    for {
        var t1256 *_goml_vec_uint32 = value__4.words
        var t1257 int
        var inline1994 int = vec_len__Vec_6uint32(t1256)
        t1257 = inline1994
        var t1258 bool = index__6 < t1257
        if t1258 {
            var t1259 *_goml_vec_uint32 = result__5.words
            var t1260 *_goml_vec_uint32 = value__4.words
            var t1261 uint32 = vec_get__Vec_6uint32(t1260, index__6)
            vec_push__Vec_6uint32(t1259, t1261)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1262 int = compound_old4 + compound_value5
            index__6 = t1262
            continue
        } else {
            break Loop_loop1255
        }
    }
    return result__5
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1269 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline1999 int = vec_len__Vec_6uint32(t1269)
    index__47 = inline1999
    var t1280 uint64 = uint64(uint32(divisor__45))
    var t1283 uint64 = uint64(uint32(divisor__45))
    Loop_loop1272:
    for {
        var t1273 bool = index__47 > 0
        if t1273 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1274 int = compound_old83 - compound_value84
            index__47 = t1274
            var t1276_rhs int = 32
            var t1276 uint64 = remainder__46 << t1276_rhs
            var t1277 *_goml_vec_uint32 = value__44.words
            var t1278 uint32 = vec_get__Vec_6uint32(t1277, index__47)
            var t1279 uint64 = uint64(uint32(t1278))
            var current__48 uint64 = t1276 | t1279
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1281 uint64 = current__48 / t1280
            var value90 uint32 = uint32(uint64(t1281))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1284 uint64 = current__48 % t1283
            remainder__46 = t1284
            continue
        } else {
            break Loop_loop1272
        }
    }
    float_natural_trim(value__44)
    var t1271 uint32 = uint32(uint64(remainder__46))
    return t1271
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1287 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1287
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline2001 bool = string_is_char_boundary(self__294, start__295)
    var inline2003 bool
    if inline2001 {
        var inline2006 bool = string_is_char_boundary(self__294, end__296)
        inline2003 = inline2006
    } else {
        inline2003 = false
    }
    if inline2003 {
        var inline2004 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline2004
    } else {
        var inline2005 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2005
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1475 bool = string_equals_ascii_case(value__84, "nan")
    if t1475 {
        var t1476 FloatNatural
        var inline2008 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2009 FloatNatural = FloatNatural{
            words: inline2008,
        }
        t1476 = inline2009
        var t1477 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1476,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1477
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1467 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1468 bool = index__85 < t1467
        var jp1462 bool
        if t1468 {
            var t1471 uint8
            var inline2013 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1471 = inline2013
            var t1472 bool = t1471 == 43
            if t1472 {
                jp1462 = true
            } else {
                var t1473 uint8
                var inline2011 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1473 = inline2011
                var t1474 bool = t1473 == 45
                jp1462 = t1474
            }
        } else {
            jp1462 = false
        }
        if jp1462 {
            var t1463 uint8
            var inline2015 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1463 = inline2015
            var t1464 bool = t1463 == 45
            negative__86 = t1464
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1465 int = compound_old140 + compound_value141
            index__85 = t1465
        } else {}
        var t1295 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1295)
        var t1459 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1456 bool
        if t1459 {
            jp1456 = true
        } else {
            var t1460 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1456 = t1460
        }
        if jp1456 {
            var t1457 FloatNatural
            var inline2017 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2018 FloatNatural = FloatNatural{
                words: inline2017,
            }
            t1457 = inline2018
            var t1458 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1457,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1458
        } else {
            var t1450 int = index__85 + 2
            var t1451 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1452 bool = t1450 <= t1451
            var jp1445 bool
            if t1452 {
                var t1453 uint8
                var inline2020 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1453 = inline2020
                var t1454 bool = t1453 == 48
                jp1445 = t1454
            } else {
                jp1445 = false
            }
            var jp1298 bool
            if jp1445 {
                var t1446 int = index__85 + 1
                var t1447 uint8
                var inline2029 uint8 = _goml_runtime_core_string_byte_get(value__84, t1446)
                t1447 = inline2029
                var t1448 uint8
                var inline2022 bool = t1447 >= 65
                var inline2024 bool
                if inline2022 {
                    var inline2027 bool = t1447 <= 90
                    inline2024 = inline2027
                } else {
                    inline2024 = false
                }
                if inline2024 {
                    var inline2025 uint8 = 97 - 65
                    var inline2026 uint8 = t1447 + inline2025
                    t1448 = inline2026
                    var t1449 bool = t1448 == 120
                    jp1298 = t1449
                    if jp1298 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1442 int = compound_old145 + compound_value146
                        index__85 = t1442
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1301 int
                    if jp1298 {
                        jp1301 = 16
                    } else {
                        jp1301 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1395 uint32 = uint32(int(jp1301))
                    Loop_loop1391:
                    for {
                        var t1392 int
                        var inline2043 int = _goml_runtime_core_string_len(value__84)
                        t1392 = inline2043
                        var t1393 bool = index__85 < t1392
                        if t1393 {
                            var current__97 uint8
                            var inline2041 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2041
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1301)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1395)
                                var t1396 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1396)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1407 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1407
                                } else {}
                                var t1405 bool = significant_digits__95 > 0
                                var jp1402 bool
                                if t1405 {
                                    jp1402 = true
                                } else {
                                    var t1406 bool = x151 != 0
                                    jp1402 = t1406
                                }
                                if jp1402 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1403 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1403
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1399 int = compound_old164 + compound_value165
                                index__85 = t1399
                                continue
                            } else {
                                var t1410 bool = current__97 == 95
                                if t1410 {
                                    var t1431 int = index__85 + 1
                                    var t1432 int
                                    var inline2039 int = _goml_runtime_core_string_len(value__84)
                                    t1432 = inline2039
                                    var t1433 bool = t1431 >= t1432
                                    if t1433 {
                                        var inline2031 FloatNatural = float_natural_zero()
                                        var inline2032 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2031,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2032
                                    } else {
                                        var t1412 int = index__85 + 1
                                        var t1413 uint8
                                        var inline2037 uint8 = _goml_runtime_core_string_byte_get(value__84, t1412)
                                        t1413 = inline2037
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1413, jp1301)
                                        var x169 bool = mtmp168._0
                                        var jp1428 bool
                                        if jp1298 {
                                            var t1430 bool = !saw_digit__92
                                            jp1428 = t1430
                                        } else {
                                            jp1428 = false
                                        }
                                        var jp1415 bool
                                        if jp1428 {
                                            var t1429 bool = index__85 == mantissa_start__89
                                            jp1415 = t1429
                                        } else {
                                            jp1415 = false
                                        }
                                        var t1425 bool = !previous_digit__96
                                        var jp1423 bool
                                        if t1425 {
                                            var t1426 bool = !jp1415
                                            jp1423 = t1426
                                        } else {
                                            jp1423 = false
                                        }
                                        var jp1420 bool
                                        if jp1423 {
                                            jp1420 = true
                                        } else {
                                            var t1424 bool = !x169
                                            jp1420 = t1424
                                        }
                                        if jp1420 {
                                            var inline2034 FloatNatural = float_natural_zero()
                                            var inline2035 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2034,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2035
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1417 int = compound_old173 + compound_value174
                                            index__85 = t1417
                                            continue
                                        }
                                    }
                                } else {
                                    var t1440 bool = current__97 == 46
                                    var jp1437 bool
                                    if t1440 {
                                        var t1441 bool = !saw_dot__93
                                        jp1437 = t1441
                                    } else {
                                        jp1437 = false
                                    }
                                    if jp1437 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1438 int = compound_old178 + compound_value179
                                        index__85 = t1438
                                        continue
                                    } else {
                                        break Loop_loop1391
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1391
                        }
                    }
                    var t1389 bool = !saw_digit__92
                    if t1389 {
                        var inline2045 FloatNatural = float_natural_zero()
                        var inline2046 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2045,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2046
                    } else {
                        var jp1305 uint8
                        if jp1298 {
                            jp1305 = 112
                        } else {
                            jp1305 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1384 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1385 bool = index__85 < t1384
                        var jp1322 bool
                        if t1385 {
                            var t1386 uint8
                            var inline2048 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1386 = inline2048
                            var t1387 uint8 = ascii_lower(t1386)
                            var t1388 bool = t1387 == jp1305
                            jp1322 = t1388
                        } else {
                            jp1322 = false
                        }
                        if jp1322 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1323 int = compound_old183 + compound_value184
                            index__85 = t1323
                            var t1374 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1375 bool = index__85 < t1374
                            var jp1369 bool
                            if t1375 {
                                var t1378 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1379 bool = t1378 == 43
                                if t1379 {
                                    jp1369 = true
                                } else {
                                    var t1380 uint8
                                    var inline2050 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1380 = inline2050
                                    var t1381 bool = t1380 == 45
                                    jp1369 = t1381
                                }
                            } else {
                                jp1369 = false
                            }
                            if jp1369 {
                                var t1370 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1371 bool = t1370 == 45
                                exponent_negative__104 = t1371
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1372 int = compound_old187 + compound_value188
                                index__85 = t1372
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1330:
                            for {
                                var t1331 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1332 bool = index__85 < t1331
                                if t1332 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1366 bool = current__106 >= 48
                                    var jp1335 bool
                                    if t1366 {
                                        var t1367 bool = current__106 <= 57
                                        jp1335 = t1367
                                    } else {
                                        jp1335 = false
                                    }
                                    if jp1335 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1339 bool = exponent__103 < 1000000
                                        if t1339 {
                                            var t1340 int = exponent__103 * 10
                                            var t1341 uint8 = current__106 - 48
                                            var t1342 int = int(uint8(t1341))
                                            var t1343 int = t1340 + t1342
                                            exponent__103 = t1343
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1337 int = compound_old196 + compound_value197
                                        index__85 = t1337
                                        continue
                                    } else {
                                        var t1345 bool = current__106 == 95
                                        if t1345 {
                                            var t1362 bool = !previous_digit__96
                                            var jp1358 bool
                                            if t1362 {
                                                jp1358 = true
                                            } else {
                                                var t1363 int = index__85 + 1
                                                var t1364 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1365 bool = t1363 >= t1364
                                                jp1358 = t1365
                                            }
                                            var jp1353 bool
                                            if jp1358 {
                                                jp1353 = true
                                            } else {
                                                var t1359 int = index__85 + 1
                                                var t1360 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1359)
                                                var t1361 bool = t1360 < 48
                                                jp1353 = t1361
                                            }
                                            var jp1350 bool
                                            if jp1353 {
                                                jp1350 = true
                                            } else {
                                                var t1354 int = index__85 + 1
                                                var t1355 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1354)
                                                var t1356 bool = t1355 > 57
                                                jp1350 = t1356
                                            }
                                            if jp1350 {
                                                var t1351 ParsedFloat = invalid_parsed_float()
                                                return t1351
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1347 int = compound_old201 + compound_value202
                                                index__85 = t1347
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1330
                                        }
                                    }
                                } else {
                                    break Loop_loop1330
                                }
                            }
                            var t1328 bool = !exponent_digits__105
                            if t1328 {
                                var t1329 ParsedFloat = invalid_parsed_float()
                                return t1329
                            } else {
                                var t1318 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1319 bool = index__85 != t1318
                                if t1319 {
                                    var t1320 ParsedFloat = invalid_parsed_float()
                                    return t1320
                                } else {
                                    if exponent_negative__104 {
                                        var t1317 int = 0 - exponent__103
                                        exponent__103 = t1317
                                    } else {}
                                    var jp1310 int
                                    if jp1298 {
                                        jp1310 = 0
                                    } else {
                                        var t1316 int = exponent__103 - fraction_digits__94
                                        jp1310 = t1316
                                    }
                                    var jp1312 int
                                    if jp1298 {
                                        var t1314 int = fraction_digits__94 * 4
                                        var t1315 int = exponent__103 - t1314
                                        jp1312 = t1315
                                    } else {
                                        jp1312 = 0
                                    }
                                    var t1313 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1310,
                                        binary_exponent: jp1312,
                                        hexadecimal: jp1298,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1313
                                }
                            }
                        } else {
                            if jp1298 {
                                var t1383 ParsedFloat = invalid_parsed_float()
                                return t1383
                            } else {
                                var t1318 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1319 bool = index__85 != t1318
                                if t1319 {
                                    var t1320 ParsedFloat = invalid_parsed_float()
                                    return t1320
                                } else {
                                    if exponent_negative__104 {
                                        var t1317 int = 0 - exponent__103
                                        exponent__103 = t1317
                                    } else {}
                                    var jp1310 int
                                    if jp1298 {
                                        jp1310 = 0
                                    } else {
                                        var t1316 int = exponent__103 - fraction_digits__94
                                        jp1310 = t1316
                                    }
                                    var jp1312 int
                                    if jp1298 {
                                        var t1314 int = fraction_digits__94 * 4
                                        var t1315 int = exponent__103 - t1314
                                        jp1312 = t1315
                                    } else {
                                        jp1312 = 0
                                    }
                                    var t1313 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1310,
                                        binary_exponent: jp1312,
                                        hexadecimal: jp1298,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1313
                                }
                            }
                        }
                    }
                } else {
                    t1448 = t1447
                    var t1449 bool = t1448 == 120
                    jp1298 = t1449
                    if jp1298 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1442 int = compound_old145 + compound_value146
                        index__85 = t1442
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1301 int
                    if jp1298 {
                        jp1301 = 16
                    } else {
                        jp1301 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1395 uint32 = uint32(int(jp1301))
                    Loop_loop1391__2:
                    for {
                        var t1392 int
                        var inline2043 int = _goml_runtime_core_string_len(value__84)
                        t1392 = inline2043
                        var t1393 bool = index__85 < t1392
                        if t1393 {
                            var current__97 uint8
                            var inline2041 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2041
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1301)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1395)
                                var t1396 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1396)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1407 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1407
                                } else {}
                                var t1405 bool = significant_digits__95 > 0
                                var jp1402 bool
                                if t1405 {
                                    jp1402 = true
                                } else {
                                    var t1406 bool = x151 != 0
                                    jp1402 = t1406
                                }
                                if jp1402 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1403 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1403
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1399 int = compound_old164 + compound_value165
                                index__85 = t1399
                                continue
                            } else {
                                var t1410 bool = current__97 == 95
                                if t1410 {
                                    var t1431 int = index__85 + 1
                                    var t1432 int
                                    var inline2039 int = _goml_runtime_core_string_len(value__84)
                                    t1432 = inline2039
                                    var t1433 bool = t1431 >= t1432
                                    if t1433 {
                                        var inline2031 FloatNatural = float_natural_zero()
                                        var inline2032 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2031,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2032
                                    } else {
                                        var t1412 int = index__85 + 1
                                        var t1413 uint8
                                        var inline2037 uint8 = _goml_runtime_core_string_byte_get(value__84, t1412)
                                        t1413 = inline2037
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1413, jp1301)
                                        var x169 bool = mtmp168._0
                                        var jp1428 bool
                                        if jp1298 {
                                            var t1430 bool = !saw_digit__92
                                            jp1428 = t1430
                                        } else {
                                            jp1428 = false
                                        }
                                        var jp1415 bool
                                        if jp1428 {
                                            var t1429 bool = index__85 == mantissa_start__89
                                            jp1415 = t1429
                                        } else {
                                            jp1415 = false
                                        }
                                        var t1425 bool = !previous_digit__96
                                        var jp1423 bool
                                        if t1425 {
                                            var t1426 bool = !jp1415
                                            jp1423 = t1426
                                        } else {
                                            jp1423 = false
                                        }
                                        var jp1420 bool
                                        if jp1423 {
                                            jp1420 = true
                                        } else {
                                            var t1424 bool = !x169
                                            jp1420 = t1424
                                        }
                                        if jp1420 {
                                            var inline2034 FloatNatural = float_natural_zero()
                                            var inline2035 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2034,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2035
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1417 int = compound_old173 + compound_value174
                                            index__85 = t1417
                                            continue
                                        }
                                    }
                                } else {
                                    var t1440 bool = current__97 == 46
                                    var jp1437 bool
                                    if t1440 {
                                        var t1441 bool = !saw_dot__93
                                        jp1437 = t1441
                                    } else {
                                        jp1437 = false
                                    }
                                    if jp1437 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1438 int = compound_old178 + compound_value179
                                        index__85 = t1438
                                        continue
                                    } else {
                                        break Loop_loop1391__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1391__2
                        }
                    }
                    var t1389 bool = !saw_digit__92
                    if t1389 {
                        var inline2045 FloatNatural = float_natural_zero()
                        var inline2046 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2045,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2046
                    } else {
                        var jp1305 uint8
                        if jp1298 {
                            jp1305 = 112
                        } else {
                            jp1305 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1384 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1385 bool = index__85 < t1384
                        var jp1322 bool
                        if t1385 {
                            var t1386 uint8
                            var inline2048 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1386 = inline2048
                            var t1387 uint8 = ascii_lower(t1386)
                            var t1388 bool = t1387 == jp1305
                            jp1322 = t1388
                        } else {
                            jp1322 = false
                        }
                        if jp1322 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1323 int = compound_old183 + compound_value184
                            index__85 = t1323
                            var t1374 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1375 bool = index__85 < t1374
                            var jp1369 bool
                            if t1375 {
                                var t1378 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1379 bool = t1378 == 43
                                if t1379 {
                                    jp1369 = true
                                } else {
                                    var t1380 uint8
                                    var inline2050 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1380 = inline2050
                                    var t1381 bool = t1380 == 45
                                    jp1369 = t1381
                                }
                            } else {
                                jp1369 = false
                            }
                            if jp1369 {
                                var t1370 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1371 bool = t1370 == 45
                                exponent_negative__104 = t1371
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1372 int = compound_old187 + compound_value188
                                index__85 = t1372
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1330__2:
                            for {
                                var t1331 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1332 bool = index__85 < t1331
                                if t1332 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1366 bool = current__106 >= 48
                                    var jp1335 bool
                                    if t1366 {
                                        var t1367 bool = current__106 <= 57
                                        jp1335 = t1367
                                    } else {
                                        jp1335 = false
                                    }
                                    if jp1335 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1339 bool = exponent__103 < 1000000
                                        if t1339 {
                                            var t1340 int = exponent__103 * 10
                                            var t1341 uint8 = current__106 - 48
                                            var t1342 int = int(uint8(t1341))
                                            var t1343 int = t1340 + t1342
                                            exponent__103 = t1343
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1337 int = compound_old196 + compound_value197
                                        index__85 = t1337
                                        continue
                                    } else {
                                        var t1345 bool = current__106 == 95
                                        if t1345 {
                                            var t1362 bool = !previous_digit__96
                                            var jp1358 bool
                                            if t1362 {
                                                jp1358 = true
                                            } else {
                                                var t1363 int = index__85 + 1
                                                var t1364 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1365 bool = t1363 >= t1364
                                                jp1358 = t1365
                                            }
                                            var jp1353 bool
                                            if jp1358 {
                                                jp1353 = true
                                            } else {
                                                var t1359 int = index__85 + 1
                                                var t1360 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1359)
                                                var t1361 bool = t1360 < 48
                                                jp1353 = t1361
                                            }
                                            var jp1350 bool
                                            if jp1353 {
                                                jp1350 = true
                                            } else {
                                                var t1354 int = index__85 + 1
                                                var t1355 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1354)
                                                var t1356 bool = t1355 > 57
                                                jp1350 = t1356
                                            }
                                            if jp1350 {
                                                var t1351 ParsedFloat = invalid_parsed_float()
                                                return t1351
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1347 int = compound_old201 + compound_value202
                                                index__85 = t1347
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1330__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1330__2
                                }
                            }
                            var t1328 bool = !exponent_digits__105
                            if t1328 {
                                var t1329 ParsedFloat = invalid_parsed_float()
                                return t1329
                            } else {
                                var t1318 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1319 bool = index__85 != t1318
                                if t1319 {
                                    var t1320 ParsedFloat = invalid_parsed_float()
                                    return t1320
                                } else {
                                    if exponent_negative__104 {
                                        var t1317 int = 0 - exponent__103
                                        exponent__103 = t1317
                                    } else {}
                                    var jp1310 int
                                    if jp1298 {
                                        jp1310 = 0
                                    } else {
                                        var t1316 int = exponent__103 - fraction_digits__94
                                        jp1310 = t1316
                                    }
                                    var jp1312 int
                                    if jp1298 {
                                        var t1314 int = fraction_digits__94 * 4
                                        var t1315 int = exponent__103 - t1314
                                        jp1312 = t1315
                                    } else {
                                        jp1312 = 0
                                    }
                                    var t1313 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1310,
                                        binary_exponent: jp1312,
                                        hexadecimal: jp1298,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1313
                                }
                            }
                        } else {
                            if jp1298 {
                                var t1383 ParsedFloat = invalid_parsed_float()
                                return t1383
                            } else {
                                var t1318 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1319 bool = index__85 != t1318
                                if t1319 {
                                    var t1320 ParsedFloat = invalid_parsed_float()
                                    return t1320
                                } else {
                                    if exponent_negative__104 {
                                        var t1317 int = 0 - exponent__103
                                        exponent__103 = t1317
                                    } else {}
                                    var jp1310 int
                                    if jp1298 {
                                        jp1310 = 0
                                    } else {
                                        var t1316 int = exponent__103 - fraction_digits__94
                                        jp1310 = t1316
                                    }
                                    var jp1312 int
                                    if jp1298 {
                                        var t1314 int = fraction_digits__94 * 4
                                        var t1315 int = exponent__103 - t1314
                                        jp1312 = t1315
                                    } else {
                                        jp1312 = 0
                                    }
                                    var t1313 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1310,
                                        binary_exponent: jp1312,
                                        hexadecimal: jp1298,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1313
                                }
                            }
                        }
                    }
                }
            } else {
                jp1298 = false
                if jp1298 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1442 int = compound_old145 + compound_value146
                    index__85 = t1442
                } else {}
                var mantissa_start__89 int = index__85
                var jp1301 int
                if jp1298 {
                    jp1301 = 16
                } else {
                    jp1301 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1395 uint32 = uint32(int(jp1301))
                Loop_loop1391__3:
                for {
                    var t1392 int
                    var inline2043 int = _goml_runtime_core_string_len(value__84)
                    t1392 = inline2043
                    var t1393 bool = index__85 < t1392
                    if t1393 {
                        var current__97 uint8
                        var inline2041 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline2041
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1301)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1395)
                            var t1396 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1396)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1407 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1407
                            } else {}
                            var t1405 bool = significant_digits__95 > 0
                            var jp1402 bool
                            if t1405 {
                                jp1402 = true
                            } else {
                                var t1406 bool = x151 != 0
                                jp1402 = t1406
                            }
                            if jp1402 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1403 int = compound_old160 + compound_value161
                                significant_digits__95 = t1403
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1399 int = compound_old164 + compound_value165
                            index__85 = t1399
                            continue
                        } else {
                            var t1410 bool = current__97 == 95
                            if t1410 {
                                var t1431 int = index__85 + 1
                                var t1432 int
                                var inline2039 int = _goml_runtime_core_string_len(value__84)
                                t1432 = inline2039
                                var t1433 bool = t1431 >= t1432
                                if t1433 {
                                    var inline2031 FloatNatural = float_natural_zero()
                                    var inline2032 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline2031,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline2032
                                } else {
                                    var t1412 int = index__85 + 1
                                    var t1413 uint8
                                    var inline2037 uint8 = _goml_runtime_core_string_byte_get(value__84, t1412)
                                    t1413 = inline2037
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1413, jp1301)
                                    var x169 bool = mtmp168._0
                                    var jp1428 bool
                                    if jp1298 {
                                        var t1430 bool = !saw_digit__92
                                        jp1428 = t1430
                                    } else {
                                        jp1428 = false
                                    }
                                    var jp1415 bool
                                    if jp1428 {
                                        var t1429 bool = index__85 == mantissa_start__89
                                        jp1415 = t1429
                                    } else {
                                        jp1415 = false
                                    }
                                    var t1425 bool = !previous_digit__96
                                    var jp1423 bool
                                    if t1425 {
                                        var t1426 bool = !jp1415
                                        jp1423 = t1426
                                    } else {
                                        jp1423 = false
                                    }
                                    var jp1420 bool
                                    if jp1423 {
                                        jp1420 = true
                                    } else {
                                        var t1424 bool = !x169
                                        jp1420 = t1424
                                    }
                                    if jp1420 {
                                        var inline2034 FloatNatural = float_natural_zero()
                                        var inline2035 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2034,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2035
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1417 int = compound_old173 + compound_value174
                                        index__85 = t1417
                                        continue
                                    }
                                }
                            } else {
                                var t1440 bool = current__97 == 46
                                var jp1437 bool
                                if t1440 {
                                    var t1441 bool = !saw_dot__93
                                    jp1437 = t1441
                                } else {
                                    jp1437 = false
                                }
                                if jp1437 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1438 int = compound_old178 + compound_value179
                                    index__85 = t1438
                                    continue
                                } else {
                                    break Loop_loop1391__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1391__3
                    }
                }
                var t1389 bool = !saw_digit__92
                if t1389 {
                    var inline2045 FloatNatural = float_natural_zero()
                    var inline2046 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline2045,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline2046
                } else {
                    var jp1305 uint8
                    if jp1298 {
                        jp1305 = 112
                    } else {
                        jp1305 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1384 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1385 bool = index__85 < t1384
                    var jp1322 bool
                    if t1385 {
                        var t1386 uint8
                        var inline2048 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1386 = inline2048
                        var t1387 uint8 = ascii_lower(t1386)
                        var t1388 bool = t1387 == jp1305
                        jp1322 = t1388
                    } else {
                        jp1322 = false
                    }
                    if jp1322 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1323 int = compound_old183 + compound_value184
                        index__85 = t1323
                        var t1374 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1375 bool = index__85 < t1374
                        var jp1369 bool
                        if t1375 {
                            var t1378 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1379 bool = t1378 == 43
                            if t1379 {
                                jp1369 = true
                            } else {
                                var t1380 uint8
                                var inline2050 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1380 = inline2050
                                var t1381 bool = t1380 == 45
                                jp1369 = t1381
                            }
                        } else {
                            jp1369 = false
                        }
                        if jp1369 {
                            var t1370 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1371 bool = t1370 == 45
                            exponent_negative__104 = t1371
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1372 int = compound_old187 + compound_value188
                            index__85 = t1372
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1330__3:
                        for {
                            var t1331 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1332 bool = index__85 < t1331
                            if t1332 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1366 bool = current__106 >= 48
                                var jp1335 bool
                                if t1366 {
                                    var t1367 bool = current__106 <= 57
                                    jp1335 = t1367
                                } else {
                                    jp1335 = false
                                }
                                if jp1335 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1339 bool = exponent__103 < 1000000
                                    if t1339 {
                                        var t1340 int = exponent__103 * 10
                                        var t1341 uint8 = current__106 - 48
                                        var t1342 int = int(uint8(t1341))
                                        var t1343 int = t1340 + t1342
                                        exponent__103 = t1343
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1337 int = compound_old196 + compound_value197
                                    index__85 = t1337
                                    continue
                                } else {
                                    var t1345 bool = current__106 == 95
                                    if t1345 {
                                        var t1362 bool = !previous_digit__96
                                        var jp1358 bool
                                        if t1362 {
                                            jp1358 = true
                                        } else {
                                            var t1363 int = index__85 + 1
                                            var t1364 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1365 bool = t1363 >= t1364
                                            jp1358 = t1365
                                        }
                                        var jp1353 bool
                                        if jp1358 {
                                            jp1353 = true
                                        } else {
                                            var t1359 int = index__85 + 1
                                            var t1360 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1359)
                                            var t1361 bool = t1360 < 48
                                            jp1353 = t1361
                                        }
                                        var jp1350 bool
                                        if jp1353 {
                                            jp1350 = true
                                        } else {
                                            var t1354 int = index__85 + 1
                                            var t1355 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1354)
                                            var t1356 bool = t1355 > 57
                                            jp1350 = t1356
                                        }
                                        if jp1350 {
                                            var t1351 ParsedFloat = invalid_parsed_float()
                                            return t1351
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1347 int = compound_old201 + compound_value202
                                            index__85 = t1347
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1330__3
                                    }
                                }
                            } else {
                                break Loop_loop1330__3
                            }
                        }
                        var t1328 bool = !exponent_digits__105
                        if t1328 {
                            var t1329 ParsedFloat = invalid_parsed_float()
                            return t1329
                        } else {
                            var t1318 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1319 bool = index__85 != t1318
                            if t1319 {
                                var t1320 ParsedFloat = invalid_parsed_float()
                                return t1320
                            } else {
                                if exponent_negative__104 {
                                    var t1317 int = 0 - exponent__103
                                    exponent__103 = t1317
                                } else {}
                                var jp1310 int
                                if jp1298 {
                                    jp1310 = 0
                                } else {
                                    var t1316 int = exponent__103 - fraction_digits__94
                                    jp1310 = t1316
                                }
                                var jp1312 int
                                if jp1298 {
                                    var t1314 int = fraction_digits__94 * 4
                                    var t1315 int = exponent__103 - t1314
                                    jp1312 = t1315
                                } else {
                                    jp1312 = 0
                                }
                                var t1313 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1310,
                                    binary_exponent: jp1312,
                                    hexadecimal: jp1298,
                                    significant_digits: significant_digits__95,
                                }
                                return t1313
                            }
                        }
                    } else {
                        if jp1298 {
                            var t1383 ParsedFloat = invalid_parsed_float()
                            return t1383
                        } else {
                            var t1318 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1319 bool = index__85 != t1318
                            if t1319 {
                                var t1320 ParsedFloat = invalid_parsed_float()
                                return t1320
                            } else {
                                if exponent_negative__104 {
                                    var t1317 int = 0 - exponent__103
                                    exponent__103 = t1317
                                } else {}
                                var jp1310 int
                                if jp1298 {
                                    jp1310 = 0
                                } else {
                                    var t1316 int = exponent__103 - fraction_digits__94
                                    jp1310 = t1316
                                }
                                var jp1312 int
                                if jp1298 {
                                    var t1314 int = fraction_digits__94 * 4
                                    var t1315 int = exponent__103 - t1314
                                    jp1312 = t1315
                                } else {
                                    jp1312 = 0
                                }
                                var t1313 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1310,
                                    binary_exponent: jp1312,
                                    hexadecimal: jp1298,
                                    significant_digits: significant_digits__95,
                                }
                                return t1313
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
    var inline2052 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline2052, 1)
    var inline2054 FloatNatural = FloatNatural{
        words: inline2052,
    }
    result__26 = inline2054
    var count__27 int = 0
    Loop_loop1481:
    for {
        var t1482 bool = count__27 < exponent__25
        if t1482 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1483 int = compound_old46 + compound_value47
            count__27 = t1483
            continue
        } else {
            break Loop_loop1481
        }
    }
    return result__26
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1570 bool
    var inline2056 *_goml_vec_uint32 = numerator__65.words
    var inline2057 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2056)
    t1570 = inline2057
    if t1570 {
        var t1571 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1571
    } else {
        var t1567 bool = binary_shift__67 >= 0
        var jp1492 FloatNatural
        if t1567 {
            var t1568 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1492 = t1568
        } else {
            var t1569 FloatNatural = float_natural_copy(numerator__65)
            jp1492 = t1569
        }
        var t1563 bool = binary_shift__67 >= 0
        var jp1494 FloatNatural
        if t1563 {
            var t1564 FloatNatural = float_natural_copy(denominator__66)
            jp1494 = t1564
        } else {
            var t1565 int = 0 - binary_shift__67
            var t1566 FloatNatural = float_natural_shift_left(denominator__66, t1565)
            jp1494 = t1566
        }
        var t1495 int = float_natural_bit_length(jp1492)
        var t1496 int = float_natural_bit_length(jp1494)
        var exponent__72 int = t1495 - t1496
        var t1557 bool = exponent__72 >= 0
        var jp1498 int
        if t1557 {
            var t1558 FloatNatural = float_natural_shift_left(jp1494, exponent__72)
            var t1559 int = float_natural_compare(jp1492, t1558)
            jp1498 = t1559
        } else {
            var t1560 int = 0 - exponent__72
            var t1561 FloatNatural = float_natural_shift_left(jp1492, t1560)
            var t1562 int = float_natural_compare(t1561, jp1494)
            jp1498 = t1562
        }
        var t1554 bool = jp1498 < 0
        if t1554 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1555 int = compound_old120 - compound_value121
            exponent__72 = t1555
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1548 bool = exponent__72 > exponent_bias__69
        if t1548 {
            var t1549 int = exponent_bias__69 + exponent_bias__69
            var t1550 int = t1549 + 1
            var t1551 uint64 = uint64(int(t1550))
            var t1552 uint64 = t1551 << mantissa_bits__68
            var t1553 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1552,
                _1: true,
            }
            return t1553
        } else {
            var t1543 bool = exponent__72 < minimum_exponent__74
            var jp1502 uint64
            if t1543 {
                var t1544 int = mantissa_bits__68 - minimum_exponent__74
                var t1545 uint64 = float_rational_quotient(jp1492, jp1494, t1544)
                jp1502 = t1545
            } else {
                var t1546 int = mantissa_bits__68 - exponent__72
                var t1547 uint64 = float_rational_quotient(jp1492, jp1494, t1546)
                jp1502 = t1547
            }
            var mantissa__76 uint64 = jp1502
            var t1505 bool = exponent__72 < minimum_exponent__74
            if t1505 {
                var t1508 bool = mantissa__76 == 0
                if t1508 {
                    var t1509 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1509
                } else {
                    var t1512_lhs uint64 = 1
                    var t1512 uint64 = t1512_lhs << mantissa_bits__68
                    var t1513 bool = mantissa__76 >= t1512
                    if t1513 {
                        var t1514_lhs uint64 = 1
                        var t1514 uint64 = t1514_lhs << mantissa_bits__68
                        var t1515_lhs uint64 = 1
                        var t1515 uint64 = t1515_lhs << mantissa_bits__68
                        var t1516 uint64 = mantissa__76 - t1515
                        var t1517 uint64 = t1514 | t1516
                        var t1518 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1517,
                            _1: false,
                        }
                        return t1518
                    } else {
                        var t1519 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1519
                    }
                }
            } else {
                var t1536 int = mantissa_bits__68 + 1
                var t1537_lhs uint64 = 1
                var t1537 uint64 = t1537_lhs << t1536
                var t1538 bool = mantissa__76 >= t1537
                if t1538 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1539 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1539
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1541 int = compound_old128 + compound_value129
                    exponent__72 = t1541
                } else {}
                var t1523 bool = exponent__72 > exponent_bias__69
                if t1523 {
                    var t1524 int = exponent_bias__69 + exponent_bias__69
                    var t1525 int = t1524 + 1
                    var t1526 uint64 = uint64(int(t1525))
                    var t1527 uint64 = t1526 << mantissa_bits__68
                    var t1528 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1527,
                        _1: true,
                    }
                    return t1528
                } else {
                    var t1529 int = exponent__72 + exponent_bias__69
                    var t1530 uint64 = uint64(int(t1529))
                    var t1531 uint64 = t1530 << mantissa_bits__68
                    var t1532_lhs uint64 = 1
                    var t1532 uint64 = t1532_lhs << mantissa_bits__68
                    var t1533 uint64 = mantissa__76 - t1532
                    var t1534 uint64 = t1531 | t1533
                    var t1535 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1534,
                        _1: false,
                    }
                    return t1535
                }
            }
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1576 int = vec_len__Vec_6uint32(self__528)
    var t1577 bool = t1576 == 0
    return t1577
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1580:
    for {
        var t1588 *_goml_vec_uint32 = value__7.words
        var t1589 bool
        var inline2068 int = vec_len__Vec_6uint32(t1588)
        var inline2069 bool = inline2068 == 0
        t1589 = inline2069
        var t1590 bool = !t1589
        var jp1582 bool
        if t1590 {
            var t1591 *_goml_vec_uint32 = value__7.words
            var t1592 *_goml_vec_uint32 = value__7.words
            var t1593 int
            var inline2062 int = vec_len__Vec_6uint32(t1592)
            t1593 = inline2062
            var t1594 int = t1593 - 1
            var t1595 uint32 = vec_get__Vec_6uint32(t1591, t1594)
            var t1596 bool = t1595 == 0
            jp1582 = t1596
        } else {
            jp1582 = false
        }
        if jp1582 {
            var t1583 *_goml_vec_uint32 = value__7.words
            var t1584 *_goml_vec_uint32 = value__7.words
            var t1585 int
            var inline2066 int = vec_len__Vec_6uint32(t1584)
            t1585 = inline2066
            var t1586 int = t1585 - 1
            vec_truncate__Vec_6uint32(t1583, t1586)
            continue
        } else {
            break Loop_loop1580
        }
    }
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1605 bool = string_is_char_boundary(value__274, start__275)
    var jp1602 bool
    if t1605 {
        var t1606 bool = string_is_char_boundary(value__274, end__276)
        jp1602 = t1606
    } else {
        jp1602 = false
    }
    if jp1602 {
        var t1603 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1603
    } else {
        var t1604 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1604
    }
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1621 int
    var inline2086 int = _goml_runtime_core_string_len(value__78)
    t1621 = inline2086
    var t1622 int
    var inline2084 int = _goml_runtime_core_string_len(expected__79)
    t1622 = inline2084
    var t1623 bool = t1621 != t1622
    if t1623 {
        return false
    } else {
        var index__80 int = 0
        var inline2076 uint8 = 97 - 65
        Loop_loop1611:
        for {
            var t1612 int
            var inline2082 int = _goml_runtime_core_string_len(value__78)
            t1612 = inline2082
            var t1613 bool = index__80 < t1612
            if t1613 {
                var t1617 uint8
                var inline2080 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1617 = inline2080
                var t1618 uint8
                var inline2073 bool = t1617 >= 65
                var inline2075 bool
                if inline2073 {
                    var inline2078 bool = t1617 <= 90
                    inline2075 = inline2078
                } else {
                    inline2075 = false
                }
                if inline2075 {
                    var inline2077 uint8 = t1617 + inline2076
                    t1618 = inline2077
                    var t1619 uint8
                    var inline2071 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1619 = inline2071
                    var t1620 bool = t1618 != t1619
                    if t1620 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1615 int = compound_old134 + compound_value135
                        index__80 = t1615
                        continue
                    }
                } else {
                    t1618 = t1617
                    var t1619 uint8
                    var inline2071 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1619 = inline2071
                    var t1620 bool = t1618 != t1619
                    if t1620 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1615 int = compound_old134 + compound_value135
                        index__80 = t1615
                        continue
                    }
                }
            } else {
                break Loop_loop1611
            }
        }
        return true
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1632 bool = value__77 >= 65
    var jp1629 bool
    if t1632 {
        var t1633 bool = value__77 <= 90
        jp1629 = t1633
    } else {
        jp1629 = false
    }
    if jp1629 {
        var t1630 uint8 = 97 - 65
        var t1631 uint8 = value__77 + t1630
        return t1631
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1660 bool = value__81 >= 48
    var jp1644 bool
    if t1660 {
        var t1661 bool = value__81 <= 57
        jp1644 = t1661
    } else {
        jp1644 = false
    }
    var jp1637 int
    if jp1644 {
        var t1645 uint8 = value__81 - 48
        var t1646 int = int(uint8(t1645))
        jp1637 = t1646
        var t1640 bool = jp1637 < base__82
        if t1640 {
            var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1637,
            }
            return t1641
        } else {
            var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1642
        }
    } else {
        var t1656 uint8
        var inline2102 bool = value__81 >= 65
        var inline2104 bool
        if inline2102 {
            var inline2107 bool = value__81 <= 90
            inline2104 = inline2107
        } else {
            inline2104 = false
        }
        if inline2104 {
            var inline2105 uint8 = 97 - 65
            var inline2106 uint8 = value__81 + inline2105
            t1656 = inline2106
            var t1657 bool = t1656 >= 97
            var jp1650 bool
            if t1657 {
                var t1658 uint8
                var inline2088 bool = value__81 >= 65
                var inline2090 bool
                if inline2088 {
                    var inline2093 bool = value__81 <= 90
                    inline2090 = inline2093
                } else {
                    inline2090 = false
                }
                if inline2090 {
                    var inline2091 uint8 = 97 - 65
                    var inline2092 uint8 = value__81 + inline2091
                    t1658 = inline2092
                    var t1659 bool = t1658 <= 102
                    jp1650 = t1659
                    if jp1650 {
                        var t1651 uint8
                        var inline2095 bool = value__81 >= 65
                        var inline2097 bool
                        if inline2095 {
                            var inline2100 bool = value__81 <= 90
                            inline2097 = inline2100
                        } else {
                            inline2097 = false
                        }
                        if inline2097 {
                            var inline2098 uint8 = 97 - 65
                            var inline2099 uint8 = value__81 + inline2098
                            t1651 = inline2099
                            var t1652 uint8 = t1651 - 97
                            var t1653 uint8 = t1652 + 10
                            var t1654 int = int(uint8(t1653))
                            jp1637 = t1654
                            var t1640 bool = jp1637 < base__82
                            if t1640 {
                                var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1637,
                                }
                                return t1641
                            } else {
                                var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1642
                            }
                        } else {
                            t1651 = value__81
                            var t1652 uint8 = t1651 - 97
                            var t1653 uint8 = t1652 + 10
                            var t1654 int = int(uint8(t1653))
                            jp1637 = t1654
                            var t1640 bool = jp1637 < base__82
                            if t1640 {
                                var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1637,
                                }
                                return t1641
                            } else {
                                var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1642
                            }
                        }
                    } else {
                        var t1655 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1655
                    }
                } else {
                    t1658 = value__81
                    var t1659 bool = t1658 <= 102
                    jp1650 = t1659
                    if jp1650 {
                        var t1651 uint8
                        var inline2095 bool = value__81 >= 65
                        var inline2097 bool
                        if inline2095 {
                            var inline2100 bool = value__81 <= 90
                            inline2097 = inline2100
                        } else {
                            inline2097 = false
                        }
                        if inline2097 {
                            var inline2098 uint8 = 97 - 65
                            var inline2099 uint8 = value__81 + inline2098
                            t1651 = inline2099
                            var t1652 uint8 = t1651 - 97
                            var t1653 uint8 = t1652 + 10
                            var t1654 int = int(uint8(t1653))
                            jp1637 = t1654
                            var t1640 bool = jp1637 < base__82
                            if t1640 {
                                var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1637,
                                }
                                return t1641
                            } else {
                                var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1642
                            }
                        } else {
                            t1651 = value__81
                            var t1652 uint8 = t1651 - 97
                            var t1653 uint8 = t1652 + 10
                            var t1654 int = int(uint8(t1653))
                            jp1637 = t1654
                            var t1640 bool = jp1637 < base__82
                            if t1640 {
                                var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1637,
                                }
                                return t1641
                            } else {
                                var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1642
                            }
                        }
                    } else {
                        var t1655 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1655
                    }
                }
            } else {
                jp1650 = false
                if jp1650 {
                    var t1651 uint8
                    var inline2095 bool = value__81 >= 65
                    var inline2097 bool
                    if inline2095 {
                        var inline2100 bool = value__81 <= 90
                        inline2097 = inline2100
                    } else {
                        inline2097 = false
                    }
                    if inline2097 {
                        var inline2098 uint8 = 97 - 65
                        var inline2099 uint8 = value__81 + inline2098
                        t1651 = inline2099
                        var t1652 uint8 = t1651 - 97
                        var t1653 uint8 = t1652 + 10
                        var t1654 int = int(uint8(t1653))
                        jp1637 = t1654
                        var t1640 bool = jp1637 < base__82
                        if t1640 {
                            var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1637,
                            }
                            return t1641
                        } else {
                            var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1642
                        }
                    } else {
                        t1651 = value__81
                        var t1652 uint8 = t1651 - 97
                        var t1653 uint8 = t1652 + 10
                        var t1654 int = int(uint8(t1653))
                        jp1637 = t1654
                        var t1640 bool = jp1637 < base__82
                        if t1640 {
                            var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1637,
                            }
                            return t1641
                        } else {
                            var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1642
                        }
                    }
                } else {
                    var t1655 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1655
                }
            }
        } else {
            t1656 = value__81
            var t1657 bool = t1656 >= 97
            var jp1650 bool
            if t1657 {
                var t1658 uint8
                var inline2088 bool = value__81 >= 65
                var inline2090 bool
                if inline2088 {
                    var inline2093 bool = value__81 <= 90
                    inline2090 = inline2093
                } else {
                    inline2090 = false
                }
                if inline2090 {
                    var inline2091 uint8 = 97 - 65
                    var inline2092 uint8 = value__81 + inline2091
                    t1658 = inline2092
                    var t1659 bool = t1658 <= 102
                    jp1650 = t1659
                    if jp1650 {
                        var t1651 uint8
                        var inline2095 bool = value__81 >= 65
                        var inline2097 bool
                        if inline2095 {
                            var inline2100 bool = value__81 <= 90
                            inline2097 = inline2100
                        } else {
                            inline2097 = false
                        }
                        if inline2097 {
                            var inline2098 uint8 = 97 - 65
                            var inline2099 uint8 = value__81 + inline2098
                            t1651 = inline2099
                            var t1652 uint8 = t1651 - 97
                            var t1653 uint8 = t1652 + 10
                            var t1654 int = int(uint8(t1653))
                            jp1637 = t1654
                            var t1640 bool = jp1637 < base__82
                            if t1640 {
                                var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1637,
                                }
                                return t1641
                            } else {
                                var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1642
                            }
                        } else {
                            t1651 = value__81
                            var t1652 uint8 = t1651 - 97
                            var t1653 uint8 = t1652 + 10
                            var t1654 int = int(uint8(t1653))
                            jp1637 = t1654
                            var t1640 bool = jp1637 < base__82
                            if t1640 {
                                var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1637,
                                }
                                return t1641
                            } else {
                                var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1642
                            }
                        }
                    } else {
                        var t1655 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1655
                    }
                } else {
                    t1658 = value__81
                    var t1659 bool = t1658 <= 102
                    jp1650 = t1659
                    if jp1650 {
                        var t1651 uint8
                        var inline2095 bool = value__81 >= 65
                        var inline2097 bool
                        if inline2095 {
                            var inline2100 bool = value__81 <= 90
                            inline2097 = inline2100
                        } else {
                            inline2097 = false
                        }
                        if inline2097 {
                            var inline2098 uint8 = 97 - 65
                            var inline2099 uint8 = value__81 + inline2098
                            t1651 = inline2099
                            var t1652 uint8 = t1651 - 97
                            var t1653 uint8 = t1652 + 10
                            var t1654 int = int(uint8(t1653))
                            jp1637 = t1654
                            var t1640 bool = jp1637 < base__82
                            if t1640 {
                                var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1637,
                                }
                                return t1641
                            } else {
                                var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1642
                            }
                        } else {
                            t1651 = value__81
                            var t1652 uint8 = t1651 - 97
                            var t1653 uint8 = t1652 + 10
                            var t1654 int = int(uint8(t1653))
                            jp1637 = t1654
                            var t1640 bool = jp1637 < base__82
                            if t1640 {
                                var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1637,
                                }
                                return t1641
                            } else {
                                var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1642
                            }
                        }
                    } else {
                        var t1655 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1655
                    }
                }
            } else {
                jp1650 = false
                if jp1650 {
                    var t1651 uint8
                    var inline2095 bool = value__81 >= 65
                    var inline2097 bool
                    if inline2095 {
                        var inline2100 bool = value__81 <= 90
                        inline2097 = inline2100
                    } else {
                        inline2097 = false
                    }
                    if inline2097 {
                        var inline2098 uint8 = 97 - 65
                        var inline2099 uint8 = value__81 + inline2098
                        t1651 = inline2099
                        var t1652 uint8 = t1651 - 97
                        var t1653 uint8 = t1652 + 10
                        var t1654 int = int(uint8(t1653))
                        jp1637 = t1654
                        var t1640 bool = jp1637 < base__82
                        if t1640 {
                            var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1637,
                            }
                            return t1641
                        } else {
                            var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1642
                        }
                    } else {
                        t1651 = value__81
                        var t1652 uint8 = t1651 - 97
                        var t1653 uint8 = t1652 + 10
                        var t1654 int = int(uint8(t1653))
                        jp1637 = t1654
                        var t1640 bool = jp1637 < base__82
                        if t1640 {
                            var t1641 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1637,
                            }
                            return t1641
                        } else {
                            var t1642 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1642
                        }
                    }
                } else {
                    var t1655 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1655
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1664:
    for {
        var t1665 bool = carry__22 != 0
        if t1665 {
            var t1674 *_goml_vec_uint32 = value__20.words
            var t1675 int
            var inline2112 int = vec_len__Vec_6uint32(t1674)
            t1675 = inline2112
            var t1676 bool = index__23 == t1675
            if t1676 {
                var t1677 *_goml_vec_uint32 = value__20.words
                var inline2109 uint32 = 0
                vec_push__Vec_6uint32(t1677, inline2109)
            } else {}
            var t1667 *_goml_vec_uint32 = value__20.words
            var t1668 uint32 = vec_get__Vec_6uint32(t1667, index__23)
            var t1669 uint64 = uint64(uint32(t1668))
            var sum__24 uint64 = t1669 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1671_rhs int = 32
            var t1671 uint64 = sum__24 >> t1671_rhs
            carry__22 = t1671
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1672 int = compound_old42 + compound_value43
            index__23 = t1672
            continue
        } else {
            break Loop_loop1664
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1681 FloatNatural
    var inline2114 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2115 FloatNatural = FloatNatural{
        words: inline2114,
    }
    t1681 = inline2115
    var t1682 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1681,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1682
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1702 *_goml_vec_uint32 = value__9.words
    var t1703 bool
    var inline2121 int = vec_len__Vec_6uint32(t1702)
    var inline2122 bool = inline2121 == 0
    t1703 = inline2122
    if t1703 {
        return 0
    } else {
        var t1686 *_goml_vec_uint32 = value__9.words
        var t1687 *_goml_vec_uint32 = value__9.words
        var t1688 int
        var inline2119 int = vec_len__Vec_6uint32(t1687)
        t1688 = inline2119
        var t1689 int = t1688 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1686, t1689)
        var bits__11 int = 0
        Loop_loop1696:
        for {
            var t1697 bool = high__10 != 0
            if t1697 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1698 uint32 = compound_old9 >> compound_value10
                high__10 = t1698
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1700 int = compound_old12 + compound_value13
                bits__11 = t1700
                continue
            } else {
                break Loop_loop1696
            }
        }
        var t1691 *_goml_vec_uint32 = value__9.words
        var t1692 int
        var inline2117 int = vec_len__Vec_6uint32(t1691)
        t1692 = inline2117
        var t1693 int = t1692 - 1
        var t1694 int = t1693 * 32
        var t1695 int = t1694 + bits__11
        return t1695
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1725 *_goml_vec_uint32 = left__12.words
    var t1726 int
    var inline2132 int = vec_len__Vec_6uint32(t1725)
    t1726 = inline2132
    var t1727 *_goml_vec_uint32 = right__13.words
    var t1728 int
    var inline2130 int = vec_len__Vec_6uint32(t1727)
    t1728 = inline2130
    var t1729 bool = t1726 < t1728
    if t1729 {
        return -1
    } else {
        var t1731 *_goml_vec_uint32 = left__12.words
        var t1732 int
        var inline2126 int = vec_len__Vec_6uint32(t1731)
        t1732 = inline2126
        var t1733 *_goml_vec_uint32 = right__13.words
        var t1734 int
        var inline2124 int = vec_len__Vec_6uint32(t1733)
        t1734 = inline2124
        var t1735 bool = t1732 > t1734
        if t1735 {
            return 1
        } else {
            var t1707 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2128 int = vec_len__Vec_6uint32(t1707)
            index__14 = inline2128
            Loop_loop1709:
            for {
                var t1710 bool = index__14 > 0
                if t1710 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1711 int = compound_old17 - compound_value18
                    index__14 = t1711
                    var t1714 *_goml_vec_uint32 = left__12.words
                    var t1715 uint32 = vec_get__Vec_6uint32(t1714, index__14)
                    var t1716 *_goml_vec_uint32 = right__13.words
                    var t1717 uint32 = vec_get__Vec_6uint32(t1716, index__14)
                    var t1718 bool = t1715 < t1717
                    if t1718 {
                        return -1
                    } else {
                        var t1720 *_goml_vec_uint32 = left__12.words
                        var t1721 uint32 = vec_get__Vec_6uint32(t1720, index__14)
                        var t1722 *_goml_vec_uint32 = right__13.words
                        var t1723 uint32 = vec_get__Vec_6uint32(t1722, index__14)
                        var t1724 bool = t1721 > t1723
                        if t1724 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1709
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1771 bool = shift__57 >= 0
    var jp1739 FloatNatural
    if t1771 {
        var t1772 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1739 = t1772
    } else {
        var t1773 FloatNatural = float_natural_copy(numerator__55)
        jp1739 = t1773
    }
    var t1767 bool = shift__57 >= 0
    var jp1741 FloatNatural
    if t1767 {
        var t1768 FloatNatural = float_natural_copy(denominator__56)
        jp1741 = t1768
    } else {
        var t1769 int = 0 - shift__57
        var t1770 FloatNatural = float_natural_shift_left(denominator__56, t1769)
        jp1741 = t1770
    }
    var quotient__60 uint64 = 0
    Loop_loop1754:
    for {
        var t1755 int = float_natural_compare(jp1739, jp1741)
        var t1756 bool = t1755 >= 0
        if t1756 {
            var t1757 int = float_natural_bit_length(jp1739)
            var t1758 int = float_natural_bit_length(jp1741)
            var offset__61 int = t1757 - t1758
            var part__62 FloatNatural = float_natural_shift_left(jp1741, offset__61)
            var t1762 int = float_natural_compare(jp1739, part__62)
            var t1763 bool = t1762 < 0
            if t1763 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1764 int = compound_old105 - compound_value106
                offset__61 = t1764
                var t1766 FloatNatural = float_natural_shift_left(jp1741, offset__61)
                part__62 = t1766
            } else {}
            float_natural_subtract(jp1739, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1760 uint64 = compound_old111 | compound_value112
            quotient__60 = t1760
            continue
        } else {
            break Loop_loop1754
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1739, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1741)
    var t1748 bool = rounding__64 > 0
    var jp1745 bool
    if t1748 {
        jp1745 = true
    } else {
        var t1751 bool = rounding__64 == 0
        if t1751 {
            var t1752_rhs uint64 = 1
            var t1752 uint64 = quotient__60 & t1752_rhs
            var t1753 bool = t1752 == 1
            jp1745 = t1753
        } else {
            jp1745 = false
        }
    }
    if jp1745 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1746 uint64 = compound_old115 + compound_value116
        quotient__60 = t1746
    } else {}
    return quotient__60
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(self__531 *_goml_vec_uint32, len__532 int) struct{} {
    vec_truncate__Vec_6uint32(self__531, len__532)
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1789 bool = index__269 < 0
    var jp1781 bool
    if t1789 {
        jp1781 = true
    } else {
        var t1790 int
        var inline2134 int = _goml_runtime_core_string_len(value__268)
        t1790 = inline2134
        var t1791 bool = index__269 > t1790
        jp1781 = t1791
    }
    if jp1781 {
        return false
    } else {
        var t1784 int
        var inline2138 int = _goml_runtime_core_string_len(value__268)
        t1784 = inline2138
        var t1785 bool = index__269 == t1784
        if t1785 {
            return true
        } else {
            var t1786 uint8
            var inline2136 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1786 = inline2136
            var t1787_rhs uint8 = 192
            var t1787 uint8 = t1786 & t1787_rhs
            var t1788 bool = t1787 != 128
            return t1788
        }
    }
}

func float_natural_subtract(value__37 FloatNatural, other__38 FloatNatural) struct{} {
    var base__39 uint64 = 4294967296
    var borrow__40 uint64 = 0
    var index__41 int = 0
    Loop_loop1795:
    for {
        var t1796 *_goml_vec_uint32 = value__37.words
        var t1797 int
        var inline2142 int = vec_len__Vec_6uint32(t1796)
        t1797 = inline2142
        var t1798 bool = index__41 < t1797
        if t1798 {
            var t1812 *_goml_vec_uint32 = other__38.words
            var t1813 int
            var inline2140 int = vec_len__Vec_6uint32(t1812)
            t1813 = inline2140
            var t1814 bool = index__41 < t1813
            var jp1800 uint64
            if t1814 {
                var t1815 *_goml_vec_uint32 = other__38.words
                var t1816 uint32 = vec_get__Vec_6uint32(t1815, index__41)
                var t1817 uint64 = uint64(uint32(t1816))
                jp1800 = t1817
            } else {
                jp1800 = 0
            }
            var right__42 uint64 = jp1800 + borrow__40
            var t1801 *_goml_vec_uint32 = value__37.words
            var t1802 uint32 = vec_get__Vec_6uint32(t1801, index__41)
            var left__43 uint64 = uint64(uint32(t1802))
            var t1806 bool = left__43 >= right__42
            if t1806 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1807 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1807))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1809 uint64 = base__39 + left__43
                var t1810 uint64 = t1809 - right__42
                var value75 uint32 = uint32(uint64(t1810))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1804 int = compound_old79 + compound_value80
            index__41 = t1804
            continue
        } else {
            break Loop_loop1795
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func main() {
    main0()
}
