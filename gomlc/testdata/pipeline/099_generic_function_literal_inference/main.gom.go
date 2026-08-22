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
    var a__1 uint8
    var inline1861 uint8 = 42
    a__1 = inline1861
    var t800 string
    var inline1859 string = __goml_builtin_uint8_to_string(a__1)
    t800 = inline1859
    var inline1856 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t800)
    _goml_runtime_core_string_println(inline1856)
    var b__2 float32
    var inline1854 float32 = 3.140000104904175
    b__2 = inline1854
    var t801 string
    var inline1852 string = __goml_builtin_float32_to_string(b__2)
    t801 = inline1852
    var inline1849 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t801)
    _goml_runtime_core_string_println(inline1849)
    var c__3 int64
    var inline1847 int64 = 100
    c__3 = inline1847
    var t802 string
    var inline1845 string = __goml_builtin_int64_to_string(c__3)
    t802 = inline1845
    var inline1842 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t802)
    _goml_runtime_core_string_println(inline1842)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_uint8_to_string(value__228 uint8) string {
    var t825 uint64 = uint64(uint8(value__228))
    var t826 string = decimal_string(t825)
    return t826
}

func __goml_builtin_float32_to_string(value__194 float32) string {
    var t829 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(value__194)
    var t830 uint64 = uint64(uint32(t829))
    var t831 string = format_float_bits(t830, 23, 8, 127)
    return t831
}

func __goml_builtin_int64_to_string(value__226 int64) string {
    var inline1873 bool = value__226 < 0
    if inline1873 {
        var inline1874 uint64 = uint64(int64(value__226))
        var inline1875 uint64 = 0 - inline1874
        var inline1876 string = decimal_string(inline1875)
        var inline1877 string = "-" + inline1876
        return inline1877
    } else {
        var inline1878 uint64 = uint64(int64(value__226))
        var inline1879 string = decimal_string(inline1878)
        return inline1879
    }
}

func decimal_string(value__208 uint64) string {
    var t857 bool = value__208 == 0
    if t857 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop850:
        for {
            var t851 bool = remaining__210 > 0
            if t851 {
                var t852_rhs uint64 = 10
                var t852 uint64 = remaining__210 % t852_rhs
                var t853 uint8 = uint8(uint64(t852))
                var t854 uint8 = t853 + 48
                vec_push__Vec_5uint8(reversed__209, t854)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t855 uint64 = compound_old353 / compound_value354
                remaining__210 = t855
                continue
            } else {
                break Loop_loop850
            }
        }
        var t839 int
        var inline1889 int = vec_len__Vec_5uint8(reversed__209)
        t839 = inline1889
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t839)
        var offset__212 int = 0
        Loop_loop841:
        for {
            var t842 int
            var inline1887 int = vec_len__Vec_5uint8(reversed__209)
            t842 = inline1887
            var t843 bool = offset__212 < t842
            if t843 {
                var t844 int
                var inline1885 int = vec_len__Vec_5uint8(reversed__209)
                t844 = inline1885
                var t845 int = t844 - offset__212
                var t846 int = t845 - 1
                var t847 uint8 = vec_get__Vec_5uint8(reversed__209, t846)
                vec_push__Vec_5uint8(bytes__211, t847)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t848 int = compound_old358 + compound_value359
                offset__212 = t848
                continue
            } else {
                break Loop_loop841
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t860 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t860
    var t861 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t861 != 0
    var t862_lhs uint64 = 1
    var t862 uint64 = t862_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t862 - 1
    var t863 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t863 & exponent_mask__166
    var t864_lhs uint64 = 1
    var t864 uint64 = t864_lhs << mantissa_bits__161
    var t865 uint64 = t864 - 1
    var fraction__168 uint64 = bits__160 & t865
    var t929 bool = exponent__167 == exponent_mask__166
    if t929 {
        var t931 bool = fraction__168 == 0
        if t931 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t937 bool = exponent__167 == 0
        var jp935 bool
        if t937 {
            var t938 bool = fraction__168 == 0
            jp935 = t938
        } else {
            jp935 = false
        }
        if jp935 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t926 bool = exponent__167 == 0
            var jp868 uint64
            if t926 {
                jp868 = fraction__168
            } else {
                var t927_lhs uint64 = 1
                var t927 uint64 = t927_lhs << mantissa_bits__161
                var t928 uint64 = fraction__168 | t927
                jp868 = t928
            }
            var t920 bool = exponent__167 == 0
            var jp870 int
            if t920 {
                var t921 int = 1 - exponent_bias__163
                var t922 int = t921 - mantissa_bits__161
                jp870 = t922
            } else {
                var t923 int = int(uint64(exponent__167))
                var t924 int = t923 - exponent_bias__163
                var t925 int = t924 - mantissa_bits__161
                jp870 = t925
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp868)
            var t875 bool = jp870 >= 0
            var jp872 int
            if t875 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp870)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t894 bool = mantissa_bits__161 == 23
                var jp877 int
                if t894 {
                    jp877 = 9
                } else {
                    jp877 = 17
                }
                var t891 int
                var inline1897 int = _goml_runtime_core_string_len(digits__173)
                t891 = inline1897
                var t892 bool = t891 < jp877
                var jp879 int
                if t892 {
                    var inline1891 int = _goml_runtime_core_string_len(digits__173)
                    jp879 = inline1891
                } else {
                    jp879 = jp877
                }
                var count__176 int = 1
                Loop_loop882:
                for {
                    var t883 bool = count__176 <= jp879
                    if t883 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t884 int
                        var inline1893 int = _goml_runtime_core_string_len(digits__173)
                        t884 = inline1893
                        var jp886 int
                        if x319 {
                            jp886 = 1
                        } else {
                            jp886 = 0
                        }
                        var point__180 int = t884 + jp886
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t890 bool = x322 == bits__160
                        if t890 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t888 int = compound_old324 + compound_value325
                            count__176 = t888
                            continue
                        }
                    } else {
                        break Loop_loop882
                    }
                }
                var inline1895 int = _goml_runtime_core_string_len(digits__173)
                jp872 = inline1895
                var t873 string = float_natural_decimal(exact_value__171)
                var t874 string = fixed_float_text(t873, jp872, negative__165)
                return t874
            } else {
                var count__183 int = 0
                var t916 int = 0 - jp870
                Loop_loop915:
                for {
                    var t917 bool = count__183 < t916
                    if t917 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t918 int = compound_old329 + compound_value330
                        count__183 = t918
                        continue
                    } else {
                        break Loop_loop915
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t896 int
                var inline1903 int = _goml_runtime_core_string_len(digits__184)
                t896 = inline1903
                var point__185 int = t896 + jp870
                var t914 bool = mantissa_bits__161 == 23
                var jp898 int
                if t914 {
                    jp898 = 9
                } else {
                    jp898 = 17
                }
                var t911 int
                var inline1901 int = _goml_runtime_core_string_len(digits__184)
                t911 = inline1901
                var t912 bool = t911 < jp898
                var jp900 int
                if t912 {
                    var inline1899 int = _goml_runtime_core_string_len(digits__184)
                    jp900 = inline1899
                } else {
                    jp900 = jp898
                }
                count__183 = 1
                Loop_loop902:
                for {
                    var t903 bool = count__183 <= jp900
                    if t903 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp905 int
                        if x336 {
                            jp905 = 1
                        } else {
                            jp905 = 0
                        }
                        var t906 int = point__185 + jp905
                        var candidate__191 string = fixed_float_text(rounded__190, t906, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t910 bool = x339 == bits__160
                        if t910 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t908 int = compound_old341 + compound_value342
                            count__183 = t908
                            continue
                        }
                    } else {
                        break Loop_loop902
                    }
                }
                jp872 = point__185
                var t873 string = float_natural_decimal(exact_value__171)
                var t874 string = fixed_float_text(t873, jp872, negative__165)
                return t874
            }
        }
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline1909 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline1910 FloatNatural = FloatNatural{
        words: inline1909,
    }
    result__2 = inline1910
    var t958 bool = value__1 != 0
    if t958 {
        var t959 *_goml_vec_uint32 = result__2.words
        var t960 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t959, t960)
        var t961_rhs int = 32
        var t961 uint64 = value__1 >> t961_rhs
        var high__3 uint32 = uint32(uint64(t961))
        var t963 bool = high__3 != 0
        if t963 {
            var t964 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t964, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t993 bool
    var inline1927 *_goml_vec_uint32 = value__28.words
    var inline1928 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1927)
    t993 = inline1928
    if t993 {
        var inline1912 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline1913 FloatNatural = FloatNatural{
            words: inline1912,
        }
        return inline1913
    } else {
        var t996 bool = bits__29 == 0
        if t996 {
            var t997 FloatNatural = float_natural_copy(value__28)
            return t997
        } else {
            var result__30 FloatNatural
            var inline1924 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline1925 FloatNatural = FloatNatural{
                words: inline1924,
            }
            result__30 = inline1925
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop988:
            for {
                var t989 bool = index__33 < word_shift__31
                if t989 {
                    var t990 *_goml_vec_uint32 = result__30.words
                    var inline1915 uint32 = 0
                    vec_push__Vec_6uint32(t990, inline1915)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t991 int = compound_old52 + compound_value53
                    index__33 = t991
                    continue
                } else {
                    break Loop_loop988
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop976:
            for {
                var t977 *_goml_vec_uint32 = value__28.words
                var t978 int
                var inline1920 int = vec_len__Vec_6uint32(t977)
                t978 = inline1920
                var t979 bool = index__33 < t978
                if t979 {
                    var t980 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t980, index__33)
                    var t981 uint64 = uint64(uint32(word__35))
                    var t982 uint64 = t981 << bit_shift__32
                    var shifted__36 uint64 = t982 | carry__34
                    var t983 *_goml_vec_uint32 = result__30.words
                    var t984 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t983, t984)
                    var t985_rhs int = 32
                    var t985 uint64 = shifted__36 >> t985_rhs
                    carry__34 = t985
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t986 int = compound_old59 + compound_value60
                    index__33 = t986
                    continue
                } else {
                    break Loop_loop976
                }
            }
            var t972 bool = carry__34 != 0
            if t972 {
                var t973 *_goml_vec_uint32 = result__30.words
                var t974 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t973, t974)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t1020 bool
    var inline1943 *_goml_vec_uint32 = value__49.words
    var inline1944 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1943)
    t1020 = inline1944
    if t1020 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop1013:
        for {
            var t1014 bool
            var inline1932 *_goml_vec_uint32 = current__50.words
            var inline1933 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1932)
            t1014 = inline1933
            var t1015 bool = !t1014
            if t1015 {
                var t1016 uint32 = float_natural_divide_small(current__50, 10)
                var t1017 uint8 = uint8(uint32(t1016))
                var t1018 uint8 = t1017 + 48
                vec_push__Vec_5uint8(reversed__51, t1018)
                continue
            } else {
                break Loop_loop1013
            }
        }
        var t1002 int
        var inline1941 int = vec_len__Vec_5uint8(reversed__51)
        t1002 = inline1941
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1002)
        var offset__53 int = 0
        Loop_loop1004:
        for {
            var t1005 int
            var inline1939 int = vec_len__Vec_5uint8(reversed__51)
            t1005 = inline1939
            var t1006 bool = offset__53 < t1005
            if t1006 {
                var t1007 int
                var inline1937 int = vec_len__Vec_5uint8(reversed__51)
                t1007 = inline1937
                var t1008 int = t1007 - offset__53
                var t1009 int = t1008 - 1
                var t1010 uint8 = vec_get__Vec_5uint8(reversed__51, t1009)
                vec_push__Vec_5uint8(output__52, t1010)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t1011 int = compound_old98 + compound_value99
                offset__53 = t1011
                continue
            } else {
                break Loop_loop1004
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1023 int = _goml_runtime_core_string_len(self__289)
    return t1023
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t1026 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1026)
    var index__148 int = 0
    Loop_loop1081:
    for {
        var t1082 bool = index__148 < count__146
        if t1082 {
            var t1083 uint8
            var inline1948 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1083 = inline1948
            vec_push__Vec_5uint8(output__147, t1083)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1084 int = compound_old267 + compound_value268
            index__148 = t1084
            continue
        } else {
            break Loop_loop1081
        }
    }
    var t1078 int
    var inline1969 int = _goml_runtime_core_string_len(exact__145)
    t1078 = inline1969
    var t1079 bool = count__146 == t1078
    if t1079 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1080 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1080
    } else {
        var next__150 uint8
        var inline1967 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline1967
        var trailing__151 bool = false
        var t1029 int = count__146 + 1
        index__148 = t1029
        Loop_loop1070:
        for {
            var t1071 int
            var inline1952 int = _goml_runtime_core_string_len(exact__145)
            t1071 = inline1952
            var t1072 bool = index__148 < t1071
            if t1072 {
                var t1076 uint8
                var inline1950 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1076 = inline1950
                var t1077 bool = t1076 != 48
                if t1077 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1074 int = compound_old278 + compound_value279
                index__148 = t1074
                continue
            } else {
                break Loop_loop1070
            }
        }
        var t1058 bool = next__150 > 53
        var jp1032 bool
        if t1058 {
            jp1032 = true
        } else {
            var t1061 bool = next__150 == 53
            if t1061 {
                if trailing__151 {
                    jp1032 = true
                } else {
                    var t1064 int
                    var inline1954 int = vec_len__Vec_5uint8(output__147)
                    t1064 = inline1954
                    var t1065 int = t1064 - 1
                    var t1066 uint8 = vec_get__Vec_5uint8(output__147, t1065)
                    var t1067 uint8 = t1066 - 48
                    var t1068_rhs uint8 = 2
                    var t1068 uint8 = t1067 % t1068_rhs
                    var t1069 bool = t1068 == 1
                    jp1032 = t1069
                }
            } else {
                jp1032 = false
            }
        }
        if jp1032 {
            var index__153 int
            var inline1965 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline1965
            Loop_loop1046:
            for {
                var t1047 bool = index__153 > 0
                if t1047 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1048 int = compound_old282 - compound_value283
                    index__153 = t1048
                    var t1051 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1052 bool = t1051 < 57
                    if t1052 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1053 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1053)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1055 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1055
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1046
                }
            }
            var t1036 int
            var inline1963 int = vec_len__Vec_5uint8(output__147)
            t1036 = inline1963
            var t1037 int = t1036 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1037)
            var inline1960 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline1960)
            index__153 = 0
            Loop_loop1040:
            for {
                var t1041 int
                var inline1958 int = vec_len__Vec_5uint8(output__147)
                t1041 = inline1958
                var t1042 bool = index__153 < t1041
                if t1042 {
                    var t1043 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t1043)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t1044 int = compound_old302 + compound_value303
                    index__153 = t1044
                    continue
                } else {
                    break Loop_loop1040
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t1039 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t1039
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1057 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1057
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline1976 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline1976
    Loop_loop1090:
    for {
        var t1095 bool = length__159 > 1
        var jp1092 bool
        if t1095 {
            var t1096 int = length__159 - 1
            var t1097 uint8
            var inline1971 uint8 = _goml_runtime_core_string_byte_get(value__158, t1096)
            t1097 = inline1971
            var t1098 bool = t1097 == 48
            jp1092 = t1098
        } else {
            jp1092 = false
        }
        if jp1092 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1093 int = compound_old312 - compound_value313
            length__159 = t1093
            continue
        } else {
            break Loop_loop1090
        }
    }
    var inline1973 int = 0
    var inline1974 string = string_byte_slice(value__158, inline1973, length__159)
    return inline1974
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline1978 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline1978)
    } else {}
    var t1103 bool = decimal_point__138 <= 0
    if t1103 {
        var inline1993 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline1993)
        var inline1990 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline1990)
        var index__141 int = 0
        var t1113 int = 0 - decimal_point__138
        Loop_loop1112:
        for {
            var t1114 bool = index__141 < t1113
            if t1114 {
                var inline1981 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline1981)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1115 int = compound_old234 + compound_value235
                index__141 = t1115
                continue
            } else {
                break Loop_loop1112
            }
        }
        index__141 = 0
        Loop_loop1106:
        for {
            var t1107 int
            var inline1988 int = _goml_runtime_core_string_len(digits__137)
            t1107 = inline1988
            var t1108 bool = index__141 < t1107
            if t1108 {
                var t1109 uint8
                var inline1986 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1109 = inline1986
                vec_push__Vec_5uint8(bytes__140, t1109)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1110 int = compound_old240 + compound_value241
                index__141 = t1110
                continue
            } else {
                break Loop_loop1106
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1118 int
        var inline2018 int = _goml_runtime_core_string_len(digits__137)
        t1118 = inline2018
        var t1119 bool = decimal_point__138 >= t1118
        if t1119 {
            var index__142 int = 0
            Loop_loop1126:
            for {
                var t1127 int
                var inline2000 int = _goml_runtime_core_string_len(digits__137)
                t1127 = inline2000
                var t1128 bool = index__142 < t1127
                if t1128 {
                    var t1129 uint8
                    var inline1998 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1129 = inline1998
                    vec_push__Vec_5uint8(bytes__140, t1129)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1130 int = compound_old244 + compound_value245
                    index__142 = t1130
                    continue
                } else {
                    break Loop_loop1126
                }
            }
            Loop_loop1122:
            for {
                var t1123 bool = index__142 < decimal_point__138
                if t1123 {
                    var inline2002 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline2002)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1124 int = compound_old249 + compound_value250
                    index__142 = t1124
                    continue
                } else {
                    break Loop_loop1122
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1140:
            for {
                var t1141 bool = index__143 < decimal_point__138
                if t1141 {
                    var t1142 uint8
                    var inline2007 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1142 = inline2007
                    vec_push__Vec_5uint8(bytes__140, t1142)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1143 int = compound_old253 + compound_value254
                    index__143 = t1143
                    continue
                } else {
                    break Loop_loop1140
                }
            }
            var inline2015 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline2015)
            Loop_loop1134:
            for {
                var t1135 int
                var inline2013 int = _goml_runtime_core_string_len(digits__137)
                t1135 = inline2013
                var t1136 bool = index__143 < t1135
                if t1136 {
                    var t1137 uint8
                    var inline2011 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1137 = inline2011
                    vec_push__Vec_5uint8(bytes__140, t1137)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1138 int = compound_old259 + compound_value260
                    index__143 = t1138
                    continue
                } else {
                    break Loop_loop1134
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
    var t1239 bool = parsed__110.valid
    var t1240 bool = !t1239
    if t1240 {
        var t1241 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t1241
    } else {
        var t1233 bool = parsed__110.negative
        var jp1150 uint64
        if t1233 {
            var t1238 bool = mantissa_bits__108 == 23
            var jp1235 int
            if t1238 {
                jp1235 = 8
            } else {
                jp1235 = 11
            }
            var t1236 int = mantissa_bits__108 + jp1235
            var t1237_lhs uint64 = 1
            var t1237 uint64 = t1237_lhs << t1236
            jp1150 = t1237
        } else {
            jp1150 = 0
        }
        var t1232 bool = mantissa_bits__108 == 23
        var jp1152 int
        if t1232 {
            jp1152 = 8
        } else {
            jp1152 = 11
        }
        var t1153_lhs uint64 = 1
        var t1153 uint64 = t1153_lhs << jp1152
        var t1154 uint64 = t1153 - 1
        var exponent_mask__112 uint64 = t1154 << mantissa_bits__108
        var t1210 int = parsed__110.special
        var t1211 bool = t1210 == 1
        if t1211 {
            var t1212 uint64 = jp1150 | exponent_mask__112
            var t1213 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t1212,
            }
            return t1213
        } else {
            var t1215 int = parsed__110.special
            var t1216 bool = t1215 == 2
            if t1216 {
                var t1220 int = mantissa_bits__108 - 1
                var t1221_lhs uint64 = 1
                var t1221 uint64 = t1221_lhs << t1220
                var t1222 uint64 = exponent_mask__112 | t1221
                var t1227 bool = mantissa_bits__108 == 52
                var jp1224 uint64
                if t1227 {
                    jp1224 = 1
                } else {
                    jp1224 = 0
                }
                var t1225 uint64 = t1222 | jp1224
                var t1226 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t1225,
                }
                return t1226
            } else {
                var t1229 FloatNatural = parsed__110.numerator
                var t1230 bool
                var inline2020 *_goml_vec_uint32 = t1229.words
                var inline2021 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2020)
                t1230 = inline2021
                if t1230 {
                    var t1231 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp1150,
                    }
                    return t1231
                } else {
                    var t1193 bool = parsed__110.hexadecimal
                    var t1194 bool = !t1193
                    if t1194 {
                        var t1195 int = parsed__110.significant_digits
                        var t1196 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t1195 + t1196
                        var t1209 bool = mantissa_bits__108 == 23
                        var jp1198 int
                        if t1209 {
                            jp1198 = 40
                        } else {
                            jp1198 = 310
                        }
                        var t1208 bool = mantissa_bits__108 == 23
                        var jp1200 int
                        if t1208 {
                            jp1200 = -46
                        } else {
                            jp1200 = -325
                        }
                        var t1202 bool = decimal_position__113 > jp1198
                        if t1202 {
                            var t1203 uint64 = jp1150 | exponent_mask__112
                            var t1204 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t1203,
                            }
                            return t1204
                        } else {
                            var t1206 bool = decimal_position__113 < jp1200
                            if t1206 {
                                var t1207 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp1150,
                                }
                                return t1207
                            } else {
                                var t1189 bool = parsed__110.hexadecimal
                                var t1190 bool = !t1189
                                var jp1184 bool
                                if t1190 {
                                    var t1191 int = parsed__110.decimal_exponent
                                    var t1192 bool = t1191 < 0
                                    jp1184 = t1192
                                } else {
                                    jp1184 = false
                                }
                                var jp1158 FloatNatural
                                if jp1184 {
                                    var t1185 int = parsed__110.decimal_exponent
                                    var t1186 int = 0 - t1185
                                    var t1187 FloatNatural = float_natural_power5(t1186)
                                    jp1158 = t1187
                                } else {
                                    var inline2023 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline2023, 1)
                                    var inline2025 FloatNatural = FloatNatural{
                                        words: inline2023,
                                    }
                                    jp1158 = inline2025
                                }
                                var t1179 bool = parsed__110.hexadecimal
                                var t1180 bool = !t1179
                                var jp1170 bool
                                if t1180 {
                                    var t1181 int = parsed__110.decimal_exponent
                                    var t1182 bool = t1181 > 0
                                    jp1170 = t1182
                                } else {
                                    jp1170 = false
                                }
                                var jp1160 FloatNatural
                                if jp1170 {
                                    var t1171 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t1171)
                                    var count__118 int = 0
                                    Loop_loop1173:
                                    for {
                                        var t1174 int = parsed__110.decimal_exponent
                                        var t1175 bool = count__118 < t1174
                                        if t1175 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t1176 int = compound_old213 + compound_value214
                                            count__118 = t1176
                                            continue
                                        } else {
                                            break Loop_loop1173
                                        }
                                    }
                                    jp1160 = result__117
                                    var t1166 bool = parsed__110.hexadecimal
                                    var jp1162 int
                                    if t1166 {
                                        var t1167 int = parsed__110.binary_exponent
                                        jp1162 = t1167
                                    } else {
                                        var t1168 int = parsed__110.decimal_exponent
                                        jp1162 = t1168
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1160, jp1158, jp1162, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1163 bool = !x219
                                    var t1164 uint64 = jp1150 | x218
                                    var t1165 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1163,
                                        _1: t1164,
                                    }
                                    return t1165
                                } else {
                                    var t1178 FloatNatural = parsed__110.numerator
                                    jp1160 = t1178
                                    var t1166 bool = parsed__110.hexadecimal
                                    var jp1162 int
                                    if t1166 {
                                        var t1167 int = parsed__110.binary_exponent
                                        jp1162 = t1167
                                    } else {
                                        var t1168 int = parsed__110.decimal_exponent
                                        jp1162 = t1168
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1160, jp1158, jp1162, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1163 bool = !x219
                                    var t1164 uint64 = jp1150 | x218
                                    var t1165 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1163,
                                        _1: t1164,
                                    }
                                    return t1165
                                }
                            }
                        }
                    } else {
                        var t1189 bool = parsed__110.hexadecimal
                        var t1190 bool = !t1189
                        var jp1184 bool
                        if t1190 {
                            var t1191 int = parsed__110.decimal_exponent
                            var t1192 bool = t1191 < 0
                            jp1184 = t1192
                        } else {
                            jp1184 = false
                        }
                        var jp1158 FloatNatural
                        if jp1184 {
                            var t1185 int = parsed__110.decimal_exponent
                            var t1186 int = 0 - t1185
                            var t1187 FloatNatural = float_natural_power5(t1186)
                            jp1158 = t1187
                        } else {
                            var inline2023 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline2023, 1)
                            var inline2025 FloatNatural = FloatNatural{
                                words: inline2023,
                            }
                            jp1158 = inline2025
                        }
                        var t1179 bool = parsed__110.hexadecimal
                        var t1180 bool = !t1179
                        var jp1170 bool
                        if t1180 {
                            var t1181 int = parsed__110.decimal_exponent
                            var t1182 bool = t1181 > 0
                            jp1170 = t1182
                        } else {
                            jp1170 = false
                        }
                        var jp1160 FloatNatural
                        if jp1170 {
                            var t1171 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t1171)
                            var count__118 int = 0
                            Loop_loop1173__2:
                            for {
                                var t1174 int = parsed__110.decimal_exponent
                                var t1175 bool = count__118 < t1174
                                if t1175 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t1176 int = compound_old213 + compound_value214
                                    count__118 = t1176
                                    continue
                                } else {
                                    break Loop_loop1173__2
                                }
                            }
                            jp1160 = result__117
                            var t1166 bool = parsed__110.hexadecimal
                            var jp1162 int
                            if t1166 {
                                var t1167 int = parsed__110.binary_exponent
                                jp1162 = t1167
                            } else {
                                var t1168 int = parsed__110.decimal_exponent
                                jp1162 = t1168
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1160, jp1158, jp1162, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1163 bool = !x219
                            var t1164 uint64 = jp1150 | x218
                            var t1165 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1163,
                                _1: t1164,
                            }
                            return t1165
                        } else {
                            var t1178 FloatNatural = parsed__110.numerator
                            jp1160 = t1178
                            var t1166 bool = parsed__110.hexadecimal
                            var jp1162 int
                            if t1166 {
                                var t1167 int = parsed__110.binary_exponent
                                jp1162 = t1167
                            } else {
                                var t1168 int = parsed__110.decimal_exponent
                                jp1162 = t1168
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1160, jp1158, jp1162, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1163 bool = !x219
                            var t1164 uint64 = jp1150 | x218
                            var t1165 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1163,
                                _1: t1164,
                            }
                            return t1165
                        }
                    }
                }
            }
        }
    }
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1263 bool = factor__16 == 0
    if t1263 {
        var t1264 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1264, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1257 uint64 = uint64(uint32(factor__16))
        Loop_loop1250:
        for {
            var t1251 *_goml_vec_uint32 = value__15.words
            var t1252 int
            var inline2029 int = vec_len__Vec_6uint32(t1251)
            t1252 = inline2029
            var t1253 bool = index__18 < t1252
            if t1253 {
                var t1254 *_goml_vec_uint32 = value__15.words
                var t1255 uint32 = vec_get__Vec_6uint32(t1254, index__18)
                var t1256 uint64 = uint64(uint32(t1255))
                var t1258 uint64 = t1256 * t1257
                var product__19 uint64 = t1258 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1260_rhs int = 32
                var t1260 uint64 = product__19 >> t1260_rhs
                carry__17 = t1260
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1261 int = compound_old30 + compound_value31
                index__18 = t1261
                continue
            } else {
                break Loop_loop1250
            }
        }
        var t1246 bool = carry__17 != 0
        if t1246 {
            var t1247 *_goml_vec_uint32 = value__15.words
            var t1248 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1247, t1248)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_natural_zero() FloatNatural {
    var t1267 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1268 FloatNatural = FloatNatural{
        words: t1267,
    }
    return t1268
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline2040 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2041 FloatNatural = FloatNatural{
        words: inline2040,
    }
    result__5 = inline2041
    var index__6 int = 0
    Loop_loop1278:
    for {
        var t1279 *_goml_vec_uint32 = value__4.words
        var t1280 int
        var inline2038 int = vec_len__Vec_6uint32(t1279)
        t1280 = inline2038
        var t1281 bool = index__6 < t1280
        if t1281 {
            var t1282 *_goml_vec_uint32 = result__5.words
            var t1283 *_goml_vec_uint32 = value__4.words
            var t1284 uint32 = vec_get__Vec_6uint32(t1283, index__6)
            vec_push__Vec_6uint32(t1282, t1284)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1285 int = compound_old4 + compound_value5
            index__6 = t1285
            continue
        } else {
            break Loop_loop1278
        }
    }
    return result__5
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1292 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline2043 int = vec_len__Vec_6uint32(t1292)
    index__47 = inline2043
    var t1303 uint64 = uint64(uint32(divisor__45))
    var t1306 uint64 = uint64(uint32(divisor__45))
    Loop_loop1295:
    for {
        var t1296 bool = index__47 > 0
        if t1296 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1297 int = compound_old83 - compound_value84
            index__47 = t1297
            var t1299_rhs int = 32
            var t1299 uint64 = remainder__46 << t1299_rhs
            var t1300 *_goml_vec_uint32 = value__44.words
            var t1301 uint32 = vec_get__Vec_6uint32(t1300, index__47)
            var t1302 uint64 = uint64(uint32(t1301))
            var current__48 uint64 = t1299 | t1302
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1304 uint64 = current__48 / t1303
            var value90 uint32 = uint32(uint64(t1304))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1307 uint64 = current__48 % t1306
            remainder__46 = t1307
            continue
        } else {
            break Loop_loop1295
        }
    }
    float_natural_trim(value__44)
    var t1294 uint32 = uint32(uint64(remainder__46))
    return t1294
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1310 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1310
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline2045 bool = string_is_char_boundary(self__294, start__295)
    var inline2047 bool
    if inline2045 {
        var inline2050 bool = string_is_char_boundary(self__294, end__296)
        inline2047 = inline2050
    } else {
        inline2047 = false
    }
    if inline2047 {
        var inline2048 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline2048
    } else {
        var inline2049 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2049
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1498 bool = string_equals_ascii_case(value__84, "nan")
    if t1498 {
        var t1499 FloatNatural
        var inline2052 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2053 FloatNatural = FloatNatural{
            words: inline2052,
        }
        t1499 = inline2053
        var t1500 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1499,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1500
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1490 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1491 bool = index__85 < t1490
        var jp1485 bool
        if t1491 {
            var t1494 uint8
            var inline2057 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1494 = inline2057
            var t1495 bool = t1494 == 43
            if t1495 {
                jp1485 = true
            } else {
                var t1496 uint8
                var inline2055 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1496 = inline2055
                var t1497 bool = t1496 == 45
                jp1485 = t1497
            }
        } else {
            jp1485 = false
        }
        if jp1485 {
            var t1486 uint8
            var inline2059 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1486 = inline2059
            var t1487 bool = t1486 == 45
            negative__86 = t1487
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1488 int = compound_old140 + compound_value141
            index__85 = t1488
        } else {}
        var t1318 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1318)
        var t1482 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1479 bool
        if t1482 {
            jp1479 = true
        } else {
            var t1483 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1479 = t1483
        }
        if jp1479 {
            var t1480 FloatNatural
            var inline2061 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2062 FloatNatural = FloatNatural{
                words: inline2061,
            }
            t1480 = inline2062
            var t1481 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1480,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1481
        } else {
            var t1473 int = index__85 + 2
            var t1474 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1475 bool = t1473 <= t1474
            var jp1468 bool
            if t1475 {
                var t1476 uint8
                var inline2064 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1476 = inline2064
                var t1477 bool = t1476 == 48
                jp1468 = t1477
            } else {
                jp1468 = false
            }
            var jp1321 bool
            if jp1468 {
                var t1469 int = index__85 + 1
                var t1470 uint8
                var inline2073 uint8 = _goml_runtime_core_string_byte_get(value__84, t1469)
                t1470 = inline2073
                var t1471 uint8
                var inline2066 bool = t1470 >= 65
                var inline2068 bool
                if inline2066 {
                    var inline2071 bool = t1470 <= 90
                    inline2068 = inline2071
                } else {
                    inline2068 = false
                }
                if inline2068 {
                    var inline2069 uint8 = 97 - 65
                    var inline2070 uint8 = t1470 + inline2069
                    t1471 = inline2070
                    var t1472 bool = t1471 == 120
                    jp1321 = t1472
                    if jp1321 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1465 int = compound_old145 + compound_value146
                        index__85 = t1465
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1324 int
                    if jp1321 {
                        jp1324 = 16
                    } else {
                        jp1324 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1418 uint32 = uint32(int(jp1324))
                    Loop_loop1414:
                    for {
                        var t1415 int
                        var inline2087 int = _goml_runtime_core_string_len(value__84)
                        t1415 = inline2087
                        var t1416 bool = index__85 < t1415
                        if t1416 {
                            var current__97 uint8
                            var inline2085 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2085
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1324)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1418)
                                var t1419 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1419)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1430 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1430
                                } else {}
                                var t1428 bool = significant_digits__95 > 0
                                var jp1425 bool
                                if t1428 {
                                    jp1425 = true
                                } else {
                                    var t1429 bool = x151 != 0
                                    jp1425 = t1429
                                }
                                if jp1425 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1426 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1426
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1422 int = compound_old164 + compound_value165
                                index__85 = t1422
                                continue
                            } else {
                                var t1433 bool = current__97 == 95
                                if t1433 {
                                    var t1454 int = index__85 + 1
                                    var t1455 int
                                    var inline2083 int = _goml_runtime_core_string_len(value__84)
                                    t1455 = inline2083
                                    var t1456 bool = t1454 >= t1455
                                    if t1456 {
                                        var inline2075 FloatNatural = float_natural_zero()
                                        var inline2076 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2075,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2076
                                    } else {
                                        var t1435 int = index__85 + 1
                                        var t1436 uint8
                                        var inline2081 uint8 = _goml_runtime_core_string_byte_get(value__84, t1435)
                                        t1436 = inline2081
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1436, jp1324)
                                        var x169 bool = mtmp168._0
                                        var jp1451 bool
                                        if jp1321 {
                                            var t1453 bool = !saw_digit__92
                                            jp1451 = t1453
                                        } else {
                                            jp1451 = false
                                        }
                                        var jp1438 bool
                                        if jp1451 {
                                            var t1452 bool = index__85 == mantissa_start__89
                                            jp1438 = t1452
                                        } else {
                                            jp1438 = false
                                        }
                                        var t1448 bool = !previous_digit__96
                                        var jp1446 bool
                                        if t1448 {
                                            var t1449 bool = !jp1438
                                            jp1446 = t1449
                                        } else {
                                            jp1446 = false
                                        }
                                        var jp1443 bool
                                        if jp1446 {
                                            jp1443 = true
                                        } else {
                                            var t1447 bool = !x169
                                            jp1443 = t1447
                                        }
                                        if jp1443 {
                                            var inline2078 FloatNatural = float_natural_zero()
                                            var inline2079 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2078,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2079
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1440 int = compound_old173 + compound_value174
                                            index__85 = t1440
                                            continue
                                        }
                                    }
                                } else {
                                    var t1463 bool = current__97 == 46
                                    var jp1460 bool
                                    if t1463 {
                                        var t1464 bool = !saw_dot__93
                                        jp1460 = t1464
                                    } else {
                                        jp1460 = false
                                    }
                                    if jp1460 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1461 int = compound_old178 + compound_value179
                                        index__85 = t1461
                                        continue
                                    } else {
                                        break Loop_loop1414
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1414
                        }
                    }
                    var t1412 bool = !saw_digit__92
                    if t1412 {
                        var inline2089 FloatNatural = float_natural_zero()
                        var inline2090 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2089,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2090
                    } else {
                        var jp1328 uint8
                        if jp1321 {
                            jp1328 = 112
                        } else {
                            jp1328 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1407 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1408 bool = index__85 < t1407
                        var jp1345 bool
                        if t1408 {
                            var t1409 uint8
                            var inline2092 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1409 = inline2092
                            var t1410 uint8 = ascii_lower(t1409)
                            var t1411 bool = t1410 == jp1328
                            jp1345 = t1411
                        } else {
                            jp1345 = false
                        }
                        if jp1345 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1346 int = compound_old183 + compound_value184
                            index__85 = t1346
                            var t1397 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1398 bool = index__85 < t1397
                            var jp1392 bool
                            if t1398 {
                                var t1401 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1402 bool = t1401 == 43
                                if t1402 {
                                    jp1392 = true
                                } else {
                                    var t1403 uint8
                                    var inline2094 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1403 = inline2094
                                    var t1404 bool = t1403 == 45
                                    jp1392 = t1404
                                }
                            } else {
                                jp1392 = false
                            }
                            if jp1392 {
                                var t1393 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1394 bool = t1393 == 45
                                exponent_negative__104 = t1394
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1395 int = compound_old187 + compound_value188
                                index__85 = t1395
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1353:
                            for {
                                var t1354 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1355 bool = index__85 < t1354
                                if t1355 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1389 bool = current__106 >= 48
                                    var jp1358 bool
                                    if t1389 {
                                        var t1390 bool = current__106 <= 57
                                        jp1358 = t1390
                                    } else {
                                        jp1358 = false
                                    }
                                    if jp1358 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1362 bool = exponent__103 < 1000000
                                        if t1362 {
                                            var t1363 int = exponent__103 * 10
                                            var t1364 uint8 = current__106 - 48
                                            var t1365 int = int(uint8(t1364))
                                            var t1366 int = t1363 + t1365
                                            exponent__103 = t1366
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1360 int = compound_old196 + compound_value197
                                        index__85 = t1360
                                        continue
                                    } else {
                                        var t1368 bool = current__106 == 95
                                        if t1368 {
                                            var t1385 bool = !previous_digit__96
                                            var jp1381 bool
                                            if t1385 {
                                                jp1381 = true
                                            } else {
                                                var t1386 int = index__85 + 1
                                                var t1387 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1388 bool = t1386 >= t1387
                                                jp1381 = t1388
                                            }
                                            var jp1376 bool
                                            if jp1381 {
                                                jp1376 = true
                                            } else {
                                                var t1382 int = index__85 + 1
                                                var t1383 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1382)
                                                var t1384 bool = t1383 < 48
                                                jp1376 = t1384
                                            }
                                            var jp1373 bool
                                            if jp1376 {
                                                jp1373 = true
                                            } else {
                                                var t1377 int = index__85 + 1
                                                var t1378 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1377)
                                                var t1379 bool = t1378 > 57
                                                jp1373 = t1379
                                            }
                                            if jp1373 {
                                                var t1374 ParsedFloat = invalid_parsed_float()
                                                return t1374
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1370 int = compound_old201 + compound_value202
                                                index__85 = t1370
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1353
                                        }
                                    }
                                } else {
                                    break Loop_loop1353
                                }
                            }
                            var t1351 bool = !exponent_digits__105
                            if t1351 {
                                var t1352 ParsedFloat = invalid_parsed_float()
                                return t1352
                            } else {
                                var t1341 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1342 bool = index__85 != t1341
                                if t1342 {
                                    var t1343 ParsedFloat = invalid_parsed_float()
                                    return t1343
                                } else {
                                    if exponent_negative__104 {
                                        var t1340 int = 0 - exponent__103
                                        exponent__103 = t1340
                                    } else {}
                                    var jp1333 int
                                    if jp1321 {
                                        jp1333 = 0
                                    } else {
                                        var t1339 int = exponent__103 - fraction_digits__94
                                        jp1333 = t1339
                                    }
                                    var jp1335 int
                                    if jp1321 {
                                        var t1337 int = fraction_digits__94 * 4
                                        var t1338 int = exponent__103 - t1337
                                        jp1335 = t1338
                                    } else {
                                        jp1335 = 0
                                    }
                                    var t1336 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1333,
                                        binary_exponent: jp1335,
                                        hexadecimal: jp1321,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1336
                                }
                            }
                        } else {
                            if jp1321 {
                                var t1406 ParsedFloat = invalid_parsed_float()
                                return t1406
                            } else {
                                var t1341 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1342 bool = index__85 != t1341
                                if t1342 {
                                    var t1343 ParsedFloat = invalid_parsed_float()
                                    return t1343
                                } else {
                                    if exponent_negative__104 {
                                        var t1340 int = 0 - exponent__103
                                        exponent__103 = t1340
                                    } else {}
                                    var jp1333 int
                                    if jp1321 {
                                        jp1333 = 0
                                    } else {
                                        var t1339 int = exponent__103 - fraction_digits__94
                                        jp1333 = t1339
                                    }
                                    var jp1335 int
                                    if jp1321 {
                                        var t1337 int = fraction_digits__94 * 4
                                        var t1338 int = exponent__103 - t1337
                                        jp1335 = t1338
                                    } else {
                                        jp1335 = 0
                                    }
                                    var t1336 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1333,
                                        binary_exponent: jp1335,
                                        hexadecimal: jp1321,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1336
                                }
                            }
                        }
                    }
                } else {
                    t1471 = t1470
                    var t1472 bool = t1471 == 120
                    jp1321 = t1472
                    if jp1321 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1465 int = compound_old145 + compound_value146
                        index__85 = t1465
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1324 int
                    if jp1321 {
                        jp1324 = 16
                    } else {
                        jp1324 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1418 uint32 = uint32(int(jp1324))
                    Loop_loop1414__2:
                    for {
                        var t1415 int
                        var inline2087 int = _goml_runtime_core_string_len(value__84)
                        t1415 = inline2087
                        var t1416 bool = index__85 < t1415
                        if t1416 {
                            var current__97 uint8
                            var inline2085 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2085
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1324)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1418)
                                var t1419 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1419)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1430 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1430
                                } else {}
                                var t1428 bool = significant_digits__95 > 0
                                var jp1425 bool
                                if t1428 {
                                    jp1425 = true
                                } else {
                                    var t1429 bool = x151 != 0
                                    jp1425 = t1429
                                }
                                if jp1425 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1426 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1426
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1422 int = compound_old164 + compound_value165
                                index__85 = t1422
                                continue
                            } else {
                                var t1433 bool = current__97 == 95
                                if t1433 {
                                    var t1454 int = index__85 + 1
                                    var t1455 int
                                    var inline2083 int = _goml_runtime_core_string_len(value__84)
                                    t1455 = inline2083
                                    var t1456 bool = t1454 >= t1455
                                    if t1456 {
                                        var inline2075 FloatNatural = float_natural_zero()
                                        var inline2076 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2075,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2076
                                    } else {
                                        var t1435 int = index__85 + 1
                                        var t1436 uint8
                                        var inline2081 uint8 = _goml_runtime_core_string_byte_get(value__84, t1435)
                                        t1436 = inline2081
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1436, jp1324)
                                        var x169 bool = mtmp168._0
                                        var jp1451 bool
                                        if jp1321 {
                                            var t1453 bool = !saw_digit__92
                                            jp1451 = t1453
                                        } else {
                                            jp1451 = false
                                        }
                                        var jp1438 bool
                                        if jp1451 {
                                            var t1452 bool = index__85 == mantissa_start__89
                                            jp1438 = t1452
                                        } else {
                                            jp1438 = false
                                        }
                                        var t1448 bool = !previous_digit__96
                                        var jp1446 bool
                                        if t1448 {
                                            var t1449 bool = !jp1438
                                            jp1446 = t1449
                                        } else {
                                            jp1446 = false
                                        }
                                        var jp1443 bool
                                        if jp1446 {
                                            jp1443 = true
                                        } else {
                                            var t1447 bool = !x169
                                            jp1443 = t1447
                                        }
                                        if jp1443 {
                                            var inline2078 FloatNatural = float_natural_zero()
                                            var inline2079 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2078,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2079
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1440 int = compound_old173 + compound_value174
                                            index__85 = t1440
                                            continue
                                        }
                                    }
                                } else {
                                    var t1463 bool = current__97 == 46
                                    var jp1460 bool
                                    if t1463 {
                                        var t1464 bool = !saw_dot__93
                                        jp1460 = t1464
                                    } else {
                                        jp1460 = false
                                    }
                                    if jp1460 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1461 int = compound_old178 + compound_value179
                                        index__85 = t1461
                                        continue
                                    } else {
                                        break Loop_loop1414__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1414__2
                        }
                    }
                    var t1412 bool = !saw_digit__92
                    if t1412 {
                        var inline2089 FloatNatural = float_natural_zero()
                        var inline2090 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2089,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2090
                    } else {
                        var jp1328 uint8
                        if jp1321 {
                            jp1328 = 112
                        } else {
                            jp1328 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1407 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1408 bool = index__85 < t1407
                        var jp1345 bool
                        if t1408 {
                            var t1409 uint8
                            var inline2092 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1409 = inline2092
                            var t1410 uint8 = ascii_lower(t1409)
                            var t1411 bool = t1410 == jp1328
                            jp1345 = t1411
                        } else {
                            jp1345 = false
                        }
                        if jp1345 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1346 int = compound_old183 + compound_value184
                            index__85 = t1346
                            var t1397 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1398 bool = index__85 < t1397
                            var jp1392 bool
                            if t1398 {
                                var t1401 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1402 bool = t1401 == 43
                                if t1402 {
                                    jp1392 = true
                                } else {
                                    var t1403 uint8
                                    var inline2094 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1403 = inline2094
                                    var t1404 bool = t1403 == 45
                                    jp1392 = t1404
                                }
                            } else {
                                jp1392 = false
                            }
                            if jp1392 {
                                var t1393 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1394 bool = t1393 == 45
                                exponent_negative__104 = t1394
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1395 int = compound_old187 + compound_value188
                                index__85 = t1395
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1353__2:
                            for {
                                var t1354 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1355 bool = index__85 < t1354
                                if t1355 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1389 bool = current__106 >= 48
                                    var jp1358 bool
                                    if t1389 {
                                        var t1390 bool = current__106 <= 57
                                        jp1358 = t1390
                                    } else {
                                        jp1358 = false
                                    }
                                    if jp1358 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1362 bool = exponent__103 < 1000000
                                        if t1362 {
                                            var t1363 int = exponent__103 * 10
                                            var t1364 uint8 = current__106 - 48
                                            var t1365 int = int(uint8(t1364))
                                            var t1366 int = t1363 + t1365
                                            exponent__103 = t1366
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1360 int = compound_old196 + compound_value197
                                        index__85 = t1360
                                        continue
                                    } else {
                                        var t1368 bool = current__106 == 95
                                        if t1368 {
                                            var t1385 bool = !previous_digit__96
                                            var jp1381 bool
                                            if t1385 {
                                                jp1381 = true
                                            } else {
                                                var t1386 int = index__85 + 1
                                                var t1387 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1388 bool = t1386 >= t1387
                                                jp1381 = t1388
                                            }
                                            var jp1376 bool
                                            if jp1381 {
                                                jp1376 = true
                                            } else {
                                                var t1382 int = index__85 + 1
                                                var t1383 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1382)
                                                var t1384 bool = t1383 < 48
                                                jp1376 = t1384
                                            }
                                            var jp1373 bool
                                            if jp1376 {
                                                jp1373 = true
                                            } else {
                                                var t1377 int = index__85 + 1
                                                var t1378 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1377)
                                                var t1379 bool = t1378 > 57
                                                jp1373 = t1379
                                            }
                                            if jp1373 {
                                                var t1374 ParsedFloat = invalid_parsed_float()
                                                return t1374
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1370 int = compound_old201 + compound_value202
                                                index__85 = t1370
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1353__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1353__2
                                }
                            }
                            var t1351 bool = !exponent_digits__105
                            if t1351 {
                                var t1352 ParsedFloat = invalid_parsed_float()
                                return t1352
                            } else {
                                var t1341 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1342 bool = index__85 != t1341
                                if t1342 {
                                    var t1343 ParsedFloat = invalid_parsed_float()
                                    return t1343
                                } else {
                                    if exponent_negative__104 {
                                        var t1340 int = 0 - exponent__103
                                        exponent__103 = t1340
                                    } else {}
                                    var jp1333 int
                                    if jp1321 {
                                        jp1333 = 0
                                    } else {
                                        var t1339 int = exponent__103 - fraction_digits__94
                                        jp1333 = t1339
                                    }
                                    var jp1335 int
                                    if jp1321 {
                                        var t1337 int = fraction_digits__94 * 4
                                        var t1338 int = exponent__103 - t1337
                                        jp1335 = t1338
                                    } else {
                                        jp1335 = 0
                                    }
                                    var t1336 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1333,
                                        binary_exponent: jp1335,
                                        hexadecimal: jp1321,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1336
                                }
                            }
                        } else {
                            if jp1321 {
                                var t1406 ParsedFloat = invalid_parsed_float()
                                return t1406
                            } else {
                                var t1341 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1342 bool = index__85 != t1341
                                if t1342 {
                                    var t1343 ParsedFloat = invalid_parsed_float()
                                    return t1343
                                } else {
                                    if exponent_negative__104 {
                                        var t1340 int = 0 - exponent__103
                                        exponent__103 = t1340
                                    } else {}
                                    var jp1333 int
                                    if jp1321 {
                                        jp1333 = 0
                                    } else {
                                        var t1339 int = exponent__103 - fraction_digits__94
                                        jp1333 = t1339
                                    }
                                    var jp1335 int
                                    if jp1321 {
                                        var t1337 int = fraction_digits__94 * 4
                                        var t1338 int = exponent__103 - t1337
                                        jp1335 = t1338
                                    } else {
                                        jp1335 = 0
                                    }
                                    var t1336 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1333,
                                        binary_exponent: jp1335,
                                        hexadecimal: jp1321,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1336
                                }
                            }
                        }
                    }
                }
            } else {
                jp1321 = false
                if jp1321 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1465 int = compound_old145 + compound_value146
                    index__85 = t1465
                } else {}
                var mantissa_start__89 int = index__85
                var jp1324 int
                if jp1321 {
                    jp1324 = 16
                } else {
                    jp1324 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1418 uint32 = uint32(int(jp1324))
                Loop_loop1414__3:
                for {
                    var t1415 int
                    var inline2087 int = _goml_runtime_core_string_len(value__84)
                    t1415 = inline2087
                    var t1416 bool = index__85 < t1415
                    if t1416 {
                        var current__97 uint8
                        var inline2085 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline2085
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1324)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1418)
                            var t1419 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1419)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1430 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1430
                            } else {}
                            var t1428 bool = significant_digits__95 > 0
                            var jp1425 bool
                            if t1428 {
                                jp1425 = true
                            } else {
                                var t1429 bool = x151 != 0
                                jp1425 = t1429
                            }
                            if jp1425 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1426 int = compound_old160 + compound_value161
                                significant_digits__95 = t1426
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1422 int = compound_old164 + compound_value165
                            index__85 = t1422
                            continue
                        } else {
                            var t1433 bool = current__97 == 95
                            if t1433 {
                                var t1454 int = index__85 + 1
                                var t1455 int
                                var inline2083 int = _goml_runtime_core_string_len(value__84)
                                t1455 = inline2083
                                var t1456 bool = t1454 >= t1455
                                if t1456 {
                                    var inline2075 FloatNatural = float_natural_zero()
                                    var inline2076 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline2075,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline2076
                                } else {
                                    var t1435 int = index__85 + 1
                                    var t1436 uint8
                                    var inline2081 uint8 = _goml_runtime_core_string_byte_get(value__84, t1435)
                                    t1436 = inline2081
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1436, jp1324)
                                    var x169 bool = mtmp168._0
                                    var jp1451 bool
                                    if jp1321 {
                                        var t1453 bool = !saw_digit__92
                                        jp1451 = t1453
                                    } else {
                                        jp1451 = false
                                    }
                                    var jp1438 bool
                                    if jp1451 {
                                        var t1452 bool = index__85 == mantissa_start__89
                                        jp1438 = t1452
                                    } else {
                                        jp1438 = false
                                    }
                                    var t1448 bool = !previous_digit__96
                                    var jp1446 bool
                                    if t1448 {
                                        var t1449 bool = !jp1438
                                        jp1446 = t1449
                                    } else {
                                        jp1446 = false
                                    }
                                    var jp1443 bool
                                    if jp1446 {
                                        jp1443 = true
                                    } else {
                                        var t1447 bool = !x169
                                        jp1443 = t1447
                                    }
                                    if jp1443 {
                                        var inline2078 FloatNatural = float_natural_zero()
                                        var inline2079 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2078,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2079
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1440 int = compound_old173 + compound_value174
                                        index__85 = t1440
                                        continue
                                    }
                                }
                            } else {
                                var t1463 bool = current__97 == 46
                                var jp1460 bool
                                if t1463 {
                                    var t1464 bool = !saw_dot__93
                                    jp1460 = t1464
                                } else {
                                    jp1460 = false
                                }
                                if jp1460 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1461 int = compound_old178 + compound_value179
                                    index__85 = t1461
                                    continue
                                } else {
                                    break Loop_loop1414__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1414__3
                    }
                }
                var t1412 bool = !saw_digit__92
                if t1412 {
                    var inline2089 FloatNatural = float_natural_zero()
                    var inline2090 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline2089,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline2090
                } else {
                    var jp1328 uint8
                    if jp1321 {
                        jp1328 = 112
                    } else {
                        jp1328 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1407 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1408 bool = index__85 < t1407
                    var jp1345 bool
                    if t1408 {
                        var t1409 uint8
                        var inline2092 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1409 = inline2092
                        var t1410 uint8 = ascii_lower(t1409)
                        var t1411 bool = t1410 == jp1328
                        jp1345 = t1411
                    } else {
                        jp1345 = false
                    }
                    if jp1345 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1346 int = compound_old183 + compound_value184
                        index__85 = t1346
                        var t1397 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1398 bool = index__85 < t1397
                        var jp1392 bool
                        if t1398 {
                            var t1401 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1402 bool = t1401 == 43
                            if t1402 {
                                jp1392 = true
                            } else {
                                var t1403 uint8
                                var inline2094 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1403 = inline2094
                                var t1404 bool = t1403 == 45
                                jp1392 = t1404
                            }
                        } else {
                            jp1392 = false
                        }
                        if jp1392 {
                            var t1393 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1394 bool = t1393 == 45
                            exponent_negative__104 = t1394
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1395 int = compound_old187 + compound_value188
                            index__85 = t1395
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1353__3:
                        for {
                            var t1354 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1355 bool = index__85 < t1354
                            if t1355 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1389 bool = current__106 >= 48
                                var jp1358 bool
                                if t1389 {
                                    var t1390 bool = current__106 <= 57
                                    jp1358 = t1390
                                } else {
                                    jp1358 = false
                                }
                                if jp1358 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1362 bool = exponent__103 < 1000000
                                    if t1362 {
                                        var t1363 int = exponent__103 * 10
                                        var t1364 uint8 = current__106 - 48
                                        var t1365 int = int(uint8(t1364))
                                        var t1366 int = t1363 + t1365
                                        exponent__103 = t1366
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1360 int = compound_old196 + compound_value197
                                    index__85 = t1360
                                    continue
                                } else {
                                    var t1368 bool = current__106 == 95
                                    if t1368 {
                                        var t1385 bool = !previous_digit__96
                                        var jp1381 bool
                                        if t1385 {
                                            jp1381 = true
                                        } else {
                                            var t1386 int = index__85 + 1
                                            var t1387 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1388 bool = t1386 >= t1387
                                            jp1381 = t1388
                                        }
                                        var jp1376 bool
                                        if jp1381 {
                                            jp1376 = true
                                        } else {
                                            var t1382 int = index__85 + 1
                                            var t1383 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1382)
                                            var t1384 bool = t1383 < 48
                                            jp1376 = t1384
                                        }
                                        var jp1373 bool
                                        if jp1376 {
                                            jp1373 = true
                                        } else {
                                            var t1377 int = index__85 + 1
                                            var t1378 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1377)
                                            var t1379 bool = t1378 > 57
                                            jp1373 = t1379
                                        }
                                        if jp1373 {
                                            var t1374 ParsedFloat = invalid_parsed_float()
                                            return t1374
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1370 int = compound_old201 + compound_value202
                                            index__85 = t1370
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1353__3
                                    }
                                }
                            } else {
                                break Loop_loop1353__3
                            }
                        }
                        var t1351 bool = !exponent_digits__105
                        if t1351 {
                            var t1352 ParsedFloat = invalid_parsed_float()
                            return t1352
                        } else {
                            var t1341 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1342 bool = index__85 != t1341
                            if t1342 {
                                var t1343 ParsedFloat = invalid_parsed_float()
                                return t1343
                            } else {
                                if exponent_negative__104 {
                                    var t1340 int = 0 - exponent__103
                                    exponent__103 = t1340
                                } else {}
                                var jp1333 int
                                if jp1321 {
                                    jp1333 = 0
                                } else {
                                    var t1339 int = exponent__103 - fraction_digits__94
                                    jp1333 = t1339
                                }
                                var jp1335 int
                                if jp1321 {
                                    var t1337 int = fraction_digits__94 * 4
                                    var t1338 int = exponent__103 - t1337
                                    jp1335 = t1338
                                } else {
                                    jp1335 = 0
                                }
                                var t1336 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1333,
                                    binary_exponent: jp1335,
                                    hexadecimal: jp1321,
                                    significant_digits: significant_digits__95,
                                }
                                return t1336
                            }
                        }
                    } else {
                        if jp1321 {
                            var t1406 ParsedFloat = invalid_parsed_float()
                            return t1406
                        } else {
                            var t1341 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1342 bool = index__85 != t1341
                            if t1342 {
                                var t1343 ParsedFloat = invalid_parsed_float()
                                return t1343
                            } else {
                                if exponent_negative__104 {
                                    var t1340 int = 0 - exponent__103
                                    exponent__103 = t1340
                                } else {}
                                var jp1333 int
                                if jp1321 {
                                    jp1333 = 0
                                } else {
                                    var t1339 int = exponent__103 - fraction_digits__94
                                    jp1333 = t1339
                                }
                                var jp1335 int
                                if jp1321 {
                                    var t1337 int = fraction_digits__94 * 4
                                    var t1338 int = exponent__103 - t1337
                                    jp1335 = t1338
                                } else {
                                    jp1335 = 0
                                }
                                var t1336 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1333,
                                    binary_exponent: jp1335,
                                    hexadecimal: jp1321,
                                    significant_digits: significant_digits__95,
                                }
                                return t1336
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
    var inline2096 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline2096, 1)
    var inline2098 FloatNatural = FloatNatural{
        words: inline2096,
    }
    result__26 = inline2098
    var count__27 int = 0
    Loop_loop1504:
    for {
        var t1505 bool = count__27 < exponent__25
        if t1505 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1506 int = compound_old46 + compound_value47
            count__27 = t1506
            continue
        } else {
            break Loop_loop1504
        }
    }
    return result__26
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1593 bool
    var inline2100 *_goml_vec_uint32 = numerator__65.words
    var inline2101 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2100)
    t1593 = inline2101
    if t1593 {
        var t1594 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1594
    } else {
        var t1590 bool = binary_shift__67 >= 0
        var jp1515 FloatNatural
        if t1590 {
            var t1591 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1515 = t1591
        } else {
            var t1592 FloatNatural = float_natural_copy(numerator__65)
            jp1515 = t1592
        }
        var t1586 bool = binary_shift__67 >= 0
        var jp1517 FloatNatural
        if t1586 {
            var t1587 FloatNatural = float_natural_copy(denominator__66)
            jp1517 = t1587
        } else {
            var t1588 int = 0 - binary_shift__67
            var t1589 FloatNatural = float_natural_shift_left(denominator__66, t1588)
            jp1517 = t1589
        }
        var t1518 int = float_natural_bit_length(jp1515)
        var t1519 int = float_natural_bit_length(jp1517)
        var exponent__72 int = t1518 - t1519
        var t1580 bool = exponent__72 >= 0
        var jp1521 int
        if t1580 {
            var t1581 FloatNatural = float_natural_shift_left(jp1517, exponent__72)
            var t1582 int = float_natural_compare(jp1515, t1581)
            jp1521 = t1582
        } else {
            var t1583 int = 0 - exponent__72
            var t1584 FloatNatural = float_natural_shift_left(jp1515, t1583)
            var t1585 int = float_natural_compare(t1584, jp1517)
            jp1521 = t1585
        }
        var t1577 bool = jp1521 < 0
        if t1577 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1578 int = compound_old120 - compound_value121
            exponent__72 = t1578
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1571 bool = exponent__72 > exponent_bias__69
        if t1571 {
            var t1572 int = exponent_bias__69 + exponent_bias__69
            var t1573 int = t1572 + 1
            var t1574 uint64 = uint64(int(t1573))
            var t1575 uint64 = t1574 << mantissa_bits__68
            var t1576 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1575,
                _1: true,
            }
            return t1576
        } else {
            var t1566 bool = exponent__72 < minimum_exponent__74
            var jp1525 uint64
            if t1566 {
                var t1567 int = mantissa_bits__68 - minimum_exponent__74
                var t1568 uint64 = float_rational_quotient(jp1515, jp1517, t1567)
                jp1525 = t1568
            } else {
                var t1569 int = mantissa_bits__68 - exponent__72
                var t1570 uint64 = float_rational_quotient(jp1515, jp1517, t1569)
                jp1525 = t1570
            }
            var mantissa__76 uint64 = jp1525
            var t1528 bool = exponent__72 < minimum_exponent__74
            if t1528 {
                var t1531 bool = mantissa__76 == 0
                if t1531 {
                    var t1532 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1532
                } else {
                    var t1535_lhs uint64 = 1
                    var t1535 uint64 = t1535_lhs << mantissa_bits__68
                    var t1536 bool = mantissa__76 >= t1535
                    if t1536 {
                        var t1537_lhs uint64 = 1
                        var t1537 uint64 = t1537_lhs << mantissa_bits__68
                        var t1538_lhs uint64 = 1
                        var t1538 uint64 = t1538_lhs << mantissa_bits__68
                        var t1539 uint64 = mantissa__76 - t1538
                        var t1540 uint64 = t1537 | t1539
                        var t1541 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1540,
                            _1: false,
                        }
                        return t1541
                    } else {
                        var t1542 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1542
                    }
                }
            } else {
                var t1559 int = mantissa_bits__68 + 1
                var t1560_lhs uint64 = 1
                var t1560 uint64 = t1560_lhs << t1559
                var t1561 bool = mantissa__76 >= t1560
                if t1561 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1562 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1562
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1564 int = compound_old128 + compound_value129
                    exponent__72 = t1564
                } else {}
                var t1546 bool = exponent__72 > exponent_bias__69
                if t1546 {
                    var t1547 int = exponent_bias__69 + exponent_bias__69
                    var t1548 int = t1547 + 1
                    var t1549 uint64 = uint64(int(t1548))
                    var t1550 uint64 = t1549 << mantissa_bits__68
                    var t1551 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1550,
                        _1: true,
                    }
                    return t1551
                } else {
                    var t1552 int = exponent__72 + exponent_bias__69
                    var t1553 uint64 = uint64(int(t1552))
                    var t1554 uint64 = t1553 << mantissa_bits__68
                    var t1555_lhs uint64 = 1
                    var t1555 uint64 = t1555_lhs << mantissa_bits__68
                    var t1556 uint64 = mantissa__76 - t1555
                    var t1557 uint64 = t1554 | t1556
                    var t1558 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1557,
                        _1: false,
                    }
                    return t1558
                }
            }
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1599 int = vec_len__Vec_6uint32(self__528)
    var t1600 bool = t1599 == 0
    return t1600
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1603:
    for {
        var t1611 *_goml_vec_uint32 = value__7.words
        var t1612 bool
        var inline2112 int = vec_len__Vec_6uint32(t1611)
        var inline2113 bool = inline2112 == 0
        t1612 = inline2113
        var t1613 bool = !t1612
        var jp1605 bool
        if t1613 {
            var t1614 *_goml_vec_uint32 = value__7.words
            var t1615 *_goml_vec_uint32 = value__7.words
            var t1616 int
            var inline2106 int = vec_len__Vec_6uint32(t1615)
            t1616 = inline2106
            var t1617 int = t1616 - 1
            var t1618 uint32 = vec_get__Vec_6uint32(t1614, t1617)
            var t1619 bool = t1618 == 0
            jp1605 = t1619
        } else {
            jp1605 = false
        }
        if jp1605 {
            var t1606 *_goml_vec_uint32 = value__7.words
            var t1607 *_goml_vec_uint32 = value__7.words
            var t1608 int
            var inline2110 int = vec_len__Vec_6uint32(t1607)
            t1608 = inline2110
            var t1609 int = t1608 - 1
            vec_truncate__Vec_6uint32(t1606, t1609)
            continue
        } else {
            break Loop_loop1603
        }
    }
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1628 bool = string_is_char_boundary(value__274, start__275)
    var jp1625 bool
    if t1628 {
        var t1629 bool = string_is_char_boundary(value__274, end__276)
        jp1625 = t1629
    } else {
        jp1625 = false
    }
    if jp1625 {
        var t1626 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1626
    } else {
        var t1627 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1627
    }
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1644 int
    var inline2130 int = _goml_runtime_core_string_len(value__78)
    t1644 = inline2130
    var t1645 int
    var inline2128 int = _goml_runtime_core_string_len(expected__79)
    t1645 = inline2128
    var t1646 bool = t1644 != t1645
    if t1646 {
        return false
    } else {
        var index__80 int = 0
        var inline2120 uint8 = 97 - 65
        Loop_loop1634:
        for {
            var t1635 int
            var inline2126 int = _goml_runtime_core_string_len(value__78)
            t1635 = inline2126
            var t1636 bool = index__80 < t1635
            if t1636 {
                var t1640 uint8
                var inline2124 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1640 = inline2124
                var t1641 uint8
                var inline2117 bool = t1640 >= 65
                var inline2119 bool
                if inline2117 {
                    var inline2122 bool = t1640 <= 90
                    inline2119 = inline2122
                } else {
                    inline2119 = false
                }
                if inline2119 {
                    var inline2121 uint8 = t1640 + inline2120
                    t1641 = inline2121
                    var t1642 uint8
                    var inline2115 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1642 = inline2115
                    var t1643 bool = t1641 != t1642
                    if t1643 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1638 int = compound_old134 + compound_value135
                        index__80 = t1638
                        continue
                    }
                } else {
                    t1641 = t1640
                    var t1642 uint8
                    var inline2115 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1642 = inline2115
                    var t1643 bool = t1641 != t1642
                    if t1643 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1638 int = compound_old134 + compound_value135
                        index__80 = t1638
                        continue
                    }
                }
            } else {
                break Loop_loop1634
            }
        }
        return true
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1655 bool = value__77 >= 65
    var jp1652 bool
    if t1655 {
        var t1656 bool = value__77 <= 90
        jp1652 = t1656
    } else {
        jp1652 = false
    }
    if jp1652 {
        var t1653 uint8 = 97 - 65
        var t1654 uint8 = value__77 + t1653
        return t1654
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1683 bool = value__81 >= 48
    var jp1667 bool
    if t1683 {
        var t1684 bool = value__81 <= 57
        jp1667 = t1684
    } else {
        jp1667 = false
    }
    var jp1660 int
    if jp1667 {
        var t1668 uint8 = value__81 - 48
        var t1669 int = int(uint8(t1668))
        jp1660 = t1669
        var t1663 bool = jp1660 < base__82
        if t1663 {
            var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1660,
            }
            return t1664
        } else {
            var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1665
        }
    } else {
        var t1679 uint8
        var inline2146 bool = value__81 >= 65
        var inline2148 bool
        if inline2146 {
            var inline2151 bool = value__81 <= 90
            inline2148 = inline2151
        } else {
            inline2148 = false
        }
        if inline2148 {
            var inline2149 uint8 = 97 - 65
            var inline2150 uint8 = value__81 + inline2149
            t1679 = inline2150
            var t1680 bool = t1679 >= 97
            var jp1673 bool
            if t1680 {
                var t1681 uint8
                var inline2132 bool = value__81 >= 65
                var inline2134 bool
                if inline2132 {
                    var inline2137 bool = value__81 <= 90
                    inline2134 = inline2137
                } else {
                    inline2134 = false
                }
                if inline2134 {
                    var inline2135 uint8 = 97 - 65
                    var inline2136 uint8 = value__81 + inline2135
                    t1681 = inline2136
                    var t1682 bool = t1681 <= 102
                    jp1673 = t1682
                    if jp1673 {
                        var t1674 uint8
                        var inline2139 bool = value__81 >= 65
                        var inline2141 bool
                        if inline2139 {
                            var inline2144 bool = value__81 <= 90
                            inline2141 = inline2144
                        } else {
                            inline2141 = false
                        }
                        if inline2141 {
                            var inline2142 uint8 = 97 - 65
                            var inline2143 uint8 = value__81 + inline2142
                            t1674 = inline2143
                            var t1675 uint8 = t1674 - 97
                            var t1676 uint8 = t1675 + 10
                            var t1677 int = int(uint8(t1676))
                            jp1660 = t1677
                            var t1663 bool = jp1660 < base__82
                            if t1663 {
                                var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1660,
                                }
                                return t1664
                            } else {
                                var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1665
                            }
                        } else {
                            t1674 = value__81
                            var t1675 uint8 = t1674 - 97
                            var t1676 uint8 = t1675 + 10
                            var t1677 int = int(uint8(t1676))
                            jp1660 = t1677
                            var t1663 bool = jp1660 < base__82
                            if t1663 {
                                var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1660,
                                }
                                return t1664
                            } else {
                                var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1665
                            }
                        }
                    } else {
                        var t1678 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1678
                    }
                } else {
                    t1681 = value__81
                    var t1682 bool = t1681 <= 102
                    jp1673 = t1682
                    if jp1673 {
                        var t1674 uint8
                        var inline2139 bool = value__81 >= 65
                        var inline2141 bool
                        if inline2139 {
                            var inline2144 bool = value__81 <= 90
                            inline2141 = inline2144
                        } else {
                            inline2141 = false
                        }
                        if inline2141 {
                            var inline2142 uint8 = 97 - 65
                            var inline2143 uint8 = value__81 + inline2142
                            t1674 = inline2143
                            var t1675 uint8 = t1674 - 97
                            var t1676 uint8 = t1675 + 10
                            var t1677 int = int(uint8(t1676))
                            jp1660 = t1677
                            var t1663 bool = jp1660 < base__82
                            if t1663 {
                                var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1660,
                                }
                                return t1664
                            } else {
                                var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1665
                            }
                        } else {
                            t1674 = value__81
                            var t1675 uint8 = t1674 - 97
                            var t1676 uint8 = t1675 + 10
                            var t1677 int = int(uint8(t1676))
                            jp1660 = t1677
                            var t1663 bool = jp1660 < base__82
                            if t1663 {
                                var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1660,
                                }
                                return t1664
                            } else {
                                var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1665
                            }
                        }
                    } else {
                        var t1678 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1678
                    }
                }
            } else {
                jp1673 = false
                if jp1673 {
                    var t1674 uint8
                    var inline2139 bool = value__81 >= 65
                    var inline2141 bool
                    if inline2139 {
                        var inline2144 bool = value__81 <= 90
                        inline2141 = inline2144
                    } else {
                        inline2141 = false
                    }
                    if inline2141 {
                        var inline2142 uint8 = 97 - 65
                        var inline2143 uint8 = value__81 + inline2142
                        t1674 = inline2143
                        var t1675 uint8 = t1674 - 97
                        var t1676 uint8 = t1675 + 10
                        var t1677 int = int(uint8(t1676))
                        jp1660 = t1677
                        var t1663 bool = jp1660 < base__82
                        if t1663 {
                            var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1660,
                            }
                            return t1664
                        } else {
                            var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1665
                        }
                    } else {
                        t1674 = value__81
                        var t1675 uint8 = t1674 - 97
                        var t1676 uint8 = t1675 + 10
                        var t1677 int = int(uint8(t1676))
                        jp1660 = t1677
                        var t1663 bool = jp1660 < base__82
                        if t1663 {
                            var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1660,
                            }
                            return t1664
                        } else {
                            var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1665
                        }
                    }
                } else {
                    var t1678 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1678
                }
            }
        } else {
            t1679 = value__81
            var t1680 bool = t1679 >= 97
            var jp1673 bool
            if t1680 {
                var t1681 uint8
                var inline2132 bool = value__81 >= 65
                var inline2134 bool
                if inline2132 {
                    var inline2137 bool = value__81 <= 90
                    inline2134 = inline2137
                } else {
                    inline2134 = false
                }
                if inline2134 {
                    var inline2135 uint8 = 97 - 65
                    var inline2136 uint8 = value__81 + inline2135
                    t1681 = inline2136
                    var t1682 bool = t1681 <= 102
                    jp1673 = t1682
                    if jp1673 {
                        var t1674 uint8
                        var inline2139 bool = value__81 >= 65
                        var inline2141 bool
                        if inline2139 {
                            var inline2144 bool = value__81 <= 90
                            inline2141 = inline2144
                        } else {
                            inline2141 = false
                        }
                        if inline2141 {
                            var inline2142 uint8 = 97 - 65
                            var inline2143 uint8 = value__81 + inline2142
                            t1674 = inline2143
                            var t1675 uint8 = t1674 - 97
                            var t1676 uint8 = t1675 + 10
                            var t1677 int = int(uint8(t1676))
                            jp1660 = t1677
                            var t1663 bool = jp1660 < base__82
                            if t1663 {
                                var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1660,
                                }
                                return t1664
                            } else {
                                var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1665
                            }
                        } else {
                            t1674 = value__81
                            var t1675 uint8 = t1674 - 97
                            var t1676 uint8 = t1675 + 10
                            var t1677 int = int(uint8(t1676))
                            jp1660 = t1677
                            var t1663 bool = jp1660 < base__82
                            if t1663 {
                                var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1660,
                                }
                                return t1664
                            } else {
                                var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1665
                            }
                        }
                    } else {
                        var t1678 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1678
                    }
                } else {
                    t1681 = value__81
                    var t1682 bool = t1681 <= 102
                    jp1673 = t1682
                    if jp1673 {
                        var t1674 uint8
                        var inline2139 bool = value__81 >= 65
                        var inline2141 bool
                        if inline2139 {
                            var inline2144 bool = value__81 <= 90
                            inline2141 = inline2144
                        } else {
                            inline2141 = false
                        }
                        if inline2141 {
                            var inline2142 uint8 = 97 - 65
                            var inline2143 uint8 = value__81 + inline2142
                            t1674 = inline2143
                            var t1675 uint8 = t1674 - 97
                            var t1676 uint8 = t1675 + 10
                            var t1677 int = int(uint8(t1676))
                            jp1660 = t1677
                            var t1663 bool = jp1660 < base__82
                            if t1663 {
                                var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1660,
                                }
                                return t1664
                            } else {
                                var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1665
                            }
                        } else {
                            t1674 = value__81
                            var t1675 uint8 = t1674 - 97
                            var t1676 uint8 = t1675 + 10
                            var t1677 int = int(uint8(t1676))
                            jp1660 = t1677
                            var t1663 bool = jp1660 < base__82
                            if t1663 {
                                var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1660,
                                }
                                return t1664
                            } else {
                                var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1665
                            }
                        }
                    } else {
                        var t1678 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1678
                    }
                }
            } else {
                jp1673 = false
                if jp1673 {
                    var t1674 uint8
                    var inline2139 bool = value__81 >= 65
                    var inline2141 bool
                    if inline2139 {
                        var inline2144 bool = value__81 <= 90
                        inline2141 = inline2144
                    } else {
                        inline2141 = false
                    }
                    if inline2141 {
                        var inline2142 uint8 = 97 - 65
                        var inline2143 uint8 = value__81 + inline2142
                        t1674 = inline2143
                        var t1675 uint8 = t1674 - 97
                        var t1676 uint8 = t1675 + 10
                        var t1677 int = int(uint8(t1676))
                        jp1660 = t1677
                        var t1663 bool = jp1660 < base__82
                        if t1663 {
                            var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1660,
                            }
                            return t1664
                        } else {
                            var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1665
                        }
                    } else {
                        t1674 = value__81
                        var t1675 uint8 = t1674 - 97
                        var t1676 uint8 = t1675 + 10
                        var t1677 int = int(uint8(t1676))
                        jp1660 = t1677
                        var t1663 bool = jp1660 < base__82
                        if t1663 {
                            var t1664 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1660,
                            }
                            return t1664
                        } else {
                            var t1665 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1665
                        }
                    }
                } else {
                    var t1678 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1678
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1687:
    for {
        var t1688 bool = carry__22 != 0
        if t1688 {
            var t1697 *_goml_vec_uint32 = value__20.words
            var t1698 int
            var inline2156 int = vec_len__Vec_6uint32(t1697)
            t1698 = inline2156
            var t1699 bool = index__23 == t1698
            if t1699 {
                var t1700 *_goml_vec_uint32 = value__20.words
                var inline2153 uint32 = 0
                vec_push__Vec_6uint32(t1700, inline2153)
            } else {}
            var t1690 *_goml_vec_uint32 = value__20.words
            var t1691 uint32 = vec_get__Vec_6uint32(t1690, index__23)
            var t1692 uint64 = uint64(uint32(t1691))
            var sum__24 uint64 = t1692 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1694_rhs int = 32
            var t1694 uint64 = sum__24 >> t1694_rhs
            carry__22 = t1694
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1695 int = compound_old42 + compound_value43
            index__23 = t1695
            continue
        } else {
            break Loop_loop1687
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1704 FloatNatural
    var inline2158 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2159 FloatNatural = FloatNatural{
        words: inline2158,
    }
    t1704 = inline2159
    var t1705 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1704,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1705
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1725 *_goml_vec_uint32 = value__9.words
    var t1726 bool
    var inline2165 int = vec_len__Vec_6uint32(t1725)
    var inline2166 bool = inline2165 == 0
    t1726 = inline2166
    if t1726 {
        return 0
    } else {
        var t1709 *_goml_vec_uint32 = value__9.words
        var t1710 *_goml_vec_uint32 = value__9.words
        var t1711 int
        var inline2163 int = vec_len__Vec_6uint32(t1710)
        t1711 = inline2163
        var t1712 int = t1711 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1709, t1712)
        var bits__11 int = 0
        Loop_loop1719:
        for {
            var t1720 bool = high__10 != 0
            if t1720 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1721 uint32 = compound_old9 >> compound_value10
                high__10 = t1721
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1723 int = compound_old12 + compound_value13
                bits__11 = t1723
                continue
            } else {
                break Loop_loop1719
            }
        }
        var t1714 *_goml_vec_uint32 = value__9.words
        var t1715 int
        var inline2161 int = vec_len__Vec_6uint32(t1714)
        t1715 = inline2161
        var t1716 int = t1715 - 1
        var t1717 int = t1716 * 32
        var t1718 int = t1717 + bits__11
        return t1718
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1748 *_goml_vec_uint32 = left__12.words
    var t1749 int
    var inline2176 int = vec_len__Vec_6uint32(t1748)
    t1749 = inline2176
    var t1750 *_goml_vec_uint32 = right__13.words
    var t1751 int
    var inline2174 int = vec_len__Vec_6uint32(t1750)
    t1751 = inline2174
    var t1752 bool = t1749 < t1751
    if t1752 {
        return -1
    } else {
        var t1754 *_goml_vec_uint32 = left__12.words
        var t1755 int
        var inline2170 int = vec_len__Vec_6uint32(t1754)
        t1755 = inline2170
        var t1756 *_goml_vec_uint32 = right__13.words
        var t1757 int
        var inline2168 int = vec_len__Vec_6uint32(t1756)
        t1757 = inline2168
        var t1758 bool = t1755 > t1757
        if t1758 {
            return 1
        } else {
            var t1730 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2172 int = vec_len__Vec_6uint32(t1730)
            index__14 = inline2172
            Loop_loop1732:
            for {
                var t1733 bool = index__14 > 0
                if t1733 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1734 int = compound_old17 - compound_value18
                    index__14 = t1734
                    var t1737 *_goml_vec_uint32 = left__12.words
                    var t1738 uint32 = vec_get__Vec_6uint32(t1737, index__14)
                    var t1739 *_goml_vec_uint32 = right__13.words
                    var t1740 uint32 = vec_get__Vec_6uint32(t1739, index__14)
                    var t1741 bool = t1738 < t1740
                    if t1741 {
                        return -1
                    } else {
                        var t1743 *_goml_vec_uint32 = left__12.words
                        var t1744 uint32 = vec_get__Vec_6uint32(t1743, index__14)
                        var t1745 *_goml_vec_uint32 = right__13.words
                        var t1746 uint32 = vec_get__Vec_6uint32(t1745, index__14)
                        var t1747 bool = t1744 > t1746
                        if t1747 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1732
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1794 bool = shift__57 >= 0
    var jp1762 FloatNatural
    if t1794 {
        var t1795 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1762 = t1795
    } else {
        var t1796 FloatNatural = float_natural_copy(numerator__55)
        jp1762 = t1796
    }
    var t1790 bool = shift__57 >= 0
    var jp1764 FloatNatural
    if t1790 {
        var t1791 FloatNatural = float_natural_copy(denominator__56)
        jp1764 = t1791
    } else {
        var t1792 int = 0 - shift__57
        var t1793 FloatNatural = float_natural_shift_left(denominator__56, t1792)
        jp1764 = t1793
    }
    var quotient__60 uint64 = 0
    Loop_loop1777:
    for {
        var t1778 int = float_natural_compare(jp1762, jp1764)
        var t1779 bool = t1778 >= 0
        if t1779 {
            var t1780 int = float_natural_bit_length(jp1762)
            var t1781 int = float_natural_bit_length(jp1764)
            var offset__61 int = t1780 - t1781
            var part__62 FloatNatural = float_natural_shift_left(jp1764, offset__61)
            var t1785 int = float_natural_compare(jp1762, part__62)
            var t1786 bool = t1785 < 0
            if t1786 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1787 int = compound_old105 - compound_value106
                offset__61 = t1787
                var t1789 FloatNatural = float_natural_shift_left(jp1764, offset__61)
                part__62 = t1789
            } else {}
            float_natural_subtract(jp1762, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1783 uint64 = compound_old111 | compound_value112
            quotient__60 = t1783
            continue
        } else {
            break Loop_loop1777
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1762, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1764)
    var t1771 bool = rounding__64 > 0
    var jp1768 bool
    if t1771 {
        jp1768 = true
    } else {
        var t1774 bool = rounding__64 == 0
        if t1774 {
            var t1775_rhs uint64 = 1
            var t1775 uint64 = quotient__60 & t1775_rhs
            var t1776 bool = t1775 == 1
            jp1768 = t1776
        } else {
            jp1768 = false
        }
    }
    if jp1768 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1769 uint64 = compound_old115 + compound_value116
        quotient__60 = t1769
    } else {}
    return quotient__60
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(self__531 *_goml_vec_uint32, len__532 int) struct{} {
    vec_truncate__Vec_6uint32(self__531, len__532)
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1812 bool = index__269 < 0
    var jp1804 bool
    if t1812 {
        jp1804 = true
    } else {
        var t1813 int
        var inline2178 int = _goml_runtime_core_string_len(value__268)
        t1813 = inline2178
        var t1814 bool = index__269 > t1813
        jp1804 = t1814
    }
    if jp1804 {
        return false
    } else {
        var t1807 int
        var inline2182 int = _goml_runtime_core_string_len(value__268)
        t1807 = inline2182
        var t1808 bool = index__269 == t1807
        if t1808 {
            return true
        } else {
            var t1809 uint8
            var inline2180 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1809 = inline2180
            var t1810_rhs uint8 = 192
            var t1810 uint8 = t1809 & t1810_rhs
            var t1811 bool = t1810 != 128
            return t1811
        }
    }
}

func float_natural_subtract(value__37 FloatNatural, other__38 FloatNatural) struct{} {
    var base__39 uint64 = 4294967296
    var borrow__40 uint64 = 0
    var index__41 int = 0
    Loop_loop1818:
    for {
        var t1819 *_goml_vec_uint32 = value__37.words
        var t1820 int
        var inline2186 int = vec_len__Vec_6uint32(t1819)
        t1820 = inline2186
        var t1821 bool = index__41 < t1820
        if t1821 {
            var t1835 *_goml_vec_uint32 = other__38.words
            var t1836 int
            var inline2184 int = vec_len__Vec_6uint32(t1835)
            t1836 = inline2184
            var t1837 bool = index__41 < t1836
            var jp1823 uint64
            if t1837 {
                var t1838 *_goml_vec_uint32 = other__38.words
                var t1839 uint32 = vec_get__Vec_6uint32(t1838, index__41)
                var t1840 uint64 = uint64(uint32(t1839))
                jp1823 = t1840
            } else {
                jp1823 = 0
            }
            var right__42 uint64 = jp1823 + borrow__40
            var t1824 *_goml_vec_uint32 = value__37.words
            var t1825 uint32 = vec_get__Vec_6uint32(t1824, index__41)
            var left__43 uint64 = uint64(uint32(t1825))
            var t1829 bool = left__43 >= right__42
            if t1829 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1830 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1830))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1832 uint64 = base__39 + left__43
                var t1833 uint64 = t1832 - right__42
                var value75 uint32 = uint32(uint64(t1833))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1827 int = compound_old79 + compound_value80
            index__41 = t1827
            continue
        } else {
            break Loop_loop1818
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func main() {
    main0()
}
