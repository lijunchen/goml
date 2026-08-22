package main

import (
    _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29 "math"
)

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

func _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(arg0 float64) uint64 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float64bits(arg0)
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

type Tuple2_7float32_7float64 struct {
    _0 float32
    _1 float64
}

type Tuple2_6string_4bool struct {
    _0 string
    _1 bool
}

type Tuple2_4bool_6uint64 struct {
    _0 bool
    _1 uint64
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
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

type FloatEvent interface {
    isFloatEvent()
}

type Sample32 struct {
    _0 string
    _1 float32
}

func (_ Sample32) isFloatEvent() {}

type Sample64 struct {
    _0 string
    _1 float64
}

func (_ Sample64) isFloatEvent() {}

func summarize(event__0 FloatEvent) string {
    switch event__0.(type) {
    case Sample32:
        var x796 string = event__0.(Sample32)._0
        var x797 float32 = event__0.(Sample32)._1
        var t807 string
        var inline1823 string = __goml_builtin_float32_to_string(x797)
        t807 = inline1823
        var t808 string = x796 + t807
        return t808
    case Sample64:
        var x798 string = event__0.(Sample64)._0
        var x799 float64 = event__0.(Sample64)._1
        var t809 string
        var inline1825 string = __goml_builtin_float64_to_string(x799)
        t809 = inline1825
        var t810 string = x798 + t809
        return t810
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var first_value__12 float32 = 0.5
    var second_value__13 float32 = 2.25
    var third_value__14 float64 = 9.5
    var first__15 FloatEvent = Sample32{
        _0: "f32=",
        _1: first_value__12,
    }
    var second__16 FloatEvent = Sample32{
        _0: "f32_b=",
        _1: second_value__13,
    }
    var t819 string = summarize(first__15)
    var t820 string = summarize(second__16)
    var t821 string = t819 + t820
    var t822 string
    var inline1868 string = "f64="
    var inline1872 string = _goml_m_trait__impl_i_ToString_i_f64_i_to__string(third_value__14)
    var inline1873 string = inline1868 + inline1872
    t822 = inline1873
    var t823 string = t821 + t822
    var t824 string
    var inline1848 float32 = 0.75
    var inline1849 float64 = 4
    var inline1852 float32 = 1
    var inline1853 float64 = 5
    var inline1854 bool = inline1848 < inline1852
    var inline1855 bool = inline1849 < inline1853
    var inline1856 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1854)
    var inline1857 string = "left<1?=" + inline1856
    var inline1858 string = inline1857 + ",right<5?="
    var inline1859 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1855)
    var inline1860 string = inline1858 + inline1859
    t824 = inline1860
    var t825 string = t823 + t824
    var t826 string
    var inline1834 float32 = 1.5
    var inline1835 float64 = 7.25
    var inline1838 float32 = 1
    var inline1839 float64 = 5
    var inline1840 bool = inline1834 < inline1838
    var inline1841 bool = inline1835 < inline1839
    var inline1842 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1840)
    var inline1843 string = "left<1?=" + inline1842
    var inline1844 string = inline1843 + ",right<5?="
    var inline1845 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1841)
    var inline1846 string = inline1844 + inline1845
    t826 = inline1846
    var message__20 string = t825 + t826
    var inline1831 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__20)
    _goml_runtime_core_string_println(inline1831)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_f64_i_to__string(self__414 float64) string {
    var inline1879 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(self__414)
    var inline1880 string = format_float_bits(inline1879, 52, 11, 1023)
    return inline1880
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t835 string = _goml_runtime_core_bool_to_string(self__401)
    return t835
}

func __goml_builtin_float32_to_string(value__194 float32) string {
    var t841 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(value__194)
    var t842 uint64 = uint64(uint32(t841))
    var t843 string = format_float_bits(t842, 23, 8, 127)
    return t843
}

func __goml_builtin_float64_to_string(value__195 float64) string {
    var t846 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(value__195)
    var t847 string = format_float_bits(t846, 52, 11, 1023)
    return t847
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t852 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t852
    var t853 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t853 != 0
    var t854_lhs uint64 = 1
    var t854 uint64 = t854_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t854 - 1
    var t855 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t855 & exponent_mask__166
    var t856_lhs uint64 = 1
    var t856 uint64 = t856_lhs << mantissa_bits__161
    var t857 uint64 = t856 - 1
    var fraction__168 uint64 = bits__160 & t857
    var t921 bool = exponent__167 == exponent_mask__166
    if t921 {
        var t923 bool = fraction__168 == 0
        if t923 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t929 bool = exponent__167 == 0
        var jp927 bool
        if t929 {
            var t930 bool = fraction__168 == 0
            jp927 = t930
        } else {
            jp927 = false
        }
        if jp927 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t918 bool = exponent__167 == 0
            var jp860 uint64
            if t918 {
                jp860 = fraction__168
            } else {
                var t919_lhs uint64 = 1
                var t919 uint64 = t919_lhs << mantissa_bits__161
                var t920 uint64 = fraction__168 | t919
                jp860 = t920
            }
            var t912 bool = exponent__167 == 0
            var jp862 int
            if t912 {
                var t913 int = 1 - exponent_bias__163
                var t914 int = t913 - mantissa_bits__161
                jp862 = t914
            } else {
                var t915 int = int(uint64(exponent__167))
                var t916 int = t915 - exponent_bias__163
                var t917 int = t916 - mantissa_bits__161
                jp862 = t917
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp860)
            var t867 bool = jp862 >= 0
            var jp864 int
            if t867 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp862)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t886 bool = mantissa_bits__161 == 23
                var jp869 int
                if t886 {
                    jp869 = 9
                } else {
                    jp869 = 17
                }
                var t883 int
                var inline1889 int = _goml_runtime_core_string_len(digits__173)
                t883 = inline1889
                var t884 bool = t883 < jp869
                var jp871 int
                if t884 {
                    var inline1883 int = _goml_runtime_core_string_len(digits__173)
                    jp871 = inline1883
                } else {
                    jp871 = jp869
                }
                var count__176 int = 1
                Loop_loop874:
                for {
                    var t875 bool = count__176 <= jp871
                    if t875 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t876 int
                        var inline1885 int = _goml_runtime_core_string_len(digits__173)
                        t876 = inline1885
                        var jp878 int
                        if x319 {
                            jp878 = 1
                        } else {
                            jp878 = 0
                        }
                        var point__180 int = t876 + jp878
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t882 bool = x322 == bits__160
                        if t882 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t880 int = compound_old324 + compound_value325
                            count__176 = t880
                            continue
                        }
                    } else {
                        break Loop_loop874
                    }
                }
                var inline1887 int = _goml_runtime_core_string_len(digits__173)
                jp864 = inline1887
                var t865 string = float_natural_decimal(exact_value__171)
                var t866 string = fixed_float_text(t865, jp864, negative__165)
                return t866
            } else {
                var count__183 int = 0
                var t908 int = 0 - jp862
                Loop_loop907:
                for {
                    var t909 bool = count__183 < t908
                    if t909 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t910 int = compound_old329 + compound_value330
                        count__183 = t910
                        continue
                    } else {
                        break Loop_loop907
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t888 int
                var inline1895 int = _goml_runtime_core_string_len(digits__184)
                t888 = inline1895
                var point__185 int = t888 + jp862
                var t906 bool = mantissa_bits__161 == 23
                var jp890 int
                if t906 {
                    jp890 = 9
                } else {
                    jp890 = 17
                }
                var t903 int
                var inline1893 int = _goml_runtime_core_string_len(digits__184)
                t903 = inline1893
                var t904 bool = t903 < jp890
                var jp892 int
                if t904 {
                    var inline1891 int = _goml_runtime_core_string_len(digits__184)
                    jp892 = inline1891
                } else {
                    jp892 = jp890
                }
                count__183 = 1
                Loop_loop894:
                for {
                    var t895 bool = count__183 <= jp892
                    if t895 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp897 int
                        if x336 {
                            jp897 = 1
                        } else {
                            jp897 = 0
                        }
                        var t898 int = point__185 + jp897
                        var candidate__191 string = fixed_float_text(rounded__190, t898, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t902 bool = x339 == bits__160
                        if t902 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t900 int = compound_old341 + compound_value342
                            count__183 = t900
                            continue
                        }
                    } else {
                        break Loop_loop894
                    }
                }
                jp864 = point__185
                var t865 string = float_natural_decimal(exact_value__171)
                var t866 string = fixed_float_text(t865, jp864, negative__165)
                return t866
            }
        }
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline1901 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline1902 FloatNatural = FloatNatural{
        words: inline1901,
    }
    result__2 = inline1902
    var t934 bool = value__1 != 0
    if t934 {
        var t935 *_goml_vec_uint32 = result__2.words
        var t936 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t935, t936)
        var t937_rhs int = 32
        var t937 uint64 = value__1 >> t937_rhs
        var high__3 uint32 = uint32(uint64(t937))
        var t939 bool = high__3 != 0
        if t939 {
            var t940 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t940, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t969 bool
    var inline1919 *_goml_vec_uint32 = value__28.words
    var inline1920 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1919)
    t969 = inline1920
    if t969 {
        var inline1904 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline1905 FloatNatural = FloatNatural{
            words: inline1904,
        }
        return inline1905
    } else {
        var t972 bool = bits__29 == 0
        if t972 {
            var t973 FloatNatural = float_natural_copy(value__28)
            return t973
        } else {
            var result__30 FloatNatural
            var inline1916 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline1917 FloatNatural = FloatNatural{
                words: inline1916,
            }
            result__30 = inline1917
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop964:
            for {
                var t965 bool = index__33 < word_shift__31
                if t965 {
                    var t966 *_goml_vec_uint32 = result__30.words
                    var inline1907 uint32 = 0
                    vec_push__Vec_6uint32(t966, inline1907)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t967 int = compound_old52 + compound_value53
                    index__33 = t967
                    continue
                } else {
                    break Loop_loop964
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop952:
            for {
                var t953 *_goml_vec_uint32 = value__28.words
                var t954 int
                var inline1912 int = vec_len__Vec_6uint32(t953)
                t954 = inline1912
                var t955 bool = index__33 < t954
                if t955 {
                    var t956 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t956, index__33)
                    var t957 uint64 = uint64(uint32(word__35))
                    var t958 uint64 = t957 << bit_shift__32
                    var shifted__36 uint64 = t958 | carry__34
                    var t959 *_goml_vec_uint32 = result__30.words
                    var t960 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t959, t960)
                    var t961_rhs int = 32
                    var t961 uint64 = shifted__36 >> t961_rhs
                    carry__34 = t961
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t962 int = compound_old59 + compound_value60
                    index__33 = t962
                    continue
                } else {
                    break Loop_loop952
                }
            }
            var t948 bool = carry__34 != 0
            if t948 {
                var t949 *_goml_vec_uint32 = result__30.words
                var t950 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t949, t950)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t996 bool
    var inline1935 *_goml_vec_uint32 = value__49.words
    var inline1936 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1935)
    t996 = inline1936
    if t996 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop989:
        for {
            var t990 bool
            var inline1924 *_goml_vec_uint32 = current__50.words
            var inline1925 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1924)
            t990 = inline1925
            var t991 bool = !t990
            if t991 {
                var t992 uint32 = float_natural_divide_small(current__50, 10)
                var t993 uint8 = uint8(uint32(t992))
                var t994 uint8 = t993 + 48
                vec_push__Vec_5uint8(reversed__51, t994)
                continue
            } else {
                break Loop_loop989
            }
        }
        var t978 int
        var inline1933 int = vec_len__Vec_5uint8(reversed__51)
        t978 = inline1933
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t978)
        var offset__53 int = 0
        Loop_loop980:
        for {
            var t981 int
            var inline1931 int = vec_len__Vec_5uint8(reversed__51)
            t981 = inline1931
            var t982 bool = offset__53 < t981
            if t982 {
                var t983 int
                var inline1929 int = vec_len__Vec_5uint8(reversed__51)
                t983 = inline1929
                var t984 int = t983 - offset__53
                var t985 int = t984 - 1
                var t986 uint8 = vec_get__Vec_5uint8(reversed__51, t985)
                vec_push__Vec_5uint8(output__52, t986)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t987 int = compound_old98 + compound_value99
                offset__53 = t987
                continue
            } else {
                break Loop_loop980
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t999 int = _goml_runtime_core_string_len(self__289)
    return t999
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t1002 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1002)
    var index__148 int = 0
    Loop_loop1057:
    for {
        var t1058 bool = index__148 < count__146
        if t1058 {
            var t1059 uint8
            var inline1940 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1059 = inline1940
            vec_push__Vec_5uint8(output__147, t1059)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1060 int = compound_old267 + compound_value268
            index__148 = t1060
            continue
        } else {
            break Loop_loop1057
        }
    }
    var t1054 int
    var inline1961 int = _goml_runtime_core_string_len(exact__145)
    t1054 = inline1961
    var t1055 bool = count__146 == t1054
    if t1055 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1056 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1056
    } else {
        var next__150 uint8
        var inline1959 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline1959
        var trailing__151 bool = false
        var t1005 int = count__146 + 1
        index__148 = t1005
        Loop_loop1046:
        for {
            var t1047 int
            var inline1944 int = _goml_runtime_core_string_len(exact__145)
            t1047 = inline1944
            var t1048 bool = index__148 < t1047
            if t1048 {
                var t1052 uint8
                var inline1942 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1052 = inline1942
                var t1053 bool = t1052 != 48
                if t1053 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1050 int = compound_old278 + compound_value279
                index__148 = t1050
                continue
            } else {
                break Loop_loop1046
            }
        }
        var t1034 bool = next__150 > 53
        var jp1008 bool
        if t1034 {
            jp1008 = true
        } else {
            var t1037 bool = next__150 == 53
            if t1037 {
                if trailing__151 {
                    jp1008 = true
                } else {
                    var t1040 int
                    var inline1946 int = vec_len__Vec_5uint8(output__147)
                    t1040 = inline1946
                    var t1041 int = t1040 - 1
                    var t1042 uint8 = vec_get__Vec_5uint8(output__147, t1041)
                    var t1043 uint8 = t1042 - 48
                    var t1044_rhs uint8 = 2
                    var t1044 uint8 = t1043 % t1044_rhs
                    var t1045 bool = t1044 == 1
                    jp1008 = t1045
                }
            } else {
                jp1008 = false
            }
        }
        if jp1008 {
            var index__153 int
            var inline1957 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline1957
            Loop_loop1022:
            for {
                var t1023 bool = index__153 > 0
                if t1023 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1024 int = compound_old282 - compound_value283
                    index__153 = t1024
                    var t1027 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1028 bool = t1027 < 57
                    if t1028 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1029 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1029)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1031 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1031
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1022
                }
            }
            var t1012 int
            var inline1955 int = vec_len__Vec_5uint8(output__147)
            t1012 = inline1955
            var t1013 int = t1012 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1013)
            var inline1952 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline1952)
            index__153 = 0
            Loop_loop1016:
            for {
                var t1017 int
                var inline1950 int = vec_len__Vec_5uint8(output__147)
                t1017 = inline1950
                var t1018 bool = index__153 < t1017
                if t1018 {
                    var t1019 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t1019)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t1020 int = compound_old302 + compound_value303
                    index__153 = t1020
                    continue
                } else {
                    break Loop_loop1016
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t1015 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t1015
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1033 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1033
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline1968 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline1968
    Loop_loop1066:
    for {
        var t1071 bool = length__159 > 1
        var jp1068 bool
        if t1071 {
            var t1072 int = length__159 - 1
            var t1073 uint8
            var inline1963 uint8 = _goml_runtime_core_string_byte_get(value__158, t1072)
            t1073 = inline1963
            var t1074 bool = t1073 == 48
            jp1068 = t1074
        } else {
            jp1068 = false
        }
        if jp1068 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1069 int = compound_old312 - compound_value313
            length__159 = t1069
            continue
        } else {
            break Loop_loop1066
        }
    }
    var inline1965 int = 0
    var inline1966 string = string_byte_slice(value__158, inline1965, length__159)
    return inline1966
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline1970 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline1970)
    } else {}
    var t1079 bool = decimal_point__138 <= 0
    if t1079 {
        var inline1985 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline1985)
        var inline1982 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline1982)
        var index__141 int = 0
        var t1089 int = 0 - decimal_point__138
        Loop_loop1088:
        for {
            var t1090 bool = index__141 < t1089
            if t1090 {
                var inline1973 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline1973)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1091 int = compound_old234 + compound_value235
                index__141 = t1091
                continue
            } else {
                break Loop_loop1088
            }
        }
        index__141 = 0
        Loop_loop1082:
        for {
            var t1083 int
            var inline1980 int = _goml_runtime_core_string_len(digits__137)
            t1083 = inline1980
            var t1084 bool = index__141 < t1083
            if t1084 {
                var t1085 uint8
                var inline1978 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1085 = inline1978
                vec_push__Vec_5uint8(bytes__140, t1085)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1086 int = compound_old240 + compound_value241
                index__141 = t1086
                continue
            } else {
                break Loop_loop1082
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1094 int
        var inline2010 int = _goml_runtime_core_string_len(digits__137)
        t1094 = inline2010
        var t1095 bool = decimal_point__138 >= t1094
        if t1095 {
            var index__142 int = 0
            Loop_loop1102:
            for {
                var t1103 int
                var inline1992 int = _goml_runtime_core_string_len(digits__137)
                t1103 = inline1992
                var t1104 bool = index__142 < t1103
                if t1104 {
                    var t1105 uint8
                    var inline1990 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1105 = inline1990
                    vec_push__Vec_5uint8(bytes__140, t1105)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1106 int = compound_old244 + compound_value245
                    index__142 = t1106
                    continue
                } else {
                    break Loop_loop1102
                }
            }
            Loop_loop1098:
            for {
                var t1099 bool = index__142 < decimal_point__138
                if t1099 {
                    var inline1994 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline1994)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1100 int = compound_old249 + compound_value250
                    index__142 = t1100
                    continue
                } else {
                    break Loop_loop1098
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1116:
            for {
                var t1117 bool = index__143 < decimal_point__138
                if t1117 {
                    var t1118 uint8
                    var inline1999 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1118 = inline1999
                    vec_push__Vec_5uint8(bytes__140, t1118)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1119 int = compound_old253 + compound_value254
                    index__143 = t1119
                    continue
                } else {
                    break Loop_loop1116
                }
            }
            var inline2007 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline2007)
            Loop_loop1110:
            for {
                var t1111 int
                var inline2005 int = _goml_runtime_core_string_len(digits__137)
                t1111 = inline2005
                var t1112 bool = index__143 < t1111
                if t1112 {
                    var t1113 uint8
                    var inline2003 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1113 = inline2003
                    vec_push__Vec_5uint8(bytes__140, t1113)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1114 int = compound_old259 + compound_value260
                    index__143 = t1114
                    continue
                } else {
                    break Loop_loop1110
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
    var t1215 bool = parsed__110.valid
    var t1216 bool = !t1215
    if t1216 {
        var t1217 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t1217
    } else {
        var t1209 bool = parsed__110.negative
        var jp1126 uint64
        if t1209 {
            var t1214 bool = mantissa_bits__108 == 23
            var jp1211 int
            if t1214 {
                jp1211 = 8
            } else {
                jp1211 = 11
            }
            var t1212 int = mantissa_bits__108 + jp1211
            var t1213_lhs uint64 = 1
            var t1213 uint64 = t1213_lhs << t1212
            jp1126 = t1213
        } else {
            jp1126 = 0
        }
        var t1208 bool = mantissa_bits__108 == 23
        var jp1128 int
        if t1208 {
            jp1128 = 8
        } else {
            jp1128 = 11
        }
        var t1129_lhs uint64 = 1
        var t1129 uint64 = t1129_lhs << jp1128
        var t1130 uint64 = t1129 - 1
        var exponent_mask__112 uint64 = t1130 << mantissa_bits__108
        var t1186 int = parsed__110.special
        var t1187 bool = t1186 == 1
        if t1187 {
            var t1188 uint64 = jp1126 | exponent_mask__112
            var t1189 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t1188,
            }
            return t1189
        } else {
            var t1191 int = parsed__110.special
            var t1192 bool = t1191 == 2
            if t1192 {
                var t1196 int = mantissa_bits__108 - 1
                var t1197_lhs uint64 = 1
                var t1197 uint64 = t1197_lhs << t1196
                var t1198 uint64 = exponent_mask__112 | t1197
                var t1203 bool = mantissa_bits__108 == 52
                var jp1200 uint64
                if t1203 {
                    jp1200 = 1
                } else {
                    jp1200 = 0
                }
                var t1201 uint64 = t1198 | jp1200
                var t1202 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t1201,
                }
                return t1202
            } else {
                var t1205 FloatNatural = parsed__110.numerator
                var t1206 bool
                var inline2012 *_goml_vec_uint32 = t1205.words
                var inline2013 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2012)
                t1206 = inline2013
                if t1206 {
                    var t1207 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp1126,
                    }
                    return t1207
                } else {
                    var t1169 bool = parsed__110.hexadecimal
                    var t1170 bool = !t1169
                    if t1170 {
                        var t1171 int = parsed__110.significant_digits
                        var t1172 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t1171 + t1172
                        var t1185 bool = mantissa_bits__108 == 23
                        var jp1174 int
                        if t1185 {
                            jp1174 = 40
                        } else {
                            jp1174 = 310
                        }
                        var t1184 bool = mantissa_bits__108 == 23
                        var jp1176 int
                        if t1184 {
                            jp1176 = -46
                        } else {
                            jp1176 = -325
                        }
                        var t1178 bool = decimal_position__113 > jp1174
                        if t1178 {
                            var t1179 uint64 = jp1126 | exponent_mask__112
                            var t1180 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t1179,
                            }
                            return t1180
                        } else {
                            var t1182 bool = decimal_position__113 < jp1176
                            if t1182 {
                                var t1183 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp1126,
                                }
                                return t1183
                            } else {
                                var t1165 bool = parsed__110.hexadecimal
                                var t1166 bool = !t1165
                                var jp1160 bool
                                if t1166 {
                                    var t1167 int = parsed__110.decimal_exponent
                                    var t1168 bool = t1167 < 0
                                    jp1160 = t1168
                                } else {
                                    jp1160 = false
                                }
                                var jp1134 FloatNatural
                                if jp1160 {
                                    var t1161 int = parsed__110.decimal_exponent
                                    var t1162 int = 0 - t1161
                                    var t1163 FloatNatural = float_natural_power5(t1162)
                                    jp1134 = t1163
                                } else {
                                    var inline2015 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline2015, 1)
                                    var inline2017 FloatNatural = FloatNatural{
                                        words: inline2015,
                                    }
                                    jp1134 = inline2017
                                }
                                var t1155 bool = parsed__110.hexadecimal
                                var t1156 bool = !t1155
                                var jp1146 bool
                                if t1156 {
                                    var t1157 int = parsed__110.decimal_exponent
                                    var t1158 bool = t1157 > 0
                                    jp1146 = t1158
                                } else {
                                    jp1146 = false
                                }
                                var jp1136 FloatNatural
                                if jp1146 {
                                    var t1147 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t1147)
                                    var count__118 int = 0
                                    Loop_loop1149:
                                    for {
                                        var t1150 int = parsed__110.decimal_exponent
                                        var t1151 bool = count__118 < t1150
                                        if t1151 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t1152 int = compound_old213 + compound_value214
                                            count__118 = t1152
                                            continue
                                        } else {
                                            break Loop_loop1149
                                        }
                                    }
                                    jp1136 = result__117
                                    var t1142 bool = parsed__110.hexadecimal
                                    var jp1138 int
                                    if t1142 {
                                        var t1143 int = parsed__110.binary_exponent
                                        jp1138 = t1143
                                    } else {
                                        var t1144 int = parsed__110.decimal_exponent
                                        jp1138 = t1144
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1136, jp1134, jp1138, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1139 bool = !x219
                                    var t1140 uint64 = jp1126 | x218
                                    var t1141 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1139,
                                        _1: t1140,
                                    }
                                    return t1141
                                } else {
                                    var t1154 FloatNatural = parsed__110.numerator
                                    jp1136 = t1154
                                    var t1142 bool = parsed__110.hexadecimal
                                    var jp1138 int
                                    if t1142 {
                                        var t1143 int = parsed__110.binary_exponent
                                        jp1138 = t1143
                                    } else {
                                        var t1144 int = parsed__110.decimal_exponent
                                        jp1138 = t1144
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1136, jp1134, jp1138, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1139 bool = !x219
                                    var t1140 uint64 = jp1126 | x218
                                    var t1141 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1139,
                                        _1: t1140,
                                    }
                                    return t1141
                                }
                            }
                        }
                    } else {
                        var t1165 bool = parsed__110.hexadecimal
                        var t1166 bool = !t1165
                        var jp1160 bool
                        if t1166 {
                            var t1167 int = parsed__110.decimal_exponent
                            var t1168 bool = t1167 < 0
                            jp1160 = t1168
                        } else {
                            jp1160 = false
                        }
                        var jp1134 FloatNatural
                        if jp1160 {
                            var t1161 int = parsed__110.decimal_exponent
                            var t1162 int = 0 - t1161
                            var t1163 FloatNatural = float_natural_power5(t1162)
                            jp1134 = t1163
                        } else {
                            var inline2015 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline2015, 1)
                            var inline2017 FloatNatural = FloatNatural{
                                words: inline2015,
                            }
                            jp1134 = inline2017
                        }
                        var t1155 bool = parsed__110.hexadecimal
                        var t1156 bool = !t1155
                        var jp1146 bool
                        if t1156 {
                            var t1157 int = parsed__110.decimal_exponent
                            var t1158 bool = t1157 > 0
                            jp1146 = t1158
                        } else {
                            jp1146 = false
                        }
                        var jp1136 FloatNatural
                        if jp1146 {
                            var t1147 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t1147)
                            var count__118 int = 0
                            Loop_loop1149__2:
                            for {
                                var t1150 int = parsed__110.decimal_exponent
                                var t1151 bool = count__118 < t1150
                                if t1151 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t1152 int = compound_old213 + compound_value214
                                    count__118 = t1152
                                    continue
                                } else {
                                    break Loop_loop1149__2
                                }
                            }
                            jp1136 = result__117
                            var t1142 bool = parsed__110.hexadecimal
                            var jp1138 int
                            if t1142 {
                                var t1143 int = parsed__110.binary_exponent
                                jp1138 = t1143
                            } else {
                                var t1144 int = parsed__110.decimal_exponent
                                jp1138 = t1144
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1136, jp1134, jp1138, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1139 bool = !x219
                            var t1140 uint64 = jp1126 | x218
                            var t1141 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1139,
                                _1: t1140,
                            }
                            return t1141
                        } else {
                            var t1154 FloatNatural = parsed__110.numerator
                            jp1136 = t1154
                            var t1142 bool = parsed__110.hexadecimal
                            var jp1138 int
                            if t1142 {
                                var t1143 int = parsed__110.binary_exponent
                                jp1138 = t1143
                            } else {
                                var t1144 int = parsed__110.decimal_exponent
                                jp1138 = t1144
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1136, jp1134, jp1138, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1139 bool = !x219
                            var t1140 uint64 = jp1126 | x218
                            var t1141 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1139,
                                _1: t1140,
                            }
                            return t1141
                        }
                    }
                }
            }
        }
    }
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1239 bool = factor__16 == 0
    if t1239 {
        var t1240 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1240, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1233 uint64 = uint64(uint32(factor__16))
        Loop_loop1226:
        for {
            var t1227 *_goml_vec_uint32 = value__15.words
            var t1228 int
            var inline2021 int = vec_len__Vec_6uint32(t1227)
            t1228 = inline2021
            var t1229 bool = index__18 < t1228
            if t1229 {
                var t1230 *_goml_vec_uint32 = value__15.words
                var t1231 uint32 = vec_get__Vec_6uint32(t1230, index__18)
                var t1232 uint64 = uint64(uint32(t1231))
                var t1234 uint64 = t1232 * t1233
                var product__19 uint64 = t1234 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1236_rhs int = 32
                var t1236 uint64 = product__19 >> t1236_rhs
                carry__17 = t1236
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1237 int = compound_old30 + compound_value31
                index__18 = t1237
                continue
            } else {
                break Loop_loop1226
            }
        }
        var t1222 bool = carry__17 != 0
        if t1222 {
            var t1223 *_goml_vec_uint32 = value__15.words
            var t1224 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1223, t1224)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_natural_zero() FloatNatural {
    var t1243 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1244 FloatNatural = FloatNatural{
        words: t1243,
    }
    return t1244
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline2032 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2033 FloatNatural = FloatNatural{
        words: inline2032,
    }
    result__5 = inline2033
    var index__6 int = 0
    Loop_loop1254:
    for {
        var t1255 *_goml_vec_uint32 = value__4.words
        var t1256 int
        var inline2030 int = vec_len__Vec_6uint32(t1255)
        t1256 = inline2030
        var t1257 bool = index__6 < t1256
        if t1257 {
            var t1258 *_goml_vec_uint32 = result__5.words
            var t1259 *_goml_vec_uint32 = value__4.words
            var t1260 uint32 = vec_get__Vec_6uint32(t1259, index__6)
            vec_push__Vec_6uint32(t1258, t1260)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1261 int = compound_old4 + compound_value5
            index__6 = t1261
            continue
        } else {
            break Loop_loop1254
        }
    }
    return result__5
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1268 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline2035 int = vec_len__Vec_6uint32(t1268)
    index__47 = inline2035
    var t1279 uint64 = uint64(uint32(divisor__45))
    var t1282 uint64 = uint64(uint32(divisor__45))
    Loop_loop1271:
    for {
        var t1272 bool = index__47 > 0
        if t1272 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1273 int = compound_old83 - compound_value84
            index__47 = t1273
            var t1275_rhs int = 32
            var t1275 uint64 = remainder__46 << t1275_rhs
            var t1276 *_goml_vec_uint32 = value__44.words
            var t1277 uint32 = vec_get__Vec_6uint32(t1276, index__47)
            var t1278 uint64 = uint64(uint32(t1277))
            var current__48 uint64 = t1275 | t1278
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1280 uint64 = current__48 / t1279
            var value90 uint32 = uint32(uint64(t1280))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1283 uint64 = current__48 % t1282
            remainder__46 = t1283
            continue
        } else {
            break Loop_loop1271
        }
    }
    float_natural_trim(value__44)
    var t1270 uint32 = uint32(uint64(remainder__46))
    return t1270
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1291 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1291
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline2037 bool = string_is_char_boundary(self__294, start__295)
    var inline2039 bool
    if inline2037 {
        var inline2042 bool = string_is_char_boundary(self__294, end__296)
        inline2039 = inline2042
    } else {
        inline2039 = false
    }
    if inline2039 {
        var inline2040 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline2040
    } else {
        var inline2041 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2041
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1479 bool = string_equals_ascii_case(value__84, "nan")
    if t1479 {
        var t1480 FloatNatural
        var inline2044 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2045 FloatNatural = FloatNatural{
            words: inline2044,
        }
        t1480 = inline2045
        var t1481 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1480,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1481
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1471 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1472 bool = index__85 < t1471
        var jp1466 bool
        if t1472 {
            var t1475 uint8
            var inline2049 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1475 = inline2049
            var t1476 bool = t1475 == 43
            if t1476 {
                jp1466 = true
            } else {
                var t1477 uint8
                var inline2047 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1477 = inline2047
                var t1478 bool = t1477 == 45
                jp1466 = t1478
            }
        } else {
            jp1466 = false
        }
        if jp1466 {
            var t1467 uint8
            var inline2051 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1467 = inline2051
            var t1468 bool = t1467 == 45
            negative__86 = t1468
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1469 int = compound_old140 + compound_value141
            index__85 = t1469
        } else {}
        var t1299 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1299)
        var t1463 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1460 bool
        if t1463 {
            jp1460 = true
        } else {
            var t1464 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1460 = t1464
        }
        if jp1460 {
            var t1461 FloatNatural
            var inline2053 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2054 FloatNatural = FloatNatural{
                words: inline2053,
            }
            t1461 = inline2054
            var t1462 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1461,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1462
        } else {
            var t1454 int = index__85 + 2
            var t1455 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1456 bool = t1454 <= t1455
            var jp1449 bool
            if t1456 {
                var t1457 uint8
                var inline2056 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1457 = inline2056
                var t1458 bool = t1457 == 48
                jp1449 = t1458
            } else {
                jp1449 = false
            }
            var jp1302 bool
            if jp1449 {
                var t1450 int = index__85 + 1
                var t1451 uint8
                var inline2065 uint8 = _goml_runtime_core_string_byte_get(value__84, t1450)
                t1451 = inline2065
                var t1452 uint8
                var inline2058 bool = t1451 >= 65
                var inline2060 bool
                if inline2058 {
                    var inline2063 bool = t1451 <= 90
                    inline2060 = inline2063
                } else {
                    inline2060 = false
                }
                if inline2060 {
                    var inline2061 uint8 = 97 - 65
                    var inline2062 uint8 = t1451 + inline2061
                    t1452 = inline2062
                    var t1453 bool = t1452 == 120
                    jp1302 = t1453
                    if jp1302 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1446 int = compound_old145 + compound_value146
                        index__85 = t1446
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1305 int
                    if jp1302 {
                        jp1305 = 16
                    } else {
                        jp1305 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1399 uint32 = uint32(int(jp1305))
                    Loop_loop1395:
                    for {
                        var t1396 int
                        var inline2079 int = _goml_runtime_core_string_len(value__84)
                        t1396 = inline2079
                        var t1397 bool = index__85 < t1396
                        if t1397 {
                            var current__97 uint8
                            var inline2077 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2077
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1305)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1399)
                                var t1400 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1400)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1411 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1411
                                } else {}
                                var t1409 bool = significant_digits__95 > 0
                                var jp1406 bool
                                if t1409 {
                                    jp1406 = true
                                } else {
                                    var t1410 bool = x151 != 0
                                    jp1406 = t1410
                                }
                                if jp1406 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1407 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1407
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1403 int = compound_old164 + compound_value165
                                index__85 = t1403
                                continue
                            } else {
                                var t1414 bool = current__97 == 95
                                if t1414 {
                                    var t1435 int = index__85 + 1
                                    var t1436 int
                                    var inline2075 int = _goml_runtime_core_string_len(value__84)
                                    t1436 = inline2075
                                    var t1437 bool = t1435 >= t1436
                                    if t1437 {
                                        var inline2067 FloatNatural = float_natural_zero()
                                        var inline2068 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2067,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2068
                                    } else {
                                        var t1416 int = index__85 + 1
                                        var t1417 uint8
                                        var inline2073 uint8 = _goml_runtime_core_string_byte_get(value__84, t1416)
                                        t1417 = inline2073
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1417, jp1305)
                                        var x169 bool = mtmp168._0
                                        var jp1432 bool
                                        if jp1302 {
                                            var t1434 bool = !saw_digit__92
                                            jp1432 = t1434
                                        } else {
                                            jp1432 = false
                                        }
                                        var jp1419 bool
                                        if jp1432 {
                                            var t1433 bool = index__85 == mantissa_start__89
                                            jp1419 = t1433
                                        } else {
                                            jp1419 = false
                                        }
                                        var t1429 bool = !previous_digit__96
                                        var jp1427 bool
                                        if t1429 {
                                            var t1430 bool = !jp1419
                                            jp1427 = t1430
                                        } else {
                                            jp1427 = false
                                        }
                                        var jp1424 bool
                                        if jp1427 {
                                            jp1424 = true
                                        } else {
                                            var t1428 bool = !x169
                                            jp1424 = t1428
                                        }
                                        if jp1424 {
                                            var inline2070 FloatNatural = float_natural_zero()
                                            var inline2071 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2070,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2071
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1421 int = compound_old173 + compound_value174
                                            index__85 = t1421
                                            continue
                                        }
                                    }
                                } else {
                                    var t1444 bool = current__97 == 46
                                    var jp1441 bool
                                    if t1444 {
                                        var t1445 bool = !saw_dot__93
                                        jp1441 = t1445
                                    } else {
                                        jp1441 = false
                                    }
                                    if jp1441 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1442 int = compound_old178 + compound_value179
                                        index__85 = t1442
                                        continue
                                    } else {
                                        break Loop_loop1395
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1395
                        }
                    }
                    var t1393 bool = !saw_digit__92
                    if t1393 {
                        var inline2081 FloatNatural = float_natural_zero()
                        var inline2082 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2081,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2082
                    } else {
                        var jp1309 uint8
                        if jp1302 {
                            jp1309 = 112
                        } else {
                            jp1309 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1388 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1389 bool = index__85 < t1388
                        var jp1326 bool
                        if t1389 {
                            var t1390 uint8
                            var inline2084 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1390 = inline2084
                            var t1391 uint8 = ascii_lower(t1390)
                            var t1392 bool = t1391 == jp1309
                            jp1326 = t1392
                        } else {
                            jp1326 = false
                        }
                        if jp1326 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1327 int = compound_old183 + compound_value184
                            index__85 = t1327
                            var t1378 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1379 bool = index__85 < t1378
                            var jp1373 bool
                            if t1379 {
                                var t1382 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1383 bool = t1382 == 43
                                if t1383 {
                                    jp1373 = true
                                } else {
                                    var t1384 uint8
                                    var inline2086 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1384 = inline2086
                                    var t1385 bool = t1384 == 45
                                    jp1373 = t1385
                                }
                            } else {
                                jp1373 = false
                            }
                            if jp1373 {
                                var t1374 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1375 bool = t1374 == 45
                                exponent_negative__104 = t1375
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1376 int = compound_old187 + compound_value188
                                index__85 = t1376
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1334:
                            for {
                                var t1335 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1336 bool = index__85 < t1335
                                if t1336 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1370 bool = current__106 >= 48
                                    var jp1339 bool
                                    if t1370 {
                                        var t1371 bool = current__106 <= 57
                                        jp1339 = t1371
                                    } else {
                                        jp1339 = false
                                    }
                                    if jp1339 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1343 bool = exponent__103 < 1000000
                                        if t1343 {
                                            var t1344 int = exponent__103 * 10
                                            var t1345 uint8 = current__106 - 48
                                            var t1346 int = int(uint8(t1345))
                                            var t1347 int = t1344 + t1346
                                            exponent__103 = t1347
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1341 int = compound_old196 + compound_value197
                                        index__85 = t1341
                                        continue
                                    } else {
                                        var t1349 bool = current__106 == 95
                                        if t1349 {
                                            var t1366 bool = !previous_digit__96
                                            var jp1362 bool
                                            if t1366 {
                                                jp1362 = true
                                            } else {
                                                var t1367 int = index__85 + 1
                                                var t1368 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1369 bool = t1367 >= t1368
                                                jp1362 = t1369
                                            }
                                            var jp1357 bool
                                            if jp1362 {
                                                jp1357 = true
                                            } else {
                                                var t1363 int = index__85 + 1
                                                var t1364 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1363)
                                                var t1365 bool = t1364 < 48
                                                jp1357 = t1365
                                            }
                                            var jp1354 bool
                                            if jp1357 {
                                                jp1354 = true
                                            } else {
                                                var t1358 int = index__85 + 1
                                                var t1359 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1358)
                                                var t1360 bool = t1359 > 57
                                                jp1354 = t1360
                                            }
                                            if jp1354 {
                                                var t1355 ParsedFloat = invalid_parsed_float()
                                                return t1355
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1351 int = compound_old201 + compound_value202
                                                index__85 = t1351
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1334
                                        }
                                    }
                                } else {
                                    break Loop_loop1334
                                }
                            }
                            var t1332 bool = !exponent_digits__105
                            if t1332 {
                                var t1333 ParsedFloat = invalid_parsed_float()
                                return t1333
                            } else {
                                var t1322 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1323 bool = index__85 != t1322
                                if t1323 {
                                    var t1324 ParsedFloat = invalid_parsed_float()
                                    return t1324
                                } else {
                                    if exponent_negative__104 {
                                        var t1321 int = 0 - exponent__103
                                        exponent__103 = t1321
                                    } else {}
                                    var jp1314 int
                                    if jp1302 {
                                        jp1314 = 0
                                    } else {
                                        var t1320 int = exponent__103 - fraction_digits__94
                                        jp1314 = t1320
                                    }
                                    var jp1316 int
                                    if jp1302 {
                                        var t1318 int = fraction_digits__94 * 4
                                        var t1319 int = exponent__103 - t1318
                                        jp1316 = t1319
                                    } else {
                                        jp1316 = 0
                                    }
                                    var t1317 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1314,
                                        binary_exponent: jp1316,
                                        hexadecimal: jp1302,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1317
                                }
                            }
                        } else {
                            if jp1302 {
                                var t1387 ParsedFloat = invalid_parsed_float()
                                return t1387
                            } else {
                                var t1322 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1323 bool = index__85 != t1322
                                if t1323 {
                                    var t1324 ParsedFloat = invalid_parsed_float()
                                    return t1324
                                } else {
                                    if exponent_negative__104 {
                                        var t1321 int = 0 - exponent__103
                                        exponent__103 = t1321
                                    } else {}
                                    var jp1314 int
                                    if jp1302 {
                                        jp1314 = 0
                                    } else {
                                        var t1320 int = exponent__103 - fraction_digits__94
                                        jp1314 = t1320
                                    }
                                    var jp1316 int
                                    if jp1302 {
                                        var t1318 int = fraction_digits__94 * 4
                                        var t1319 int = exponent__103 - t1318
                                        jp1316 = t1319
                                    } else {
                                        jp1316 = 0
                                    }
                                    var t1317 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1314,
                                        binary_exponent: jp1316,
                                        hexadecimal: jp1302,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1317
                                }
                            }
                        }
                    }
                } else {
                    t1452 = t1451
                    var t1453 bool = t1452 == 120
                    jp1302 = t1453
                    if jp1302 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1446 int = compound_old145 + compound_value146
                        index__85 = t1446
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1305 int
                    if jp1302 {
                        jp1305 = 16
                    } else {
                        jp1305 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1399 uint32 = uint32(int(jp1305))
                    Loop_loop1395__2:
                    for {
                        var t1396 int
                        var inline2079 int = _goml_runtime_core_string_len(value__84)
                        t1396 = inline2079
                        var t1397 bool = index__85 < t1396
                        if t1397 {
                            var current__97 uint8
                            var inline2077 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2077
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1305)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1399)
                                var t1400 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1400)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1411 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1411
                                } else {}
                                var t1409 bool = significant_digits__95 > 0
                                var jp1406 bool
                                if t1409 {
                                    jp1406 = true
                                } else {
                                    var t1410 bool = x151 != 0
                                    jp1406 = t1410
                                }
                                if jp1406 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1407 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1407
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1403 int = compound_old164 + compound_value165
                                index__85 = t1403
                                continue
                            } else {
                                var t1414 bool = current__97 == 95
                                if t1414 {
                                    var t1435 int = index__85 + 1
                                    var t1436 int
                                    var inline2075 int = _goml_runtime_core_string_len(value__84)
                                    t1436 = inline2075
                                    var t1437 bool = t1435 >= t1436
                                    if t1437 {
                                        var inline2067 FloatNatural = float_natural_zero()
                                        var inline2068 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2067,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2068
                                    } else {
                                        var t1416 int = index__85 + 1
                                        var t1417 uint8
                                        var inline2073 uint8 = _goml_runtime_core_string_byte_get(value__84, t1416)
                                        t1417 = inline2073
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1417, jp1305)
                                        var x169 bool = mtmp168._0
                                        var jp1432 bool
                                        if jp1302 {
                                            var t1434 bool = !saw_digit__92
                                            jp1432 = t1434
                                        } else {
                                            jp1432 = false
                                        }
                                        var jp1419 bool
                                        if jp1432 {
                                            var t1433 bool = index__85 == mantissa_start__89
                                            jp1419 = t1433
                                        } else {
                                            jp1419 = false
                                        }
                                        var t1429 bool = !previous_digit__96
                                        var jp1427 bool
                                        if t1429 {
                                            var t1430 bool = !jp1419
                                            jp1427 = t1430
                                        } else {
                                            jp1427 = false
                                        }
                                        var jp1424 bool
                                        if jp1427 {
                                            jp1424 = true
                                        } else {
                                            var t1428 bool = !x169
                                            jp1424 = t1428
                                        }
                                        if jp1424 {
                                            var inline2070 FloatNatural = float_natural_zero()
                                            var inline2071 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2070,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2071
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1421 int = compound_old173 + compound_value174
                                            index__85 = t1421
                                            continue
                                        }
                                    }
                                } else {
                                    var t1444 bool = current__97 == 46
                                    var jp1441 bool
                                    if t1444 {
                                        var t1445 bool = !saw_dot__93
                                        jp1441 = t1445
                                    } else {
                                        jp1441 = false
                                    }
                                    if jp1441 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1442 int = compound_old178 + compound_value179
                                        index__85 = t1442
                                        continue
                                    } else {
                                        break Loop_loop1395__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1395__2
                        }
                    }
                    var t1393 bool = !saw_digit__92
                    if t1393 {
                        var inline2081 FloatNatural = float_natural_zero()
                        var inline2082 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2081,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2082
                    } else {
                        var jp1309 uint8
                        if jp1302 {
                            jp1309 = 112
                        } else {
                            jp1309 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1388 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1389 bool = index__85 < t1388
                        var jp1326 bool
                        if t1389 {
                            var t1390 uint8
                            var inline2084 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1390 = inline2084
                            var t1391 uint8 = ascii_lower(t1390)
                            var t1392 bool = t1391 == jp1309
                            jp1326 = t1392
                        } else {
                            jp1326 = false
                        }
                        if jp1326 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1327 int = compound_old183 + compound_value184
                            index__85 = t1327
                            var t1378 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1379 bool = index__85 < t1378
                            var jp1373 bool
                            if t1379 {
                                var t1382 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1383 bool = t1382 == 43
                                if t1383 {
                                    jp1373 = true
                                } else {
                                    var t1384 uint8
                                    var inline2086 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1384 = inline2086
                                    var t1385 bool = t1384 == 45
                                    jp1373 = t1385
                                }
                            } else {
                                jp1373 = false
                            }
                            if jp1373 {
                                var t1374 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1375 bool = t1374 == 45
                                exponent_negative__104 = t1375
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1376 int = compound_old187 + compound_value188
                                index__85 = t1376
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1334__2:
                            for {
                                var t1335 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1336 bool = index__85 < t1335
                                if t1336 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1370 bool = current__106 >= 48
                                    var jp1339 bool
                                    if t1370 {
                                        var t1371 bool = current__106 <= 57
                                        jp1339 = t1371
                                    } else {
                                        jp1339 = false
                                    }
                                    if jp1339 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1343 bool = exponent__103 < 1000000
                                        if t1343 {
                                            var t1344 int = exponent__103 * 10
                                            var t1345 uint8 = current__106 - 48
                                            var t1346 int = int(uint8(t1345))
                                            var t1347 int = t1344 + t1346
                                            exponent__103 = t1347
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1341 int = compound_old196 + compound_value197
                                        index__85 = t1341
                                        continue
                                    } else {
                                        var t1349 bool = current__106 == 95
                                        if t1349 {
                                            var t1366 bool = !previous_digit__96
                                            var jp1362 bool
                                            if t1366 {
                                                jp1362 = true
                                            } else {
                                                var t1367 int = index__85 + 1
                                                var t1368 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1369 bool = t1367 >= t1368
                                                jp1362 = t1369
                                            }
                                            var jp1357 bool
                                            if jp1362 {
                                                jp1357 = true
                                            } else {
                                                var t1363 int = index__85 + 1
                                                var t1364 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1363)
                                                var t1365 bool = t1364 < 48
                                                jp1357 = t1365
                                            }
                                            var jp1354 bool
                                            if jp1357 {
                                                jp1354 = true
                                            } else {
                                                var t1358 int = index__85 + 1
                                                var t1359 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1358)
                                                var t1360 bool = t1359 > 57
                                                jp1354 = t1360
                                            }
                                            if jp1354 {
                                                var t1355 ParsedFloat = invalid_parsed_float()
                                                return t1355
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1351 int = compound_old201 + compound_value202
                                                index__85 = t1351
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1334__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1334__2
                                }
                            }
                            var t1332 bool = !exponent_digits__105
                            if t1332 {
                                var t1333 ParsedFloat = invalid_parsed_float()
                                return t1333
                            } else {
                                var t1322 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1323 bool = index__85 != t1322
                                if t1323 {
                                    var t1324 ParsedFloat = invalid_parsed_float()
                                    return t1324
                                } else {
                                    if exponent_negative__104 {
                                        var t1321 int = 0 - exponent__103
                                        exponent__103 = t1321
                                    } else {}
                                    var jp1314 int
                                    if jp1302 {
                                        jp1314 = 0
                                    } else {
                                        var t1320 int = exponent__103 - fraction_digits__94
                                        jp1314 = t1320
                                    }
                                    var jp1316 int
                                    if jp1302 {
                                        var t1318 int = fraction_digits__94 * 4
                                        var t1319 int = exponent__103 - t1318
                                        jp1316 = t1319
                                    } else {
                                        jp1316 = 0
                                    }
                                    var t1317 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1314,
                                        binary_exponent: jp1316,
                                        hexadecimal: jp1302,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1317
                                }
                            }
                        } else {
                            if jp1302 {
                                var t1387 ParsedFloat = invalid_parsed_float()
                                return t1387
                            } else {
                                var t1322 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1323 bool = index__85 != t1322
                                if t1323 {
                                    var t1324 ParsedFloat = invalid_parsed_float()
                                    return t1324
                                } else {
                                    if exponent_negative__104 {
                                        var t1321 int = 0 - exponent__103
                                        exponent__103 = t1321
                                    } else {}
                                    var jp1314 int
                                    if jp1302 {
                                        jp1314 = 0
                                    } else {
                                        var t1320 int = exponent__103 - fraction_digits__94
                                        jp1314 = t1320
                                    }
                                    var jp1316 int
                                    if jp1302 {
                                        var t1318 int = fraction_digits__94 * 4
                                        var t1319 int = exponent__103 - t1318
                                        jp1316 = t1319
                                    } else {
                                        jp1316 = 0
                                    }
                                    var t1317 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1314,
                                        binary_exponent: jp1316,
                                        hexadecimal: jp1302,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1317
                                }
                            }
                        }
                    }
                }
            } else {
                jp1302 = false
                if jp1302 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1446 int = compound_old145 + compound_value146
                    index__85 = t1446
                } else {}
                var mantissa_start__89 int = index__85
                var jp1305 int
                if jp1302 {
                    jp1305 = 16
                } else {
                    jp1305 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1399 uint32 = uint32(int(jp1305))
                Loop_loop1395__3:
                for {
                    var t1396 int
                    var inline2079 int = _goml_runtime_core_string_len(value__84)
                    t1396 = inline2079
                    var t1397 bool = index__85 < t1396
                    if t1397 {
                        var current__97 uint8
                        var inline2077 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline2077
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1305)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1399)
                            var t1400 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1400)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1411 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1411
                            } else {}
                            var t1409 bool = significant_digits__95 > 0
                            var jp1406 bool
                            if t1409 {
                                jp1406 = true
                            } else {
                                var t1410 bool = x151 != 0
                                jp1406 = t1410
                            }
                            if jp1406 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1407 int = compound_old160 + compound_value161
                                significant_digits__95 = t1407
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1403 int = compound_old164 + compound_value165
                            index__85 = t1403
                            continue
                        } else {
                            var t1414 bool = current__97 == 95
                            if t1414 {
                                var t1435 int = index__85 + 1
                                var t1436 int
                                var inline2075 int = _goml_runtime_core_string_len(value__84)
                                t1436 = inline2075
                                var t1437 bool = t1435 >= t1436
                                if t1437 {
                                    var inline2067 FloatNatural = float_natural_zero()
                                    var inline2068 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline2067,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline2068
                                } else {
                                    var t1416 int = index__85 + 1
                                    var t1417 uint8
                                    var inline2073 uint8 = _goml_runtime_core_string_byte_get(value__84, t1416)
                                    t1417 = inline2073
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1417, jp1305)
                                    var x169 bool = mtmp168._0
                                    var jp1432 bool
                                    if jp1302 {
                                        var t1434 bool = !saw_digit__92
                                        jp1432 = t1434
                                    } else {
                                        jp1432 = false
                                    }
                                    var jp1419 bool
                                    if jp1432 {
                                        var t1433 bool = index__85 == mantissa_start__89
                                        jp1419 = t1433
                                    } else {
                                        jp1419 = false
                                    }
                                    var t1429 bool = !previous_digit__96
                                    var jp1427 bool
                                    if t1429 {
                                        var t1430 bool = !jp1419
                                        jp1427 = t1430
                                    } else {
                                        jp1427 = false
                                    }
                                    var jp1424 bool
                                    if jp1427 {
                                        jp1424 = true
                                    } else {
                                        var t1428 bool = !x169
                                        jp1424 = t1428
                                    }
                                    if jp1424 {
                                        var inline2070 FloatNatural = float_natural_zero()
                                        var inline2071 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2070,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2071
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1421 int = compound_old173 + compound_value174
                                        index__85 = t1421
                                        continue
                                    }
                                }
                            } else {
                                var t1444 bool = current__97 == 46
                                var jp1441 bool
                                if t1444 {
                                    var t1445 bool = !saw_dot__93
                                    jp1441 = t1445
                                } else {
                                    jp1441 = false
                                }
                                if jp1441 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1442 int = compound_old178 + compound_value179
                                    index__85 = t1442
                                    continue
                                } else {
                                    break Loop_loop1395__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1395__3
                    }
                }
                var t1393 bool = !saw_digit__92
                if t1393 {
                    var inline2081 FloatNatural = float_natural_zero()
                    var inline2082 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline2081,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline2082
                } else {
                    var jp1309 uint8
                    if jp1302 {
                        jp1309 = 112
                    } else {
                        jp1309 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1388 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1389 bool = index__85 < t1388
                    var jp1326 bool
                    if t1389 {
                        var t1390 uint8
                        var inline2084 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1390 = inline2084
                        var t1391 uint8 = ascii_lower(t1390)
                        var t1392 bool = t1391 == jp1309
                        jp1326 = t1392
                    } else {
                        jp1326 = false
                    }
                    if jp1326 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1327 int = compound_old183 + compound_value184
                        index__85 = t1327
                        var t1378 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1379 bool = index__85 < t1378
                        var jp1373 bool
                        if t1379 {
                            var t1382 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1383 bool = t1382 == 43
                            if t1383 {
                                jp1373 = true
                            } else {
                                var t1384 uint8
                                var inline2086 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1384 = inline2086
                                var t1385 bool = t1384 == 45
                                jp1373 = t1385
                            }
                        } else {
                            jp1373 = false
                        }
                        if jp1373 {
                            var t1374 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1375 bool = t1374 == 45
                            exponent_negative__104 = t1375
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1376 int = compound_old187 + compound_value188
                            index__85 = t1376
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1334__3:
                        for {
                            var t1335 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1336 bool = index__85 < t1335
                            if t1336 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1370 bool = current__106 >= 48
                                var jp1339 bool
                                if t1370 {
                                    var t1371 bool = current__106 <= 57
                                    jp1339 = t1371
                                } else {
                                    jp1339 = false
                                }
                                if jp1339 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1343 bool = exponent__103 < 1000000
                                    if t1343 {
                                        var t1344 int = exponent__103 * 10
                                        var t1345 uint8 = current__106 - 48
                                        var t1346 int = int(uint8(t1345))
                                        var t1347 int = t1344 + t1346
                                        exponent__103 = t1347
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1341 int = compound_old196 + compound_value197
                                    index__85 = t1341
                                    continue
                                } else {
                                    var t1349 bool = current__106 == 95
                                    if t1349 {
                                        var t1366 bool = !previous_digit__96
                                        var jp1362 bool
                                        if t1366 {
                                            jp1362 = true
                                        } else {
                                            var t1367 int = index__85 + 1
                                            var t1368 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1369 bool = t1367 >= t1368
                                            jp1362 = t1369
                                        }
                                        var jp1357 bool
                                        if jp1362 {
                                            jp1357 = true
                                        } else {
                                            var t1363 int = index__85 + 1
                                            var t1364 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1363)
                                            var t1365 bool = t1364 < 48
                                            jp1357 = t1365
                                        }
                                        var jp1354 bool
                                        if jp1357 {
                                            jp1354 = true
                                        } else {
                                            var t1358 int = index__85 + 1
                                            var t1359 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1358)
                                            var t1360 bool = t1359 > 57
                                            jp1354 = t1360
                                        }
                                        if jp1354 {
                                            var t1355 ParsedFloat = invalid_parsed_float()
                                            return t1355
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1351 int = compound_old201 + compound_value202
                                            index__85 = t1351
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1334__3
                                    }
                                }
                            } else {
                                break Loop_loop1334__3
                            }
                        }
                        var t1332 bool = !exponent_digits__105
                        if t1332 {
                            var t1333 ParsedFloat = invalid_parsed_float()
                            return t1333
                        } else {
                            var t1322 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1323 bool = index__85 != t1322
                            if t1323 {
                                var t1324 ParsedFloat = invalid_parsed_float()
                                return t1324
                            } else {
                                if exponent_negative__104 {
                                    var t1321 int = 0 - exponent__103
                                    exponent__103 = t1321
                                } else {}
                                var jp1314 int
                                if jp1302 {
                                    jp1314 = 0
                                } else {
                                    var t1320 int = exponent__103 - fraction_digits__94
                                    jp1314 = t1320
                                }
                                var jp1316 int
                                if jp1302 {
                                    var t1318 int = fraction_digits__94 * 4
                                    var t1319 int = exponent__103 - t1318
                                    jp1316 = t1319
                                } else {
                                    jp1316 = 0
                                }
                                var t1317 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1314,
                                    binary_exponent: jp1316,
                                    hexadecimal: jp1302,
                                    significant_digits: significant_digits__95,
                                }
                                return t1317
                            }
                        }
                    } else {
                        if jp1302 {
                            var t1387 ParsedFloat = invalid_parsed_float()
                            return t1387
                        } else {
                            var t1322 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1323 bool = index__85 != t1322
                            if t1323 {
                                var t1324 ParsedFloat = invalid_parsed_float()
                                return t1324
                            } else {
                                if exponent_negative__104 {
                                    var t1321 int = 0 - exponent__103
                                    exponent__103 = t1321
                                } else {}
                                var jp1314 int
                                if jp1302 {
                                    jp1314 = 0
                                } else {
                                    var t1320 int = exponent__103 - fraction_digits__94
                                    jp1314 = t1320
                                }
                                var jp1316 int
                                if jp1302 {
                                    var t1318 int = fraction_digits__94 * 4
                                    var t1319 int = exponent__103 - t1318
                                    jp1316 = t1319
                                } else {
                                    jp1316 = 0
                                }
                                var t1317 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1314,
                                    binary_exponent: jp1316,
                                    hexadecimal: jp1302,
                                    significant_digits: significant_digits__95,
                                }
                                return t1317
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
    var inline2088 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline2088, 1)
    var inline2090 FloatNatural = FloatNatural{
        words: inline2088,
    }
    result__26 = inline2090
    var count__27 int = 0
    Loop_loop1485:
    for {
        var t1486 bool = count__27 < exponent__25
        if t1486 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1487 int = compound_old46 + compound_value47
            count__27 = t1487
            continue
        } else {
            break Loop_loop1485
        }
    }
    return result__26
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1574 bool
    var inline2092 *_goml_vec_uint32 = numerator__65.words
    var inline2093 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2092)
    t1574 = inline2093
    if t1574 {
        var t1575 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1575
    } else {
        var t1571 bool = binary_shift__67 >= 0
        var jp1496 FloatNatural
        if t1571 {
            var t1572 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1496 = t1572
        } else {
            var t1573 FloatNatural = float_natural_copy(numerator__65)
            jp1496 = t1573
        }
        var t1567 bool = binary_shift__67 >= 0
        var jp1498 FloatNatural
        if t1567 {
            var t1568 FloatNatural = float_natural_copy(denominator__66)
            jp1498 = t1568
        } else {
            var t1569 int = 0 - binary_shift__67
            var t1570 FloatNatural = float_natural_shift_left(denominator__66, t1569)
            jp1498 = t1570
        }
        var t1499 int = float_natural_bit_length(jp1496)
        var t1500 int = float_natural_bit_length(jp1498)
        var exponent__72 int = t1499 - t1500
        var t1561 bool = exponent__72 >= 0
        var jp1502 int
        if t1561 {
            var t1562 FloatNatural = float_natural_shift_left(jp1498, exponent__72)
            var t1563 int = float_natural_compare(jp1496, t1562)
            jp1502 = t1563
        } else {
            var t1564 int = 0 - exponent__72
            var t1565 FloatNatural = float_natural_shift_left(jp1496, t1564)
            var t1566 int = float_natural_compare(t1565, jp1498)
            jp1502 = t1566
        }
        var t1558 bool = jp1502 < 0
        if t1558 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1559 int = compound_old120 - compound_value121
            exponent__72 = t1559
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1552 bool = exponent__72 > exponent_bias__69
        if t1552 {
            var t1553 int = exponent_bias__69 + exponent_bias__69
            var t1554 int = t1553 + 1
            var t1555 uint64 = uint64(int(t1554))
            var t1556 uint64 = t1555 << mantissa_bits__68
            var t1557 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1556,
                _1: true,
            }
            return t1557
        } else {
            var t1547 bool = exponent__72 < minimum_exponent__74
            var jp1506 uint64
            if t1547 {
                var t1548 int = mantissa_bits__68 - minimum_exponent__74
                var t1549 uint64 = float_rational_quotient(jp1496, jp1498, t1548)
                jp1506 = t1549
            } else {
                var t1550 int = mantissa_bits__68 - exponent__72
                var t1551 uint64 = float_rational_quotient(jp1496, jp1498, t1550)
                jp1506 = t1551
            }
            var mantissa__76 uint64 = jp1506
            var t1509 bool = exponent__72 < minimum_exponent__74
            if t1509 {
                var t1512 bool = mantissa__76 == 0
                if t1512 {
                    var t1513 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1513
                } else {
                    var t1516_lhs uint64 = 1
                    var t1516 uint64 = t1516_lhs << mantissa_bits__68
                    var t1517 bool = mantissa__76 >= t1516
                    if t1517 {
                        var t1518_lhs uint64 = 1
                        var t1518 uint64 = t1518_lhs << mantissa_bits__68
                        var t1519_lhs uint64 = 1
                        var t1519 uint64 = t1519_lhs << mantissa_bits__68
                        var t1520 uint64 = mantissa__76 - t1519
                        var t1521 uint64 = t1518 | t1520
                        var t1522 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1521,
                            _1: false,
                        }
                        return t1522
                    } else {
                        var t1523 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1523
                    }
                }
            } else {
                var t1540 int = mantissa_bits__68 + 1
                var t1541_lhs uint64 = 1
                var t1541 uint64 = t1541_lhs << t1540
                var t1542 bool = mantissa__76 >= t1541
                if t1542 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1543 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1543
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1545 int = compound_old128 + compound_value129
                    exponent__72 = t1545
                } else {}
                var t1527 bool = exponent__72 > exponent_bias__69
                if t1527 {
                    var t1528 int = exponent_bias__69 + exponent_bias__69
                    var t1529 int = t1528 + 1
                    var t1530 uint64 = uint64(int(t1529))
                    var t1531 uint64 = t1530 << mantissa_bits__68
                    var t1532 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1531,
                        _1: true,
                    }
                    return t1532
                } else {
                    var t1533 int = exponent__72 + exponent_bias__69
                    var t1534 uint64 = uint64(int(t1533))
                    var t1535 uint64 = t1534 << mantissa_bits__68
                    var t1536_lhs uint64 = 1
                    var t1536 uint64 = t1536_lhs << mantissa_bits__68
                    var t1537 uint64 = mantissa__76 - t1536
                    var t1538 uint64 = t1535 | t1537
                    var t1539 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1538,
                        _1: false,
                    }
                    return t1539
                }
            }
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1580 int = vec_len__Vec_6uint32(self__528)
    var t1581 bool = t1580 == 0
    return t1581
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1584:
    for {
        var t1592 *_goml_vec_uint32 = value__7.words
        var t1593 bool
        var inline2104 int = vec_len__Vec_6uint32(t1592)
        var inline2105 bool = inline2104 == 0
        t1593 = inline2105
        var t1594 bool = !t1593
        var jp1586 bool
        if t1594 {
            var t1595 *_goml_vec_uint32 = value__7.words
            var t1596 *_goml_vec_uint32 = value__7.words
            var t1597 int
            var inline2098 int = vec_len__Vec_6uint32(t1596)
            t1597 = inline2098
            var t1598 int = t1597 - 1
            var t1599 uint32 = vec_get__Vec_6uint32(t1595, t1598)
            var t1600 bool = t1599 == 0
            jp1586 = t1600
        } else {
            jp1586 = false
        }
        if jp1586 {
            var t1587 *_goml_vec_uint32 = value__7.words
            var t1588 *_goml_vec_uint32 = value__7.words
            var t1589 int
            var inline2102 int = vec_len__Vec_6uint32(t1588)
            t1589 = inline2102
            var t1590 int = t1589 - 1
            vec_truncate__Vec_6uint32(t1587, t1590)
            continue
        } else {
            break Loop_loop1584
        }
    }
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1609 bool = string_is_char_boundary(value__274, start__275)
    var jp1606 bool
    if t1609 {
        var t1610 bool = string_is_char_boundary(value__274, end__276)
        jp1606 = t1610
    } else {
        jp1606 = false
    }
    if jp1606 {
        var t1607 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1607
    } else {
        var t1608 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1608
    }
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1625 int
    var inline2122 int = _goml_runtime_core_string_len(value__78)
    t1625 = inline2122
    var t1626 int
    var inline2120 int = _goml_runtime_core_string_len(expected__79)
    t1626 = inline2120
    var t1627 bool = t1625 != t1626
    if t1627 {
        return false
    } else {
        var index__80 int = 0
        var inline2112 uint8 = 97 - 65
        Loop_loop1615:
        for {
            var t1616 int
            var inline2118 int = _goml_runtime_core_string_len(value__78)
            t1616 = inline2118
            var t1617 bool = index__80 < t1616
            if t1617 {
                var t1621 uint8
                var inline2116 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1621 = inline2116
                var t1622 uint8
                var inline2109 bool = t1621 >= 65
                var inline2111 bool
                if inline2109 {
                    var inline2114 bool = t1621 <= 90
                    inline2111 = inline2114
                } else {
                    inline2111 = false
                }
                if inline2111 {
                    var inline2113 uint8 = t1621 + inline2112
                    t1622 = inline2113
                    var t1623 uint8
                    var inline2107 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1623 = inline2107
                    var t1624 bool = t1622 != t1623
                    if t1624 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1619 int = compound_old134 + compound_value135
                        index__80 = t1619
                        continue
                    }
                } else {
                    t1622 = t1621
                    var t1623 uint8
                    var inline2107 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1623 = inline2107
                    var t1624 bool = t1622 != t1623
                    if t1624 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1619 int = compound_old134 + compound_value135
                        index__80 = t1619
                        continue
                    }
                }
            } else {
                break Loop_loop1615
            }
        }
        return true
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1636 bool = value__77 >= 65
    var jp1633 bool
    if t1636 {
        var t1637 bool = value__77 <= 90
        jp1633 = t1637
    } else {
        jp1633 = false
    }
    if jp1633 {
        var t1634 uint8 = 97 - 65
        var t1635 uint8 = value__77 + t1634
        return t1635
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1664 bool = value__81 >= 48
    var jp1648 bool
    if t1664 {
        var t1665 bool = value__81 <= 57
        jp1648 = t1665
    } else {
        jp1648 = false
    }
    var jp1641 int
    if jp1648 {
        var t1649 uint8 = value__81 - 48
        var t1650 int = int(uint8(t1649))
        jp1641 = t1650
        var t1644 bool = jp1641 < base__82
        if t1644 {
            var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1641,
            }
            return t1645
        } else {
            var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1646
        }
    } else {
        var t1660 uint8
        var inline2138 bool = value__81 >= 65
        var inline2140 bool
        if inline2138 {
            var inline2143 bool = value__81 <= 90
            inline2140 = inline2143
        } else {
            inline2140 = false
        }
        if inline2140 {
            var inline2141 uint8 = 97 - 65
            var inline2142 uint8 = value__81 + inline2141
            t1660 = inline2142
            var t1661 bool = t1660 >= 97
            var jp1654 bool
            if t1661 {
                var t1662 uint8
                var inline2124 bool = value__81 >= 65
                var inline2126 bool
                if inline2124 {
                    var inline2129 bool = value__81 <= 90
                    inline2126 = inline2129
                } else {
                    inline2126 = false
                }
                if inline2126 {
                    var inline2127 uint8 = 97 - 65
                    var inline2128 uint8 = value__81 + inline2127
                    t1662 = inline2128
                    var t1663 bool = t1662 <= 102
                    jp1654 = t1663
                    if jp1654 {
                        var t1655 uint8
                        var inline2131 bool = value__81 >= 65
                        var inline2133 bool
                        if inline2131 {
                            var inline2136 bool = value__81 <= 90
                            inline2133 = inline2136
                        } else {
                            inline2133 = false
                        }
                        if inline2133 {
                            var inline2134 uint8 = 97 - 65
                            var inline2135 uint8 = value__81 + inline2134
                            t1655 = inline2135
                            var t1656 uint8 = t1655 - 97
                            var t1657 uint8 = t1656 + 10
                            var t1658 int = int(uint8(t1657))
                            jp1641 = t1658
                            var t1644 bool = jp1641 < base__82
                            if t1644 {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1641,
                                }
                                return t1645
                            } else {
                                var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1646
                            }
                        } else {
                            t1655 = value__81
                            var t1656 uint8 = t1655 - 97
                            var t1657 uint8 = t1656 + 10
                            var t1658 int = int(uint8(t1657))
                            jp1641 = t1658
                            var t1644 bool = jp1641 < base__82
                            if t1644 {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1641,
                                }
                                return t1645
                            } else {
                                var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1646
                            }
                        }
                    } else {
                        var t1659 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1659
                    }
                } else {
                    t1662 = value__81
                    var t1663 bool = t1662 <= 102
                    jp1654 = t1663
                    if jp1654 {
                        var t1655 uint8
                        var inline2131 bool = value__81 >= 65
                        var inline2133 bool
                        if inline2131 {
                            var inline2136 bool = value__81 <= 90
                            inline2133 = inline2136
                        } else {
                            inline2133 = false
                        }
                        if inline2133 {
                            var inline2134 uint8 = 97 - 65
                            var inline2135 uint8 = value__81 + inline2134
                            t1655 = inline2135
                            var t1656 uint8 = t1655 - 97
                            var t1657 uint8 = t1656 + 10
                            var t1658 int = int(uint8(t1657))
                            jp1641 = t1658
                            var t1644 bool = jp1641 < base__82
                            if t1644 {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1641,
                                }
                                return t1645
                            } else {
                                var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1646
                            }
                        } else {
                            t1655 = value__81
                            var t1656 uint8 = t1655 - 97
                            var t1657 uint8 = t1656 + 10
                            var t1658 int = int(uint8(t1657))
                            jp1641 = t1658
                            var t1644 bool = jp1641 < base__82
                            if t1644 {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1641,
                                }
                                return t1645
                            } else {
                                var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1646
                            }
                        }
                    } else {
                        var t1659 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1659
                    }
                }
            } else {
                jp1654 = false
                if jp1654 {
                    var t1655 uint8
                    var inline2131 bool = value__81 >= 65
                    var inline2133 bool
                    if inline2131 {
                        var inline2136 bool = value__81 <= 90
                        inline2133 = inline2136
                    } else {
                        inline2133 = false
                    }
                    if inline2133 {
                        var inline2134 uint8 = 97 - 65
                        var inline2135 uint8 = value__81 + inline2134
                        t1655 = inline2135
                        var t1656 uint8 = t1655 - 97
                        var t1657 uint8 = t1656 + 10
                        var t1658 int = int(uint8(t1657))
                        jp1641 = t1658
                        var t1644 bool = jp1641 < base__82
                        if t1644 {
                            var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1641,
                            }
                            return t1645
                        } else {
                            var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1646
                        }
                    } else {
                        t1655 = value__81
                        var t1656 uint8 = t1655 - 97
                        var t1657 uint8 = t1656 + 10
                        var t1658 int = int(uint8(t1657))
                        jp1641 = t1658
                        var t1644 bool = jp1641 < base__82
                        if t1644 {
                            var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1641,
                            }
                            return t1645
                        } else {
                            var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1646
                        }
                    }
                } else {
                    var t1659 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1659
                }
            }
        } else {
            t1660 = value__81
            var t1661 bool = t1660 >= 97
            var jp1654 bool
            if t1661 {
                var t1662 uint8
                var inline2124 bool = value__81 >= 65
                var inline2126 bool
                if inline2124 {
                    var inline2129 bool = value__81 <= 90
                    inline2126 = inline2129
                } else {
                    inline2126 = false
                }
                if inline2126 {
                    var inline2127 uint8 = 97 - 65
                    var inline2128 uint8 = value__81 + inline2127
                    t1662 = inline2128
                    var t1663 bool = t1662 <= 102
                    jp1654 = t1663
                    if jp1654 {
                        var t1655 uint8
                        var inline2131 bool = value__81 >= 65
                        var inline2133 bool
                        if inline2131 {
                            var inline2136 bool = value__81 <= 90
                            inline2133 = inline2136
                        } else {
                            inline2133 = false
                        }
                        if inline2133 {
                            var inline2134 uint8 = 97 - 65
                            var inline2135 uint8 = value__81 + inline2134
                            t1655 = inline2135
                            var t1656 uint8 = t1655 - 97
                            var t1657 uint8 = t1656 + 10
                            var t1658 int = int(uint8(t1657))
                            jp1641 = t1658
                            var t1644 bool = jp1641 < base__82
                            if t1644 {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1641,
                                }
                                return t1645
                            } else {
                                var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1646
                            }
                        } else {
                            t1655 = value__81
                            var t1656 uint8 = t1655 - 97
                            var t1657 uint8 = t1656 + 10
                            var t1658 int = int(uint8(t1657))
                            jp1641 = t1658
                            var t1644 bool = jp1641 < base__82
                            if t1644 {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1641,
                                }
                                return t1645
                            } else {
                                var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1646
                            }
                        }
                    } else {
                        var t1659 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1659
                    }
                } else {
                    t1662 = value__81
                    var t1663 bool = t1662 <= 102
                    jp1654 = t1663
                    if jp1654 {
                        var t1655 uint8
                        var inline2131 bool = value__81 >= 65
                        var inline2133 bool
                        if inline2131 {
                            var inline2136 bool = value__81 <= 90
                            inline2133 = inline2136
                        } else {
                            inline2133 = false
                        }
                        if inline2133 {
                            var inline2134 uint8 = 97 - 65
                            var inline2135 uint8 = value__81 + inline2134
                            t1655 = inline2135
                            var t1656 uint8 = t1655 - 97
                            var t1657 uint8 = t1656 + 10
                            var t1658 int = int(uint8(t1657))
                            jp1641 = t1658
                            var t1644 bool = jp1641 < base__82
                            if t1644 {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1641,
                                }
                                return t1645
                            } else {
                                var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1646
                            }
                        } else {
                            t1655 = value__81
                            var t1656 uint8 = t1655 - 97
                            var t1657 uint8 = t1656 + 10
                            var t1658 int = int(uint8(t1657))
                            jp1641 = t1658
                            var t1644 bool = jp1641 < base__82
                            if t1644 {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1641,
                                }
                                return t1645
                            } else {
                                var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1646
                            }
                        }
                    } else {
                        var t1659 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1659
                    }
                }
            } else {
                jp1654 = false
                if jp1654 {
                    var t1655 uint8
                    var inline2131 bool = value__81 >= 65
                    var inline2133 bool
                    if inline2131 {
                        var inline2136 bool = value__81 <= 90
                        inline2133 = inline2136
                    } else {
                        inline2133 = false
                    }
                    if inline2133 {
                        var inline2134 uint8 = 97 - 65
                        var inline2135 uint8 = value__81 + inline2134
                        t1655 = inline2135
                        var t1656 uint8 = t1655 - 97
                        var t1657 uint8 = t1656 + 10
                        var t1658 int = int(uint8(t1657))
                        jp1641 = t1658
                        var t1644 bool = jp1641 < base__82
                        if t1644 {
                            var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1641,
                            }
                            return t1645
                        } else {
                            var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1646
                        }
                    } else {
                        t1655 = value__81
                        var t1656 uint8 = t1655 - 97
                        var t1657 uint8 = t1656 + 10
                        var t1658 int = int(uint8(t1657))
                        jp1641 = t1658
                        var t1644 bool = jp1641 < base__82
                        if t1644 {
                            var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1641,
                            }
                            return t1645
                        } else {
                            var t1646 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1646
                        }
                    }
                } else {
                    var t1659 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1659
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1668:
    for {
        var t1669 bool = carry__22 != 0
        if t1669 {
            var t1678 *_goml_vec_uint32 = value__20.words
            var t1679 int
            var inline2148 int = vec_len__Vec_6uint32(t1678)
            t1679 = inline2148
            var t1680 bool = index__23 == t1679
            if t1680 {
                var t1681 *_goml_vec_uint32 = value__20.words
                var inline2145 uint32 = 0
                vec_push__Vec_6uint32(t1681, inline2145)
            } else {}
            var t1671 *_goml_vec_uint32 = value__20.words
            var t1672 uint32 = vec_get__Vec_6uint32(t1671, index__23)
            var t1673 uint64 = uint64(uint32(t1672))
            var sum__24 uint64 = t1673 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1675_rhs int = 32
            var t1675 uint64 = sum__24 >> t1675_rhs
            carry__22 = t1675
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1676 int = compound_old42 + compound_value43
            index__23 = t1676
            continue
        } else {
            break Loop_loop1668
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1685 FloatNatural
    var inline2150 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2151 FloatNatural = FloatNatural{
        words: inline2150,
    }
    t1685 = inline2151
    var t1686 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1685,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1686
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1706 *_goml_vec_uint32 = value__9.words
    var t1707 bool
    var inline2157 int = vec_len__Vec_6uint32(t1706)
    var inline2158 bool = inline2157 == 0
    t1707 = inline2158
    if t1707 {
        return 0
    } else {
        var t1690 *_goml_vec_uint32 = value__9.words
        var t1691 *_goml_vec_uint32 = value__9.words
        var t1692 int
        var inline2155 int = vec_len__Vec_6uint32(t1691)
        t1692 = inline2155
        var t1693 int = t1692 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1690, t1693)
        var bits__11 int = 0
        Loop_loop1700:
        for {
            var t1701 bool = high__10 != 0
            if t1701 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1702 uint32 = compound_old9 >> compound_value10
                high__10 = t1702
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1704 int = compound_old12 + compound_value13
                bits__11 = t1704
                continue
            } else {
                break Loop_loop1700
            }
        }
        var t1695 *_goml_vec_uint32 = value__9.words
        var t1696 int
        var inline2153 int = vec_len__Vec_6uint32(t1695)
        t1696 = inline2153
        var t1697 int = t1696 - 1
        var t1698 int = t1697 * 32
        var t1699 int = t1698 + bits__11
        return t1699
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1729 *_goml_vec_uint32 = left__12.words
    var t1730 int
    var inline2168 int = vec_len__Vec_6uint32(t1729)
    t1730 = inline2168
    var t1731 *_goml_vec_uint32 = right__13.words
    var t1732 int
    var inline2166 int = vec_len__Vec_6uint32(t1731)
    t1732 = inline2166
    var t1733 bool = t1730 < t1732
    if t1733 {
        return -1
    } else {
        var t1735 *_goml_vec_uint32 = left__12.words
        var t1736 int
        var inline2162 int = vec_len__Vec_6uint32(t1735)
        t1736 = inline2162
        var t1737 *_goml_vec_uint32 = right__13.words
        var t1738 int
        var inline2160 int = vec_len__Vec_6uint32(t1737)
        t1738 = inline2160
        var t1739 bool = t1736 > t1738
        if t1739 {
            return 1
        } else {
            var t1711 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2164 int = vec_len__Vec_6uint32(t1711)
            index__14 = inline2164
            Loop_loop1713:
            for {
                var t1714 bool = index__14 > 0
                if t1714 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1715 int = compound_old17 - compound_value18
                    index__14 = t1715
                    var t1718 *_goml_vec_uint32 = left__12.words
                    var t1719 uint32 = vec_get__Vec_6uint32(t1718, index__14)
                    var t1720 *_goml_vec_uint32 = right__13.words
                    var t1721 uint32 = vec_get__Vec_6uint32(t1720, index__14)
                    var t1722 bool = t1719 < t1721
                    if t1722 {
                        return -1
                    } else {
                        var t1724 *_goml_vec_uint32 = left__12.words
                        var t1725 uint32 = vec_get__Vec_6uint32(t1724, index__14)
                        var t1726 *_goml_vec_uint32 = right__13.words
                        var t1727 uint32 = vec_get__Vec_6uint32(t1726, index__14)
                        var t1728 bool = t1725 > t1727
                        if t1728 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1713
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1775 bool = shift__57 >= 0
    var jp1743 FloatNatural
    if t1775 {
        var t1776 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1743 = t1776
    } else {
        var t1777 FloatNatural = float_natural_copy(numerator__55)
        jp1743 = t1777
    }
    var t1771 bool = shift__57 >= 0
    var jp1745 FloatNatural
    if t1771 {
        var t1772 FloatNatural = float_natural_copy(denominator__56)
        jp1745 = t1772
    } else {
        var t1773 int = 0 - shift__57
        var t1774 FloatNatural = float_natural_shift_left(denominator__56, t1773)
        jp1745 = t1774
    }
    var quotient__60 uint64 = 0
    Loop_loop1758:
    for {
        var t1759 int = float_natural_compare(jp1743, jp1745)
        var t1760 bool = t1759 >= 0
        if t1760 {
            var t1761 int = float_natural_bit_length(jp1743)
            var t1762 int = float_natural_bit_length(jp1745)
            var offset__61 int = t1761 - t1762
            var part__62 FloatNatural = float_natural_shift_left(jp1745, offset__61)
            var t1766 int = float_natural_compare(jp1743, part__62)
            var t1767 bool = t1766 < 0
            if t1767 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1768 int = compound_old105 - compound_value106
                offset__61 = t1768
                var t1770 FloatNatural = float_natural_shift_left(jp1745, offset__61)
                part__62 = t1770
            } else {}
            float_natural_subtract(jp1743, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1764 uint64 = compound_old111 | compound_value112
            quotient__60 = t1764
            continue
        } else {
            break Loop_loop1758
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1743, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1745)
    var t1752 bool = rounding__64 > 0
    var jp1749 bool
    if t1752 {
        jp1749 = true
    } else {
        var t1755 bool = rounding__64 == 0
        if t1755 {
            var t1756_rhs uint64 = 1
            var t1756 uint64 = quotient__60 & t1756_rhs
            var t1757 bool = t1756 == 1
            jp1749 = t1757
        } else {
            jp1749 = false
        }
    }
    if jp1749 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1750 uint64 = compound_old115 + compound_value116
        quotient__60 = t1750
    } else {}
    return quotient__60
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(self__531 *_goml_vec_uint32, len__532 int) struct{} {
    vec_truncate__Vec_6uint32(self__531, len__532)
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1793 bool = index__269 < 0
    var jp1785 bool
    if t1793 {
        jp1785 = true
    } else {
        var t1794 int
        var inline2170 int = _goml_runtime_core_string_len(value__268)
        t1794 = inline2170
        var t1795 bool = index__269 > t1794
        jp1785 = t1795
    }
    if jp1785 {
        return false
    } else {
        var t1788 int
        var inline2174 int = _goml_runtime_core_string_len(value__268)
        t1788 = inline2174
        var t1789 bool = index__269 == t1788
        if t1789 {
            return true
        } else {
            var t1790 uint8
            var inline2172 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1790 = inline2172
            var t1791_rhs uint8 = 192
            var t1791 uint8 = t1790 & t1791_rhs
            var t1792 bool = t1791 != 128
            return t1792
        }
    }
}

func float_natural_subtract(value__37 FloatNatural, other__38 FloatNatural) struct{} {
    var base__39 uint64 = 4294967296
    var borrow__40 uint64 = 0
    var index__41 int = 0
    Loop_loop1799:
    for {
        var t1800 *_goml_vec_uint32 = value__37.words
        var t1801 int
        var inline2178 int = vec_len__Vec_6uint32(t1800)
        t1801 = inline2178
        var t1802 bool = index__41 < t1801
        if t1802 {
            var t1816 *_goml_vec_uint32 = other__38.words
            var t1817 int
            var inline2176 int = vec_len__Vec_6uint32(t1816)
            t1817 = inline2176
            var t1818 bool = index__41 < t1817
            var jp1804 uint64
            if t1818 {
                var t1819 *_goml_vec_uint32 = other__38.words
                var t1820 uint32 = vec_get__Vec_6uint32(t1819, index__41)
                var t1821 uint64 = uint64(uint32(t1820))
                jp1804 = t1821
            } else {
                jp1804 = 0
            }
            var right__42 uint64 = jp1804 + borrow__40
            var t1805 *_goml_vec_uint32 = value__37.words
            var t1806 uint32 = vec_get__Vec_6uint32(t1805, index__41)
            var left__43 uint64 = uint64(uint32(t1806))
            var t1810 bool = left__43 >= right__42
            if t1810 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1811 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1811))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1813 uint64 = base__39 + left__43
                var t1814 uint64 = t1813 - right__42
                var value75 uint32 = uint32(uint64(t1814))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1808 int = compound_old79 + compound_value80
            index__41 = t1808
            continue
        } else {
            break Loop_loop1799
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func main() {
    main0()
}
