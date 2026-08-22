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

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
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

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_4char_3int struct {
    _0 rune
    _1 int
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
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

type _goml_m_Option_____o_char_c_isize_q_ struct {
    _tag int32
    _v1_0 Tuple2_4char_3int
}

type Option__char struct {
    _tag int32
    _v1_0 rune
}

func check_utf8(bytes__0 *_goml_vec_uint8, expected__1 bool) struct{} {
    var expected_length__2 int
    var inline1100 int = vec_len__Vec_5uint8(bytes__0)
    expected_length__2 = inline1100
    var mtmp796 Tuple2_4bool_6string = string_from_utf8(bytes__0)
    var x797 bool = mtmp796._0
    var x798 string = mtmp796._1
    var t837 bool = x797 == expected__1
    var inline1097 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t837)
    _goml_runtime_core_string_println(inline1097)
    if x797 {
        var t839 int
        var inline1092 int = _goml_runtime_core_string_len(x798)
        t839 = inline1092
        var t840 bool = t839 == expected_length__2
        var inline1089 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t840)
        _goml_runtime_core_string_println(inline1089)
        return struct{}{}
    } else {
        var t842 bool = x798 == ""
        var inline1094 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t842)
        _goml_runtime_core_string_println(inline1094)
        return struct{}{}
    }
}

func check_scalar(bytes__5 *_goml_vec_uint8, expected__6 uint32, expected_width__7 int) struct{} {
    var mtmp800 Tuple2_4bool_6string = string_from_utf8(bytes__5)
    var x801 bool = mtmp800._0
    var x802 string = mtmp800._1
    var inline1127 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x801)
    _goml_runtime_core_string_println(inline1127)
    var commute_field1235 Tuple2_4char_3int
    var inline1116 int = 0
    var inline1117 Tuple3_4bool_4char_3int = string_decode_utf8_at(x802, inline1116)
    var inline1118 bool = inline1117._0
    var inline1119 rune = inline1117._1
    var inline1120 int = inline1117._2
    if inline1118 {
        var inline1124 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1119,
            _1: inline1120,
        }
        commute_field1235 = inline1124
        var x808 rune = commute_field1235._0
        var x809 int = commute_field1235._1
        var t847 uint32 = uint32(rune(x808))
        var t848 bool = t847 == expected__6
        var inline1113 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t848)
        _goml_runtime_core_string_println(inline1113)
        var t849 bool = x809 == expected_width__7
        var inline1110 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t849)
        _goml_runtime_core_string_println(inline1110)
        return struct{}{}
    } else {
        var inline1106 bool = false
        var inline1107 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1106)
        _goml_runtime_core_string_println(inline1107)
        var inline1102 bool = false
        var inline1103 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1102)
        _goml_runtime_core_string_println(inline1103)
        return struct{}{}
    }
}

func main0() struct{} {
    var t852 [0]uint8 = [0]uint8{}
    var t853 *_goml_vec_uint8 = func(values [0]uint8) *_goml_vec_uint8 {
        return &_goml_vec_uint8{
            items: values[0:len(values)],
        }
    }(t852)
    check_utf8(t853, true)
    var t854 [1]uint8 = [1]uint8{0}
    var t855 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t854)
    check_utf8(t855, true)
    var t856 [1]uint8 = [1]uint8{127}
    var t857 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t856)
    check_utf8(t857, true)
    var t858 [2]uint8 = [2]uint8{194, 128}
    var t859 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t858)
    check_scalar(t859, 128, 2)
    var t860 [2]uint8 = [2]uint8{223, 191}
    var t861 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t860)
    check_scalar(t861, 2047, 2)
    var t862 [3]uint8 = [3]uint8{224, 160, 128}
    var t863 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t862)
    check_scalar(t863, 2048, 3)
    var t864 [3]uint8 = [3]uint8{237, 159, 191}
    var t865 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t864)
    check_scalar(t865, 55295, 3)
    var t866 [3]uint8 = [3]uint8{238, 128, 128}
    var t867 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t866)
    check_scalar(t867, 57344, 3)
    var t868 [3]uint8 = [3]uint8{239, 191, 189}
    var t869 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t868)
    check_scalar(t869, 65533, 3)
    var t870 [3]uint8 = [3]uint8{239, 191, 191}
    var t871 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t870)
    check_scalar(t871, 65535, 3)
    var t872 [4]uint8 = [4]uint8{240, 144, 128, 128}
    var t873 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t872)
    check_scalar(t873, 65536, 4)
    var t874 [4]uint8 = [4]uint8{244, 143, 191, 191}
    var t875 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t874)
    check_scalar(t875, 1114111, 4)
    var t876 [1]uint8 = [1]uint8{128}
    var t877 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t876)
    check_utf8(t877, false)
    var t878 [1]uint8 = [1]uint8{191}
    var t879 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t878)
    check_utf8(t879, false)
    var t880 [2]uint8 = [2]uint8{192, 128}
    var t881 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t880)
    check_utf8(t881, false)
    var t882 [2]uint8 = [2]uint8{193, 191}
    var t883 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t882)
    check_utf8(t883, false)
    var t884 [1]uint8 = [1]uint8{194}
    var t885 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t884)
    check_utf8(t885, false)
    var t886 [2]uint8 = [2]uint8{194, 127}
    var t887 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t886)
    check_utf8(t887, false)
    var t888 [3]uint8 = [3]uint8{224, 159, 191}
    var t889 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t888)
    check_utf8(t889, false)
    var t890 [2]uint8 = [2]uint8{225, 128}
    var t891 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t890)
    check_utf8(t891, false)
    var t892 [3]uint8 = [3]uint8{225, 128, 127}
    var t893 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t892)
    check_utf8(t893, false)
    var t894 [3]uint8 = [3]uint8{237, 160, 128}
    var t895 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t894)
    check_utf8(t895, false)
    var t896 [4]uint8 = [4]uint8{240, 143, 191, 191}
    var t897 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t896)
    check_utf8(t897, false)
    var t898 [3]uint8 = [3]uint8{240, 144, 128}
    var t899 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t898)
    var inline1160 bool = false
    var inline1161 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(t899)
    var inline1162 Tuple2_4bool_6string = string_from_utf8(t899)
    var inline1163 bool = inline1162._0
    var inline1164 string = inline1162._1
    var inline1167 bool = inline1163 == inline1160
    println__T_bool(inline1167)
    if inline1163 {
        var inline1169 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline1164)
        var inline1170 bool = inline1169 == inline1161
        println__T_bool(inline1170)
    } else {
        var inline1172 bool = inline1164 == ""
        println__T_bool(inline1172)
    }
    var t900 [4]uint8 = [4]uint8{244, 144, 128, 128}
    var t901 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t900)
    var inline1145 bool = false
    var inline1146 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(t901)
    var inline1147 Tuple2_4bool_6string = string_from_utf8(t901)
    var inline1148 bool = inline1147._0
    var inline1149 string = inline1147._1
    var inline1152 bool = inline1148 == inline1145
    println__T_bool(inline1152)
    if inline1148 {
        var inline1154 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline1149)
        var inline1155 bool = inline1154 == inline1146
        println__T_bool(inline1155)
    } else {
        var inline1157 bool = inline1149 == ""
        println__T_bool(inline1157)
    }
    var t902 [4]uint8 = [4]uint8{245, 128, 128, 128}
    var t903 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t902)
    var inline1130 bool = false
    var inline1131 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(t903)
    var inline1132 Tuple2_4bool_6string = string_from_utf8(t903)
    var inline1133 bool = inline1132._0
    var inline1134 string = inline1132._1
    var inline1137 bool = inline1133 == inline1130
    println__T_bool(inline1137)
    if inline1133 {
        var inline1139 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline1134)
        var inline1140 bool = inline1139 == inline1131
        println__T_bool(inline1140)
        return struct{}{}
    } else {
        var inline1142 bool = inline1134 == ""
        println__T_bool(inline1142)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(self__526 *_goml_vec_uint8) int {
    var t907 int = vec_len__Vec_5uint8(self__526)
    return t907
}

func string_from_utf8(bytes__277 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp395 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__277)
    var x397 string = mtmp395._1
    var index__279 int = 0
    Loop_loop912:
    for {
        var t913 int
        var inline1175 int = _goml_runtime_core_string_len(x397)
        t913 = inline1175
        var t914 bool = index__279 < t913
        if t914 {
            var mtmp398 Tuple3_4bool_4char_3int = string_decode_utf8_at(x397, index__279)
            var x399 bool = mtmp398._0
            var x401 int = mtmp398._2
            if x399 {
                var compound_old402 int = index__279
                var t916 int = compound_old402 + x401
                index__279 = t916
                continue
            } else {
                var t918 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t918
            }
        } else {
            break Loop_loop912
        }
    }
    var t911 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x397,
    }
    return t911
}

func println__T_bool(value__1 bool) struct{} {
    var t920 string
    var inline1177 string = _goml_runtime_core_bool_to_string(value__1)
    t920 = inline1177
    _goml_runtime_core_string_println(t920)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t924 int = _goml_runtime_core_string_len(self__289)
    return t924
}

func string_decode_utf8_at(value__258 string, index__259 int) Tuple3_4bool_4char_3int {
    var length__260 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__258)
    var t1049 bool = index__259 < 0
    var jp1047 bool
    if t1049 {
        jp1047 = true
    } else {
        var t1050 bool = index__259 >= length__260
        jp1047 = t1050
    }
    if jp1047 {
        var inline1179 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1179
    } else {
        var t934 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, index__259)
        var first__261 uint32 = uint32(uint8(t934))
        var t937 bool = first__261 < 128
        if t937 {
            var inline1181 int = 1
            var inline1182 Option__char = __goml_builtin_char_from_uint32(first__261)
            switch inline1182._tag {
            case 0:
                var inline1183 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1183
            case 1:
                var inline1184 rune = inline1182._v1_0
                var inline1186 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1184,
                    _2: inline1181,
                }
                return inline1186
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t941 bool = first__261 < 194
            if t941 {
                var inline1188 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1188
            } else {
                var t945 bool = first__261 < 224
                if t945 {
                    var t958 int = length__260 - index__259
                    var t959 bool = t958 < 2
                    if t959 {
                        var inline1190 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1190
                    } else {
                        var t947 int = index__259 + 1
                        var t948 uint8
                        var inline1204 uint8 = _goml_runtime_core_string_byte_get(value__258, t947)
                        t948 = inline1204
                        var second__262 uint32 = uint32(uint8(t948))
                        var t951 bool
                        var inline1201 bool = second__262 < 128
                        if inline1201 {
                            t951 = true
                        } else {
                            var inline1202 bool = second__262 > 191
                            t951 = inline1202
                        }
                        if t951 {
                            var inline1192 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1192
                        } else {
                            var t953_rhs uint32 = 31
                            var t953 uint32 = first__261 & t953_rhs
                            var t954_rhs int = 6
                            var t954 uint32 = t953 << t954_rhs
                            var t955_rhs uint32 = 63
                            var t955 uint32 = second__262 & t955_rhs
                            var t956 uint32 = t954 | t955
                            var inline1194 int = 2
                            var inline1195 Option__char = __goml_builtin_char_from_uint32(t956)
                            switch inline1195._tag {
                            case 0:
                                var inline1196 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1196
                            case 1:
                                var inline1197 rune = inline1195._v1_0
                                var inline1199 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1197,
                                    _2: inline1194,
                                }
                                return inline1199
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t963 bool = first__261 < 240
                    if t963 {
                        var t996 int = length__260 - index__259
                        var t997 bool = t996 < 3
                        if t997 {
                            var inline1206 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1206
                        } else {
                            var t965 int = index__259 + 1
                            var t966 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t965)
                            var second__263 uint32 = uint32(uint8(t966))
                            var t967 int = index__259 + 2
                            var t968 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t967)
                            var third__264 uint32 = uint32(uint8(t968))
                            var t994 bool = utf8_invalid_continuation(second__263)
                            var jp989 bool
                            if t994 {
                                jp989 = true
                            } else {
                                var inline1208 bool = third__264 < 128
                                if inline1208 {
                                    jp989 = true
                                } else {
                                    var inline1209 bool = third__264 > 191
                                    jp989 = inline1209
                                }
                            }
                            var jp983 bool
                            if jp989 {
                                jp983 = true
                            } else {
                                var t992 bool = first__261 == 224
                                if t992 {
                                    var t993 bool = second__263 < 160
                                    jp983 = t993
                                } else {
                                    jp983 = false
                                }
                            }
                            var jp972 bool
                            if jp983 {
                                jp972 = true
                            } else {
                                var t986 bool = first__261 == 237
                                if t986 {
                                    var t987 bool = second__263 >= 160
                                    jp972 = t987
                                } else {
                                    jp972 = false
                                }
                            }
                            if jp972 {
                                var inline1211 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1211
                            } else {
                                var t974_rhs uint32 = 15
                                var t974 uint32 = first__261 & t974_rhs
                                var t975_rhs int = 12
                                var t975 uint32 = t974 << t975_rhs
                                var t976_rhs uint32 = 63
                                var t976 uint32 = second__263 & t976_rhs
                                var t977_rhs int = 6
                                var t977 uint32 = t976 << t977_rhs
                                var t978 uint32 = t975 | t977
                                var t979_rhs uint32 = 63
                                var t979 uint32 = third__264 & t979_rhs
                                var t980 uint32 = t978 | t979
                                var inline1213 int = 3
                                var inline1214 Option__char = __goml_builtin_char_from_uint32(t980)
                                switch inline1214._tag {
                                case 0:
                                    var inline1215 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1215
                                case 1:
                                    var inline1216 rune = inline1214._v1_0
                                    var inline1218 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1216,
                                        _2: inline1213,
                                    }
                                    return inline1218
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1001 bool = first__261 < 245
                        if t1001 {
                            var t1042 int = length__260 - index__259
                            var t1043 bool = t1042 < 4
                            if t1043 {
                                var t1044 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1044
                            } else {
                                var t1003 int = index__259 + 1
                                var t1004 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1003)
                                var second__265 uint32 = uint32(uint8(t1004))
                                var t1005 int = index__259 + 2
                                var t1006 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1005)
                                var third__266 uint32 = uint32(uint8(t1006))
                                var t1007 int = index__259 + 3
                                var t1008 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1007)
                                var fourth__267 uint32 = uint32(uint8(t1008))
                                var t1040 bool = utf8_invalid_continuation(second__265)
                                var jp1038 bool
                                if t1040 {
                                    jp1038 = true
                                } else {
                                    var t1041 bool = utf8_invalid_continuation(third__266)
                                    jp1038 = t1041
                                }
                                var jp1032 bool
                                if jp1038 {
                                    jp1032 = true
                                } else {
                                    var t1039 bool = utf8_invalid_continuation(fourth__267)
                                    jp1032 = t1039
                                }
                                var jp1026 bool
                                if jp1032 {
                                    jp1026 = true
                                } else {
                                    var t1035 bool = first__261 == 240
                                    if t1035 {
                                        var t1036 bool = second__265 < 144
                                        jp1026 = t1036
                                    } else {
                                        jp1026 = false
                                    }
                                }
                                var jp1012 bool
                                if jp1026 {
                                    jp1012 = true
                                } else {
                                    var t1029 bool = first__261 == 244
                                    if t1029 {
                                        var t1030 bool = second__265 > 143
                                        jp1012 = t1030
                                    } else {
                                        jp1012 = false
                                    }
                                }
                                if jp1012 {
                                    var t1013 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1013
                                } else {
                                    var t1014_rhs uint32 = 7
                                    var t1014 uint32 = first__261 & t1014_rhs
                                    var t1015_rhs int = 18
                                    var t1015 uint32 = t1014 << t1015_rhs
                                    var t1016_rhs uint32 = 63
                                    var t1016 uint32 = second__265 & t1016_rhs
                                    var t1017_rhs int = 12
                                    var t1017 uint32 = t1016 << t1017_rhs
                                    var t1018 uint32 = t1015 | t1017
                                    var t1019_rhs uint32 = 63
                                    var t1019 uint32 = third__266 & t1019_rhs
                                    var t1020_rhs int = 6
                                    var t1020 uint32 = t1019 << t1020_rhs
                                    var t1021 uint32 = t1018 | t1020
                                    var t1022_rhs uint32 = 63
                                    var t1022 uint32 = fourth__267 & t1022_rhs
                                    var t1023 uint32 = t1021 | t1022
                                    var t1024 Tuple3_4bool_4char_3int = utf8_valid_decode(t1023, 4)
                                    return t1024
                                }
                            }
                        } else {
                            var t1045 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1045
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t1053 string = _goml_runtime_core_bool_to_string(self__401)
    return t1053
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1056 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1056
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1059 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1059
}

func utf8_valid_decode(value__253 uint32, width__254 int) Tuple3_4bool_4char_3int {
    var commute_field1238 rune
    var inline1222 bool = utf8_valid_scalar(value__253)
    if inline1222 {
        var inline1223 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__253)
        var inline1224 rune = inline1223._1
        commute_field1238 = inline1224
        var t1065 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1238,
            _2: width__254,
        }
        return t1065
    } else {
        var inline1220 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1220
    }
}

func utf8_invalid_continuation(value__256 uint32) bool {
    var t1070 bool = value__256 < 128
    if t1070 {
        return true
    } else {
        var t1071 bool = value__256 > 191
        return t1071
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t1076 bool
    var inline1228 bool = value__283 <= 1114111
    if inline1228 {
        var inline1229 bool = value__283 >= 55296
        var inline1231 bool
        if inline1229 {
            var inline1233 bool = value__283 <= 57343
            inline1231 = inline1233
        } else {
            inline1231 = false
        }
        var inline1232 bool = !inline1231
        t1076 = inline1232
    } else {
        t1076 = false
    }
    if t1076 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t1077 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t1077
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t1082 bool = value__257 <= 1114111
    if t1082 {
        var t1086 bool = value__257 >= 55296
        var jp1084 bool
        if t1086 {
            var t1087 bool = value__257 <= 57343
            jp1084 = t1087
        } else {
            jp1084 = false
        }
        var t1085 bool = !jp1084
        return t1085
    } else {
        return false
    }
}

func main() {
    main0()
}
