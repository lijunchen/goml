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

func vec_new__Vec_5uint8() *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: nil,
    }
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
    var inline1102 int = vec_len__Vec_5uint8(bytes__0)
    expected_length__2 = inline1102
    var mtmp796 Tuple2_4bool_6string = string_from_utf8(bytes__0)
    var x797 bool = mtmp796._0
    var x798 string = mtmp796._1
    var t837 bool = x797 == expected__1
    var inline1099 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t837)
    _goml_runtime_core_string_println(inline1099)
    if x797 {
        var t839 int
        var inline1094 int = _goml_runtime_core_string_len(x798)
        t839 = inline1094
        var t840 bool = t839 == expected_length__2
        var inline1091 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t840)
        _goml_runtime_core_string_println(inline1091)
        return struct{}{}
    } else {
        var t842 bool = x798 == ""
        var inline1096 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t842)
        _goml_runtime_core_string_println(inline1096)
        return struct{}{}
    }
}

func check_scalar(bytes__5 *_goml_vec_uint8, expected__6 uint32, expected_width__7 int) struct{} {
    var mtmp800 Tuple2_4bool_6string = string_from_utf8(bytes__5)
    var x801 bool = mtmp800._0
    var x802 string = mtmp800._1
    var inline1129 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x801)
    _goml_runtime_core_string_println(inline1129)
    var commute_field1237 Tuple2_4char_3int
    var inline1118 int = 0
    var inline1119 Tuple3_4bool_4char_3int = string_decode_utf8_at(x802, inline1118)
    var inline1120 bool = inline1119._0
    var inline1121 rune = inline1119._1
    var inline1122 int = inline1119._2
    if inline1120 {
        var inline1126 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1121,
            _1: inline1122,
        }
        commute_field1237 = inline1126
        var x808 rune = commute_field1237._0
        var x809 int = commute_field1237._1
        var t847 uint32 = uint32(rune(x808))
        var t848 bool = t847 == expected__6
        var inline1115 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t848)
        _goml_runtime_core_string_println(inline1115)
        var t849 bool = x809 == expected_width__7
        var inline1112 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t849)
        _goml_runtime_core_string_println(inline1112)
        return struct{}{}
    } else {
        var inline1108 bool = false
        var inline1109 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1108)
        _goml_runtime_core_string_println(inline1109)
        var inline1104 bool = false
        var inline1105 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1104)
        _goml_runtime_core_string_println(inline1105)
        return struct{}{}
    }
}

func main0() struct{} {
    var t852 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__u8()
    check_utf8(t852, true)
    var t853 [1]uint8 = [1]uint8{0}
    var t854 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t853)
    check_utf8(t854, true)
    var t855 [1]uint8 = [1]uint8{127}
    var t856 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t855)
    check_utf8(t856, true)
    var t857 [2]uint8 = [2]uint8{194, 128}
    var t858 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t857)
    check_scalar(t858, 128, 2)
    var t859 [2]uint8 = [2]uint8{223, 191}
    var t860 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t859)
    check_scalar(t860, 2047, 2)
    var t861 [3]uint8 = [3]uint8{224, 160, 128}
    var t862 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t861)
    check_scalar(t862, 2048, 3)
    var t863 [3]uint8 = [3]uint8{237, 159, 191}
    var t864 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t863)
    check_scalar(t864, 55295, 3)
    var t865 [3]uint8 = [3]uint8{238, 128, 128}
    var t866 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t865)
    check_scalar(t866, 57344, 3)
    var t867 [3]uint8 = [3]uint8{239, 191, 189}
    var t868 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t867)
    check_scalar(t868, 65533, 3)
    var t869 [3]uint8 = [3]uint8{239, 191, 191}
    var t870 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t869)
    check_scalar(t870, 65535, 3)
    var t871 [4]uint8 = [4]uint8{240, 144, 128, 128}
    var t872 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t871)
    check_scalar(t872, 65536, 4)
    var t873 [4]uint8 = [4]uint8{244, 143, 191, 191}
    var t874 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t873)
    check_scalar(t874, 1114111, 4)
    var t875 [1]uint8 = [1]uint8{128}
    var t876 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t875)
    check_utf8(t876, false)
    var t877 [1]uint8 = [1]uint8{191}
    var t878 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t877)
    check_utf8(t878, false)
    var t879 [2]uint8 = [2]uint8{192, 128}
    var t880 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t879)
    check_utf8(t880, false)
    var t881 [2]uint8 = [2]uint8{193, 191}
    var t882 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t881)
    check_utf8(t882, false)
    var t883 [1]uint8 = [1]uint8{194}
    var t884 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t883)
    check_utf8(t884, false)
    var t885 [2]uint8 = [2]uint8{194, 127}
    var t886 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t885)
    check_utf8(t886, false)
    var t887 [3]uint8 = [3]uint8{224, 159, 191}
    var t888 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t887)
    check_utf8(t888, false)
    var t889 [2]uint8 = [2]uint8{225, 128}
    var t890 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t889)
    check_utf8(t890, false)
    var t891 [3]uint8 = [3]uint8{225, 128, 127}
    var t892 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t891)
    check_utf8(t892, false)
    var t893 [3]uint8 = [3]uint8{237, 160, 128}
    var t894 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t893)
    check_utf8(t894, false)
    var t895 [4]uint8 = [4]uint8{240, 143, 191, 191}
    var t896 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t895)
    check_utf8(t896, false)
    var t897 [3]uint8 = [3]uint8{240, 144, 128}
    var t898 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t897)
    var inline1162 bool = false
    var inline1163 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(t898)
    var inline1164 Tuple2_4bool_6string = string_from_utf8(t898)
    var inline1165 bool = inline1164._0
    var inline1166 string = inline1164._1
    var inline1169 bool = inline1165 == inline1162
    println__T_bool(inline1169)
    if inline1165 {
        var inline1171 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline1166)
        var inline1172 bool = inline1171 == inline1163
        println__T_bool(inline1172)
    } else {
        var inline1174 bool = inline1166 == ""
        println__T_bool(inline1174)
    }
    var t899 [4]uint8 = [4]uint8{244, 144, 128, 128}
    var t900 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t899)
    var inline1147 bool = false
    var inline1148 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(t900)
    var inline1149 Tuple2_4bool_6string = string_from_utf8(t900)
    var inline1150 bool = inline1149._0
    var inline1151 string = inline1149._1
    var inline1154 bool = inline1150 == inline1147
    println__T_bool(inline1154)
    if inline1150 {
        var inline1156 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline1151)
        var inline1157 bool = inline1156 == inline1148
        println__T_bool(inline1157)
    } else {
        var inline1159 bool = inline1151 == ""
        println__T_bool(inline1159)
    }
    var t901 [4]uint8 = [4]uint8{245, 128, 128, 128}
    var t902 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t901)
    var inline1132 bool = false
    var inline1133 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(t902)
    var inline1134 Tuple2_4bool_6string = string_from_utf8(t902)
    var inline1135 bool = inline1134._0
    var inline1136 string = inline1134._1
    var inline1139 bool = inline1135 == inline1132
    println__T_bool(inline1139)
    if inline1135 {
        var inline1141 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline1136)
        var inline1142 bool = inline1141 == inline1133
        println__T_bool(inline1142)
        return struct{}{}
    } else {
        var inline1144 bool = inline1136 == ""
        println__T_bool(inline1144)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(self__526 *_goml_vec_uint8) int {
    var t906 int = vec_len__Vec_5uint8(self__526)
    return t906
}

func string_from_utf8(bytes__277 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp395 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__277)
    var x397 string = mtmp395._1
    var index__279 int = 0
    Loop_loop911:
    for {
        var t912 int
        var inline1177 int = _goml_runtime_core_string_len(x397)
        t912 = inline1177
        var t913 bool = index__279 < t912
        if t913 {
            var mtmp398 Tuple3_4bool_4char_3int = string_decode_utf8_at(x397, index__279)
            var x399 bool = mtmp398._0
            var x401 int = mtmp398._2
            if x399 {
                var compound_old402 int = index__279
                var t915 int = compound_old402 + x401
                index__279 = t915
                continue
            } else {
                var t917 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t917
            }
        } else {
            break Loop_loop911
        }
    }
    var t910 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x397,
    }
    return t910
}

func println__T_bool(value__1 bool) struct{} {
    var t919 string
    var inline1179 string = _goml_runtime_core_bool_to_string(value__1)
    t919 = inline1179
    _goml_runtime_core_string_println(t919)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t923 int = _goml_runtime_core_string_len(self__289)
    return t923
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__u8() *_goml_vec_uint8 {
    var t932 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t932
}

func string_decode_utf8_at(value__258 string, index__259 int) Tuple3_4bool_4char_3int {
    var length__260 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__258)
    var t1051 bool = index__259 < 0
    var jp1049 bool
    if t1051 {
        jp1049 = true
    } else {
        var t1052 bool = index__259 >= length__260
        jp1049 = t1052
    }
    if jp1049 {
        var inline1181 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1181
    } else {
        var t936 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, index__259)
        var first__261 uint32 = uint32(uint8(t936))
        var t939 bool = first__261 < 128
        if t939 {
            var inline1183 int = 1
            var inline1184 Option__char = __goml_builtin_char_from_uint32(first__261)
            switch inline1184._tag {
            case 0:
                var inline1185 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1185
            case 1:
                var inline1186 rune = inline1184._v1_0
                var inline1188 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1186,
                    _2: inline1183,
                }
                return inline1188
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t943 bool = first__261 < 194
            if t943 {
                var inline1190 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1190
            } else {
                var t947 bool = first__261 < 224
                if t947 {
                    var t960 int = length__260 - index__259
                    var t961 bool = t960 < 2
                    if t961 {
                        var inline1192 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1192
                    } else {
                        var t949 int = index__259 + 1
                        var t950 uint8
                        var inline1206 uint8 = _goml_runtime_core_string_byte_get(value__258, t949)
                        t950 = inline1206
                        var second__262 uint32 = uint32(uint8(t950))
                        var t953 bool
                        var inline1203 bool = second__262 < 128
                        if inline1203 {
                            t953 = true
                        } else {
                            var inline1204 bool = second__262 > 191
                            t953 = inline1204
                        }
                        if t953 {
                            var inline1194 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1194
                        } else {
                            var t955_rhs uint32 = 31
                            var t955 uint32 = first__261 & t955_rhs
                            var t956_rhs int = 6
                            var t956 uint32 = t955 << t956_rhs
                            var t957_rhs uint32 = 63
                            var t957 uint32 = second__262 & t957_rhs
                            var t958 uint32 = t956 | t957
                            var inline1196 int = 2
                            var inline1197 Option__char = __goml_builtin_char_from_uint32(t958)
                            switch inline1197._tag {
                            case 0:
                                var inline1198 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1198
                            case 1:
                                var inline1199 rune = inline1197._v1_0
                                var inline1201 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1199,
                                    _2: inline1196,
                                }
                                return inline1201
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t965 bool = first__261 < 240
                    if t965 {
                        var t998 int = length__260 - index__259
                        var t999 bool = t998 < 3
                        if t999 {
                            var inline1208 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1208
                        } else {
                            var t967 int = index__259 + 1
                            var t968 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t967)
                            var second__263 uint32 = uint32(uint8(t968))
                            var t969 int = index__259 + 2
                            var t970 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t969)
                            var third__264 uint32 = uint32(uint8(t970))
                            var t996 bool = utf8_invalid_continuation(second__263)
                            var jp991 bool
                            if t996 {
                                jp991 = true
                            } else {
                                var inline1210 bool = third__264 < 128
                                if inline1210 {
                                    jp991 = true
                                } else {
                                    var inline1211 bool = third__264 > 191
                                    jp991 = inline1211
                                }
                            }
                            var jp985 bool
                            if jp991 {
                                jp985 = true
                            } else {
                                var t994 bool = first__261 == 224
                                if t994 {
                                    var t995 bool = second__263 < 160
                                    jp985 = t995
                                } else {
                                    jp985 = false
                                }
                            }
                            var jp974 bool
                            if jp985 {
                                jp974 = true
                            } else {
                                var t988 bool = first__261 == 237
                                if t988 {
                                    var t989 bool = second__263 >= 160
                                    jp974 = t989
                                } else {
                                    jp974 = false
                                }
                            }
                            if jp974 {
                                var inline1213 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1213
                            } else {
                                var t976_rhs uint32 = 15
                                var t976 uint32 = first__261 & t976_rhs
                                var t977_rhs int = 12
                                var t977 uint32 = t976 << t977_rhs
                                var t978_rhs uint32 = 63
                                var t978 uint32 = second__263 & t978_rhs
                                var t979_rhs int = 6
                                var t979 uint32 = t978 << t979_rhs
                                var t980 uint32 = t977 | t979
                                var t981_rhs uint32 = 63
                                var t981 uint32 = third__264 & t981_rhs
                                var t982 uint32 = t980 | t981
                                var inline1215 int = 3
                                var inline1216 Option__char = __goml_builtin_char_from_uint32(t982)
                                switch inline1216._tag {
                                case 0:
                                    var inline1217 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1217
                                case 1:
                                    var inline1218 rune = inline1216._v1_0
                                    var inline1220 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1218,
                                        _2: inline1215,
                                    }
                                    return inline1220
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1003 bool = first__261 < 245
                        if t1003 {
                            var t1044 int = length__260 - index__259
                            var t1045 bool = t1044 < 4
                            if t1045 {
                                var t1046 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1046
                            } else {
                                var t1005 int = index__259 + 1
                                var t1006 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1005)
                                var second__265 uint32 = uint32(uint8(t1006))
                                var t1007 int = index__259 + 2
                                var t1008 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1007)
                                var third__266 uint32 = uint32(uint8(t1008))
                                var t1009 int = index__259 + 3
                                var t1010 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1009)
                                var fourth__267 uint32 = uint32(uint8(t1010))
                                var t1042 bool = utf8_invalid_continuation(second__265)
                                var jp1040 bool
                                if t1042 {
                                    jp1040 = true
                                } else {
                                    var t1043 bool = utf8_invalid_continuation(third__266)
                                    jp1040 = t1043
                                }
                                var jp1034 bool
                                if jp1040 {
                                    jp1034 = true
                                } else {
                                    var t1041 bool = utf8_invalid_continuation(fourth__267)
                                    jp1034 = t1041
                                }
                                var jp1028 bool
                                if jp1034 {
                                    jp1028 = true
                                } else {
                                    var t1037 bool = first__261 == 240
                                    if t1037 {
                                        var t1038 bool = second__265 < 144
                                        jp1028 = t1038
                                    } else {
                                        jp1028 = false
                                    }
                                }
                                var jp1014 bool
                                if jp1028 {
                                    jp1014 = true
                                } else {
                                    var t1031 bool = first__261 == 244
                                    if t1031 {
                                        var t1032 bool = second__265 > 143
                                        jp1014 = t1032
                                    } else {
                                        jp1014 = false
                                    }
                                }
                                if jp1014 {
                                    var t1015 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1015
                                } else {
                                    var t1016_rhs uint32 = 7
                                    var t1016 uint32 = first__261 & t1016_rhs
                                    var t1017_rhs int = 18
                                    var t1017 uint32 = t1016 << t1017_rhs
                                    var t1018_rhs uint32 = 63
                                    var t1018 uint32 = second__265 & t1018_rhs
                                    var t1019_rhs int = 12
                                    var t1019 uint32 = t1018 << t1019_rhs
                                    var t1020 uint32 = t1017 | t1019
                                    var t1021_rhs uint32 = 63
                                    var t1021 uint32 = third__266 & t1021_rhs
                                    var t1022_rhs int = 6
                                    var t1022 uint32 = t1021 << t1022_rhs
                                    var t1023 uint32 = t1020 | t1022
                                    var t1024_rhs uint32 = 63
                                    var t1024 uint32 = fourth__267 & t1024_rhs
                                    var t1025 uint32 = t1023 | t1024
                                    var t1026 Tuple3_4bool_4char_3int = utf8_valid_decode(t1025, 4)
                                    return t1026
                                }
                            }
                        } else {
                            var t1047 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1047
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t1055 string = _goml_runtime_core_bool_to_string(self__401)
    return t1055
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1058 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1058
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1061 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1061
}

func utf8_valid_decode(value__253 uint32, width__254 int) Tuple3_4bool_4char_3int {
    var commute_field1240 rune
    var inline1224 bool = utf8_valid_scalar(value__253)
    if inline1224 {
        var inline1225 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__253)
        var inline1226 rune = inline1225._1
        commute_field1240 = inline1226
        var t1067 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1240,
            _2: width__254,
        }
        return t1067
    } else {
        var inline1222 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1222
    }
}

func utf8_invalid_continuation(value__256 uint32) bool {
    var t1072 bool = value__256 < 128
    if t1072 {
        return true
    } else {
        var t1073 bool = value__256 > 191
        return t1073
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t1078 bool
    var inline1230 bool = value__283 <= 1114111
    if inline1230 {
        var inline1231 bool = value__283 >= 55296
        var inline1233 bool
        if inline1231 {
            var inline1235 bool = value__283 <= 57343
            inline1233 = inline1235
        } else {
            inline1233 = false
        }
        var inline1234 bool = !inline1233
        t1078 = inline1234
    } else {
        t1078 = false
    }
    if t1078 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t1079 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t1079
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t1084 bool = value__257 <= 1114111
    if t1084 {
        var t1088 bool = value__257 >= 55296
        var jp1086 bool
        if t1088 {
            var t1089 bool = value__257 <= 57343
            jp1086 = t1089
        } else {
            jp1086 = false
        }
        var t1087 bool = !jp1086
        return t1087
    } else {
        return false
    }
}

func main() {
    main0()
}
