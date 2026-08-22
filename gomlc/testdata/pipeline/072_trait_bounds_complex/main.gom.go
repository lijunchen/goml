package main

import (
    _goml_os "os"
)

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

type Boxed struct {
    value int32
}

type Ordering int32

func _goml_m_trait__impl_i_Display_i_i32_i_show(self__0 int32) string {
    var inline1012 string = __goml_builtin_int32_to_string(self__0)
    return inline1012
}

func _goml_m_trait__impl_i_Debug_i_i32_i_show(self__1 int32) string {
    var t805 string
    var inline1014 string = __goml_builtin_int32_to_string(self__1)
    t805 = inline1014
    var t806 string = "i32(" + t805
    var t807 string = t806 + ")"
    return t807
}

func _goml_m_trait__impl_i_MyHash_i_i32_i_hash(self__4 int32) int32 {
    var t813 int32 = self__4 * 16777619
    var t814 int32 = t813 + 216613626
    return t814
}

func _goml_m_trait__impl_i_Add_i_i32_i_add(self__5 int32, other__6 int32) int32 {
    var t817 int32 = self__5 + other__6
    return t817
}

func _goml_m_trait__impl_i_Inspect_i_i32_i_inspect(self__9 int32) string {
    var t823 string
    var inline1016 string = __goml_builtin_int32_to_string(self__9)
    t823 = inline1016
    var t824 string = "<" + t823
    var t825 string = t824 + ">"
    return t825
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var t828 int32 = self__10.value
    var t829 string
    var inline1018 string = __goml_builtin_int32_to_string(t828)
    t829 = inline1018
    var t830 string = "Boxed(" + t829
    var t831 string = t830 + ")"
    return t831
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var t834 int32 = self__11.value
    var t835 string
    var inline1020 string = __goml_builtin_int32_to_string(t834)
    t835 = inline1020
    var t836 string = "Boxed{value=" + t835
    var t837 string = t836 + "}"
    return t837
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var t845 int32 = self__14.value
    var t846 int32 = t845 * 31
    var t847 int32 = t846 + 7
    var t848 int32 = t847 * 1315423911
    return t848
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var t851 int32 = self__15.value
    var t852 int32 = other__16.value
    var t853 int32 = t851 + t852
    var t854 Boxed = Boxed{
        value: t853,
    }
    return t854
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var t862 int32 = self__19.value
    var t863 string
    var inline1022 string = __goml_builtin_int32_to_string(t862)
    t863 = inline1022
    var t864 string = "[" + t863
    var t865 string = t864 + "]"
    return t865
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t871 string
    var inline1061 int32 = combine_scaled__T_i32(left__46, right__47, 2)
    var inline1062 string = report_pair__Q_i32__T_i32(tag__45, left__46, right__47, inline1061)
    t871 = inline1062
    var inline1058 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t871)
    _goml_runtime_core_string_println(inline1058)
    var t872 Boxed = Boxed{
        value: 99,
    }
    var t873 Boxed = Boxed{
        value: 3,
    }
    var t874 Boxed = Boxed{
        value: 4,
    }
    var t875 string
    var inline1055 Boxed = combine_scaled__T_Boxed(t873, t874, 2)
    var inline1056 string = report_pair__Q_Boxed__T_Boxed(t872, t873, t874, inline1055)
    t875 = inline1056
    var inline1052 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t875)
    _goml_runtime_core_string_println(inline1052)
    var t876 string
    var inline1041 int32 = _goml_m_trait__impl_i_Add_i_i32_i_add(first__49, second__50)
    var inline1042 int32 = _goml_m_trait__impl_i_Add_i_i32_i_add(inline1041, third__51)
    var inline1043 string = tag_text__Q_i32(sum_tag__48)
    var inline1044 int32 = _goml_m_trait__impl_i_MyHash_i_i32_i_hash(inline1042)
    var inline1045 string = inline1043 + " "
    var inline1046 string = _goml_m_trait__impl_i_Inspect_i_i32_i_inspect(inline1042)
    var inline1047 string = inline1045 + inline1046
    var inline1048 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline1044)
    var inline1049 string = " @" + inline1048
    var inline1050 string = inline1047 + inline1049
    t876 = inline1050
    var inline1038 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t876)
    _goml_runtime_core_string_println(inline1038)
    var t877 Boxed = Boxed{
        value: 1,
    }
    var t878 Boxed = Boxed{
        value: 5,
    }
    var t879 Boxed = Boxed{
        value: 6,
    }
    var t880 Boxed = Boxed{
        value: 7,
    }
    var t881 string
    var inline1027 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t878, t879)
    var inline1028 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(inline1027, t880)
    var inline1029 string = tag_text__Q_Boxed(t877)
    var inline1030 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(inline1028)
    var inline1031 string = inline1029 + " "
    var inline1032 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(inline1028)
    var inline1033 string = inline1031 + inline1032
    var inline1034 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline1030)
    var inline1035 string = " @" + inline1034
    var inline1036 string = inline1033 + inline1035
    t881 = inline1036
    var inline1024 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t881)
    _goml_runtime_core_string_println(inline1024)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline1064 int64 = int64(int32(self__286))
    var inline1065 string = signed_decimal_string(inline1064)
    return inline1065
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t914 int64 = int64(int32(value__225))
    var inline1149 bool = t914 < 0
    if inline1149 {
        var inline1150 uint64 = uint64(int64(t914))
        var inline1151 uint64 = 0 - inline1150
        var inline1152 string = decimal_string(inline1151)
        var inline1153 string = "-" + inline1152
        return inline1153
    } else {
        var inline1154 uint64 = uint64(int64(t914))
        var inline1155 string = decimal_string(inline1154)
        return inline1155
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func combine_scaled__T_i32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t920 int32
    var inline1159 int32 = a__23 + b__24
    t920 = inline1159
    var inline1157 int32 = t920 * factor__25
    return inline1157
}

func report_pair__Q_i32__T_i32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool
    var inline1178 bool = a__27 == b__28
    same__30 = inline1178
    var header__31 string
    var inline1172 string = _goml_m_trait__impl_i_Debug_i_i32_i_show(tag__26)
    var inline1173 string = inline1172 + "#"
    var inline1174 int32 = _goml_m_trait__impl_i_MyHash_i_i32_i_hash(tag__26)
    var inline1175 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline1174)
    var inline1176 string = inline1173 + inline1175
    header__31 = inline1176
    var repr__32 string
    var inline1167 string = _goml_m_trait__impl_i_Debug_i_i32_i_show(combined__29)
    var inline1168 string = inline1167 + " / "
    var inline1169 string = _goml_m_trait__impl_i_Display_i_i32_i_show(combined__29)
    var inline1170 string = inline1168 + inline1169
    repr__32 = inline1170
    var h__33 int32
    var inline1164 int32 = combined__29 * 16777619
    var inline1165 int32 = inline1164 + 216613626
    h__33 = inline1165
    var t924 string = header__31 + " "
    var t925 string = t924 + repr__32
    var t926 string
    if same__30 {
        t926 = "true"
    } else {
        t926 = "false"
    }
    var t927 string = " | eq=" + t926
    var t928 string
    var inline1161 string = __goml_builtin_int32_to_string(h__33)
    t928 = inline1161
    var t929 string = " | hash=" + t928
    var t930 string = t927 + t929
    var t931 string = t925 + t930
    return t931
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t934 Boxed
    var inline1184 int32 = a__23.value
    var inline1185 int32 = b__24.value
    var inline1186 int32 = inline1184 + inline1185
    var inline1187 Boxed = Boxed{
        value: inline1186,
    }
    t934 = inline1187
    var inline1180 int32 = t934.value
    var inline1181 int32 = inline1180 * factor__25
    var inline1182 Boxed = Boxed{
        value: inline1181,
    }
    return inline1182
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool
    var inline1208 int32 = a__27.value
    var inline1209 int32 = b__28.value
    var inline1210 bool = inline1208 == inline1209
    same__30 = inline1210
    var header__31 string
    var inline1202 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__26)
    var inline1203 string = inline1202 + "#"
    var inline1204 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__26)
    var inline1205 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline1204)
    var inline1206 string = inline1203 + inline1205
    header__31 = inline1206
    var repr__32 string
    var inline1197 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(combined__29)
    var inline1198 string = inline1197 + " / "
    var inline1199 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(combined__29)
    var inline1200 string = inline1198 + inline1199
    repr__32 = inline1200
    var h__33 int32
    var inline1192 int32 = combined__29.value
    var inline1193 int32 = inline1192 * 31
    var inline1194 int32 = inline1193 + 7
    var inline1195 int32 = inline1194 * 1315423911
    h__33 = inline1195
    var t938 string = header__31 + " "
    var t939 string = t938 + repr__32
    var t940 string
    if same__30 {
        t940 = "true"
    } else {
        t940 = "false"
    }
    var t941 string = " | eq=" + t940
    var t942 string
    var inline1189 string = __goml_builtin_int32_to_string(h__33)
    t942 = inline1189
    var t943 string = " | hash=" + t942
    var t944 string = t941 + t943
    var t945 string = t939 + t944
    return t945
}

func tag_text__Q_i32(tag__22 int32) string {
    var t948 string
    var inline1217 string = _goml_m_inherent_i_i32_i_i32_i_to__string(tag__22)
    var inline1218 string = "i32(" + inline1217
    var inline1219 string = inline1218 + ")"
    t948 = inline1219
    var t949 string = t948 + "#"
    var t950 int32
    var inline1214 int32 = tag__22 * 16777619
    var inline1215 int32 = inline1214 + 216613626
    t950 = inline1215
    var t951 string
    var inline1212 string = __goml_builtin_int32_to_string(t950)
    t951 = inline1212
    var t952 string = t949 + t951
    return t952
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t955 string
    var inline1228 int32 = tag__22.value
    var inline1229 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline1228)
    var inline1230 string = "Boxed{value=" + inline1229
    var inline1231 string = inline1230 + "}"
    t955 = inline1231
    var t956 string = t955 + "#"
    var t957 int32
    var inline1223 int32 = tag__22.value
    var inline1224 int32 = inline1223 * 31
    var inline1225 int32 = inline1224 + 7
    var inline1226 int32 = inline1225 * 1315423911
    t957 = inline1226
    var t958 string
    var inline1221 string = __goml_builtin_int32_to_string(t957)
    t958 = inline1221
    var t959 string = t956 + t958
    return t959
}

func signed_decimal_string(value__214 int64) string {
    var t964 bool = value__214 < 0
    if t964 {
        var t965 uint64 = uint64(int64(value__214))
        var t966 uint64 = 0 - t965
        var t967 string = decimal_string(t966)
        var t968 string = "-" + t967
        return t968
    } else {
        var t969 uint64 = uint64(int64(value__214))
        var t970 string = decimal_string(t969)
        return t970
    }
}

func decimal_string(value__208 uint64) string {
    var t1005 bool = value__208 == 0
    if t1005 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop998:
        for {
            var t999 bool = remaining__210 > 0
            if t999 {
                var t1000_rhs uint64 = 10
                var t1000 uint64 = remaining__210 % t1000_rhs
                var t1001 uint8 = uint8(uint64(t1000))
                var t1002 uint8 = t1001 + 48
                vec_push__Vec_5uint8(reversed__209, t1002)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1003 uint64 = compound_old353 / compound_value354
                remaining__210 = t1003
                continue
            } else {
                break Loop_loop998
            }
        }
        var t987 int
        var inline1257 int = vec_len__Vec_5uint8(reversed__209)
        t987 = inline1257
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t987)
        var offset__212 int = 0
        Loop_loop989:
        for {
            var t990 int
            var inline1255 int = vec_len__Vec_5uint8(reversed__209)
            t990 = inline1255
            var t991 bool = offset__212 < t990
            if t991 {
                var t992 int
                var inline1253 int = vec_len__Vec_5uint8(reversed__209)
                t992 = inline1253
                var t993 int = t992 - offset__212
                var t994 int = t993 - 1
                var t995 uint8 = vec_get__Vec_5uint8(reversed__209, t994)
                vec_push__Vec_5uint8(bytes__211, t995)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t996 int = compound_old358 + compound_value359
                offset__212 = t996
                continue
            } else {
                break Loop_loop989
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
