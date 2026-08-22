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

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
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

type Token struct {}

type Any struct {}

type Counter struct {
    current *ref_int32_x
    end int32
}

type FnIterator__i32 struct {
    next_fn func() Option__i32
}

type FnIterator__isize struct {
    next_fn func() Option__isize
}

type FnIterator__string struct {
    next_fn func() Option__string
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {}

type closure_env_std_iter_map_A_i32_B_i32_I_Counter_4 struct {
    iterator_0 Counter
    map_fn_1 func(int32) int32
}

type closure_env_std_iter_filter_I_FnIterator_i32_T_i32_5 struct {
    iterator_0 FnIterator__i32
    predicate_1 func(int32) bool
}

type closure_env_std_iter_take_I_FnIterator_i32_6 struct {
    remaining_0 *ref_int_x
    iterator_1 FnIterator__i32
}

type closure_env_std_iter_map_A_isize_B_string_I_FnIterator_isize_7 struct {
    iterator_0 FnIterator__isize
    map_fn_1 func(int) string
}

type closure_env_goml_builtin_range_8 struct {
    current_0 *ref_int_x
    end_1 int
}

type Ordering int32

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func _goml_m_trait__impl_i_Convert_i__l_i32_r__x40_Token_i_convert(self__0 Token) int32 {
    return 7
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    return "seven"
}

func main0() struct{} {
    var t843 Token = Token{}
    var t844 int32 = _goml_m_trait__impl_i_Convert_i__l_i32_r__x40_Token_i_convert(t843)
    println__T_i32(t844)
    var t845 Token = Token{}
    var t846 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t845)
    println__T_string(t846)
    var t847 Token = Token{}
    var converted__8 int32 = convert_to__T_i32__V_Token(t847)
    println__T_i32(converted__8)
    var t849 string
    t849 = "marked"
    println__T_string(t849)
    var t851 string
    t851 = "marked"
    println__T_string(t851)
    var t853 string
    t853 = "marked"
    var inline1156 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t853)
    _goml_runtime_core_string_println(inline1156)
    var t855 Counter
    var inline1151 int32 = 0
    var inline1152 int32 = 8
    var inline1153 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(inline1151)
    var inline1154 Counter = Counter{
        current: inline1153,
        end: inline1152,
    }
    t855 = inline1154
    var t856 closure_env_main_0 = closure_env_main_0{}
    var t857 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t856, p0)
    }
    var mapped__10 FnIterator__i32
    var inline1147 closure_env_std_iter_map_A_i32_B_i32_I_Counter_4 = closure_env_std_iter_map_A_i32_B_i32_I_Counter_4{
        iterator_0: t855,
        map_fn_1: t857,
    }
    var inline1148 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__en_h645d7a9bc4d79b01cd03faf046af5461_nter__4_i_apply(inline1147)
    }
    var inline1149 FnIterator__i32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__i32(inline1148)
    mapped__10 = inline1149
    var t858 closure_env_main_1 = closure_env_main_1{}
    var t859 func(int32) bool = func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t858, p0)
    }
    var filtered__12 FnIterator__i32
    var inline1143 closure_env_std_iter_filter_I_FnIterator_i32_T_i32_5 = closure_env_std_iter_filter_I_FnIterator_i32_T_i32_5{
        iterator_0: mapped__10,
        predicate_1: t859,
    }
    var inline1144 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__en_h8f163b2d5b8bf9739c89e2204772b07d__i32__5_i_apply(inline1143)
    }
    var inline1145 FnIterator__i32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__i32(inline1144)
    filtered__12 = inline1145
    var limited__13 FnIterator__i32
    var inline1134 int = 3
    var inline1135 bool = inline1134 > 0
    var inline1137 int
    if inline1135 {
        inline1137 = inline1134
    } else {
        inline1137 = 0
    }
    var inline1138 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(inline1137)
    var inline1139 closure_env_std_iter_take_I_FnIterator_i32_6 = closure_env_std_iter_take_I_FnIterator_i32_6{
        remaining_0: inline1138,
        iterator_1: filtered__12,
    }
    var inline1140 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__en_hbf515b0203b88ffdb3eaded6d77747ee__i32__6_i_apply(inline1139)
    }
    var inline1141 FnIterator__i32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__i32(inline1140)
    limited__13 = inline1141
    var for_iter803 FnIterator__i32
    for_iter803 = limited__13
    Loop_loop875:
    for {
        var for_next804 Option__i32
        var inline1112 func() Option__i32 = for_iter803.next_fn
        var inline1113 Option__i32 = inline1112()
        for_next804 = inline1113
        switch for_next804._tag {
        case 0:
            break Loop_loop875
        case 1:
            var x805 int32 = for_next804._v1_0
            var inline1109 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x805)
            _goml_runtime_core_string_println(inline1109)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t862 FnIterator__isize
    var inline1129 int = 1
    var inline1130 int = 5
    var inline1131 FnIterator__isize = __goml_builtin_range(inline1129, inline1130)
    t862 = inline1131
    var t863 closure_env_main_2 = closure_env_main_2{}
    var t864 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t863, p0, p1)
    }
    var sum__17 int = _goml_m_std_p_iter_p_fold____A__isize____I__FnIterator_l_isize_r_____T__isize(t862, 0, t864)
    var inline1126 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(sum__17)
    _goml_runtime_core_string_println(inline1126)
    var t867 FnIterator__isize
    var inline1122 int = 1
    var inline1123 int = 4
    var inline1124 FnIterator__isize = __goml_builtin_range(inline1122, inline1123)
    t867 = inline1124
    var t868 closure_env_main_3 = closure_env_main_3{}
    var t869 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t868, p0)
    }
    var t870 FnIterator__string
    var inline1118 closure_env_std_iter_map_A_isize_B_string_I_FnIterator_isize_7 = closure_env_std_iter_map_A_isize_B_string_I_FnIterator_isize_7{
        iterator_0: t867,
        map_fn_1: t869,
    }
    var inline1119 func() Option__string = func() Option__string {
        return _goml_m_inherent_i_closure__en_h646bacd23126c6108881c7c439733cbb_size__7_i_apply(inline1118)
    }
    var inline1120 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(inline1119)
    t870 = inline1120
    var texts__19 *_goml_vec_string = _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(t870)
    var for_limit810 int = vec_len__Vec_6string(texts__19)
    var for_index811 int = 0
    Loop_loop872:
    for {
        var t873 bool = for_index811 < for_limit810
        if t873 {
            var for_item812 string = vec_get__Vec_6string(texts__19, for_index811)
            var t874 int = for_index811 + 1
            for_index811 = t874
            var inline1115 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item812)
            _goml_runtime_core_string_println(inline1115)
            continue
        } else {
            break Loop_loop872
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(value__684 int32) *ref_int32_x {
    var t879 *ref_int32_x = ref__Ref_5int32(value__684)
    return t879
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(self__685 *ref_int32_x) int32 {
    var t882 int32 = ref_get__Ref_5int32(self__685)
    return t882
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(self__686 *ref_int32_x, value__687 int32) struct{} {
    ref_set__Ref_5int32(self__686, value__687)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t886 string
    var inline1162 string = __goml_builtin_int32_to_string(value__1)
    t886 = inline1162
    _goml_runtime_core_string_println(t886)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t889 string
    t889 = value__1
    _goml_runtime_core_string_println(t889)
    return struct{}{}
}

func convert_to__T_i32__V_Token(value__2 Token) int32 {
    return 7
}

func _goml_m_std_p_iter_p_fold____A__isize____I__FnIterator_l_isize_r_____T__isize(iterator__48 FnIterator__isize, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr927:
    for {
        var mtmp43 Option__isize
        var inline1174 func() Option__isize = iterator__48.next_fn
        var inline1175 Option__isize = inline1174()
        mtmp43 = inline1175
        switch mtmp43._tag {
        case 0:
            break Loop_loop_expr927
        case 1:
            var x44 int = mtmp43._v1_0
            var t929 int = combine__50(accumulator__51, x44)
            accumulator__51 = t929
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(iterator__53 FnIterator__string) *_goml_vec_string {
    var values__54 *_goml_vec_string
    var inline1189 *_goml_vec_string = vec_new__Vec_6string()
    values__54 = inline1189
    Loop_loop_expr939:
    for {
        var mtmp47 Option__string
        var inline1186 func() Option__string = iterator__53.next_fn
        var inline1187 Option__string = inline1186()
        mtmp47 = inline1187
        switch mtmp47._tag {
        case 0:
            break Loop_loop_expr939
        case 1:
            var x48 string = mtmp47._v1_0
            vec_push__Vec_6string(values__54, x48)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return values__54
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline1196 int64 = int64(int32(self__407))
    var inline1197 string = signed_decimal_string(inline1196)
    return inline1197
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__i32(next_fn__507 func() Option__i32) FnIterator__i32 {
    var t957 FnIterator__i32 = FnIterator__i32{
        next_fn: next_fn__507,
    }
    return t957
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__684 int) *ref_int_x {
    var t960 *ref_int_x = ref__Ref_3int(value__684)
    return t960
}

func __goml_builtin_range(start__756 int, end__757 int) FnIterator__isize {
    var current__758 *ref_int_x = ref__Ref_3int(start__756)
    var t972 closure_env_goml_builtin_range_8 = closure_env_goml_builtin_range_8{
        current_0: current__758,
        end_1: end__757,
    }
    var t973 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(t972)
    }
    var inline1199 FnIterator__isize = FnIterator__isize{
        next_fn: t973,
    }
    return inline1199
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1201 int64 = int64(int(self__404))
    var inline1202 string = signed_decimal_string(inline1201)
    return inline1202
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__507 func() Option__string) FnIterator__string {
    var t989 FnIterator__string = FnIterator__string{
        next_fn: next_fn__507,
    }
    return t989
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t992 int64 = int64(int(value__222))
    var inline1204 bool = t992 < 0
    if inline1204 {
        var inline1205 uint64 = uint64(int64(t992))
        var inline1206 uint64 = 0 - inline1205
        var inline1207 string = decimal_string(inline1206)
        var inline1208 string = "-" + inline1207
        return inline1208
    } else {
        var inline1209 uint64 = uint64(int64(t992))
        var inline1210 string = decimal_string(inline1209)
        return inline1210
    }
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t996 int64 = int64(int32(value__225))
    var inline1212 bool = t996 < 0
    if inline1212 {
        var inline1213 uint64 = uint64(int64(t996))
        var inline1214 uint64 = 0 - inline1213
        var inline1215 string = decimal_string(inline1214)
        var inline1216 string = "-" + inline1215
        return inline1216
    } else {
        var inline1217 uint64 = uint64(int64(t996))
        var inline1218 string = decimal_string(inline1217)
        return inline1218
    }
}

func signed_decimal_string(value__214 int64) string {
    var t1005 bool = value__214 < 0
    if t1005 {
        var t1006 uint64 = uint64(int64(value__214))
        var t1007 uint64 = 0 - t1006
        var t1008 string = decimal_string(t1007)
        var t1009 string = "-" + t1008
        return t1009
    } else {
        var t1010 uint64 = uint64(int64(value__214))
        var t1011 string = decimal_string(t1010)
        return t1011
    }
}

func decimal_string(value__208 uint64) string {
    var t1034 bool = value__208 == 0
    if t1034 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1027:
        for {
            var t1028 bool = remaining__210 > 0
            if t1028 {
                var t1029_rhs uint64 = 10
                var t1029 uint64 = remaining__210 % t1029_rhs
                var t1030 uint8 = uint8(uint64(t1029))
                var t1031 uint8 = t1030 + 48
                vec_push__Vec_5uint8(reversed__209, t1031)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1032 uint64 = compound_old353 / compound_value354
                remaining__210 = t1032
                continue
            } else {
                break Loop_loop1027
            }
        }
        var t1016 int
        var inline1228 int = vec_len__Vec_5uint8(reversed__209)
        t1016 = inline1228
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1016)
        var offset__212 int = 0
        Loop_loop1018:
        for {
            var t1019 int
            var inline1226 int = vec_len__Vec_5uint8(reversed__209)
            t1019 = inline1226
            var t1020 bool = offset__212 < t1019
            if t1020 {
                var t1021 int
                var inline1224 int = vec_len__Vec_5uint8(reversed__209)
                t1021 = inline1224
                var t1022 int = t1021 - offset__212
                var t1023 int = t1022 - 1
                var t1024 uint8 = vec_get__Vec_5uint8(reversed__209, t1023)
                vec_push__Vec_5uint8(bytes__211, t1024)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1025 int = compound_old358 + compound_value359
                offset__212 = t1025
                continue
            } else {
                break Loop_loop1018
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env815 closure_env_main_0, value__9 int32) int32 {
    var t1054 int32 = value__9 * 2
    return t1054
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env816 closure_env_main_1, value__11 int32) bool {
    var t1057 bool = value__11 > 4
    return t1057
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env817 closure_env_main_2, total__15 int, value__16 int) int {
    var t1060 int = total__15 + value__16
    return t1060
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env818 closure_env_main_3, value__18 int) string {
    var t1063 string
    var inline1230 string = __goml_builtin_int_to_string(value__18)
    t1063 = inline1230
    var t1064 string = "v" + t1063
    return t1064
}

func _goml_m_inherent_i_closure__en_h645d7a9bc4d79b01cd03faf046af5461_nter__4_i_apply(env819 closure_env_std_iter_map_A_i32_B_i32_I_Counter_4) Option__i32 {
    var iterator__4 Counter = env819.iterator_0
    var map_fn__5 func(int32) int32 = env819.map_fn_1
    var commute_field1265 int32
    var inline1232 *ref_int32_x = iterator__4.current
    var inline1233 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1232)
    var inline1234 int32 = iterator__4.end
    var inline1235 bool = inline1233 < inline1234
    if inline1235 {
        var inline1236 *ref_int32_x = iterator__4.current
        var inline1237 int32 = inline1233 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline1236, inline1237)
        commute_field1265 = inline1233
        var t1069 int32 = map_fn__5(commute_field1265)
        var t1070 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: t1069,
        }
        return t1070
    } else {
        return Option__i32{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h8f163b2d5b8bf9739c89e2204772b07d__i32__5_i_apply(env820 closure_env_std_iter_filter_I_FnIterator_i32_T_i32_5) Option__i32 {
    var iterator__7 FnIterator__i32 = env820.iterator_0
    var predicate__8 func(int32) bool = env820.predicate_1
    for {
        var mtmp3 Option__i32
        var inline1241 func() Option__i32 = iterator__7.next_fn
        var inline1242 Option__i32 = inline1241()
        mtmp3 = inline1242
        switch mtmp3._tag {
        case 0:
            return Option__i32{
                _tag: 0,
            }
        case 1:
            var x4 int32 = mtmp3._v1_0
            var t1078 bool = predicate__8(x4)
            if t1078 {
                var t1079 Option__i32 = Option__i32{
                    _tag: 1,
                    _v1_0: x4,
                }
                return t1079
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_inherent_i_closure__en_hbf515b0203b88ffdb3eaded6d77747ee__i32__6_i_apply(env821 closure_env_std_iter_take_I_FnIterator_i32_6) Option__i32 {
    var remaining__16 *ref_int_x = env821.remaining_0
    var iterator__14 FnIterator__i32 = env821.iterator_1
    var t1084 int
    var inline1251 int = ref_get__Ref_3int(remaining__16)
    t1084 = inline1251
    var t1085 bool = t1084 == 0
    if t1085 {
        return Option__i32{
            _tag: 0,
        }
    } else {
        var t1086 int
        var inline1249 int = ref_get__Ref_3int(remaining__16)
        t1086 = inline1249
        var t1087 int = t1086 - 1
        ref_set__Ref_3int(remaining__16, t1087)
        var inline1244 func() Option__i32 = iterator__14.next_fn
        var inline1245 Option__i32 = inline1244()
        return inline1245
    }
}

func _goml_m_inherent_i_closure__en_h646bacd23126c6108881c7c439733cbb_size__7_i_apply(env822 closure_env_std_iter_map_A_isize_B_string_I_FnIterator_isize_7) Option__string {
    var iterator__4 FnIterator__isize = env822.iterator_0
    var map_fn__5 func(int) string = env822.map_fn_1
    var mtmp1 Option__isize
    var inline1253 func() Option__isize = iterator__4.next_fn
    var inline1254 Option__isize = inline1253()
    mtmp1 = inline1254
    switch mtmp1._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x2 int = mtmp1._v1_0
        var t1093 string = map_fn__5(x2)
        var t1094 Option__string = Option__string{
            _tag: 1,
            _v1_0: t1093,
        }
        return t1094
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(env823 closure_env_goml_builtin_range_8) Option__isize {
    var current__758 *ref_int_x = env823.current_0
    var end__757 int = env823.end_1
    var value__759 int = ref_get__Ref_3int(current__758)
    var t1099 bool = value__759 < end__757
    if t1099 {
        var t1100 int = value__759 + 1
        ref_set__Ref_3int(current__758, t1100)
        var t1101 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__759,
        }
        return t1101
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
