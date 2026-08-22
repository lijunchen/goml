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

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

type _goml_vec_Tuple2_5int32_6string struct {
    items []Tuple2_5int32_6string
}

func vec_new__Vec_21Tuple2_5int32_6string() *_goml_vec_Tuple2_5int32_6string {
    return &_goml_vec_Tuple2_5int32_6string{
        items: nil,
    }
}

func vec_push__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string, elem Tuple2_5int32_6string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string, index int) Tuple2_5int32_6string {
    return vec.items[index]
}

func vec_len__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string) int {
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

type Tuple2_5int32_6string struct {
    _0 int32
    _1 string
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

type FnIterator__i32 struct {
    next_fn func() Option__i32
}

type FnIterator__isize struct {
    next_fn func() Option__isize
}

type closure_env_countdown_0 struct {
    current_0 *ref_int32_x
}

type closure_env_goml_builtin_range_1 struct {
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

func counted_range(calls__3 *ref_int32_x) FnIterator__isize {
    var t862 int32
    var inline1077 int32 = ref_get__Ref_5int32(calls__3)
    t862 = inline1077
    var t863 int32 = t862 + 1
    ref_set__Ref_5int32(calls__3, t863)
    var inline1071 int = 1
    var inline1072 int = 5
    var inline1073 FnIterator__isize = __goml_builtin_range(inline1071, inline1072)
    return inline1073
}

func first_even(values__4 FnIterator__isize) int {
    var for_iter798 FnIterator__isize
    for_iter798 = values__4
    Loop_loop868:
    for {
        var for_next799 Option__isize
        var inline1079 func() Option__isize = for_iter798.next_fn
        var inline1080 Option__isize = inline1079()
        for_next799 = inline1080
        switch for_next799._tag {
        case 0:
            break Loop_loop868
        case 1:
            var x800 int = for_next799._v1_0
            var t871 int = x800 / 2
            var t872 int = t871 * 2
            var t873 bool = t872 == x800
            if t873 {
                return x800
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    return -1
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(0)
    var for_limit807 int = vec_len__Vec_5int32(values__6)
    var for_index808 int = 0
    Loop_loop911:
    for {
        var t912 bool = for_index808 < for_limit807
        if t912 {
            var for_item809 int32 = vec_get__Vec_5int32(values__6, for_index808)
            var t913 int = for_index808 + 1
            for_index808 = t913
            var t917 bool = for_item809 == 20
            if t917 {
                continue
            } else {
                var t915 int32
                var inline1085 int32 = ref_get__Ref_5int32(sum__7)
                t915 = inline1085
                var t916 int32 = t915 + for_item809
                ref_set__Ref_5int32(sum__7, t916)
                continue
            }
        } else {
            break Loop_loop911
        }
    }
    var t876 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(sum__7)
    println__T_i32(t876)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_i32_c_string_q_()
    var t877 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_i32_c_string_q_(pairs__9, t877)
    var t878 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_i32_c_string_q_(pairs__9, t878)
    var for_limit818 int = vec_len__Vec_21Tuple2_5int32_6string(pairs__9)
    var for_index819 int = 0
    Loop_loop906:
    for {
        var t907 bool = for_index819 < for_limit818
        if t907 {
            var for_item820 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(pairs__9, for_index819)
            var t908 int = for_index819 + 1
            for_index819 = t908
            var x822 int32 = for_item820._0
            var x823 string = for_item820._1
            var t909 string
            var inline1090 string = __goml_builtin_int32_to_string(x822)
            t909 = inline1090
            var t910 string = t909 + x823
            var inline1087 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t910)
            _goml_runtime_core_string_println(inline1087)
            continue
        } else {
            break Loop_loop906
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    var t880 FnIterator__isize = counted_range(calls__12)
    var for_iter826 FnIterator__isize = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____isize_i_into__iter(t880)
    Loop_loop902:
    for {
        var for_next827 Option__isize
        var inline1096 func() Option__isize = for_iter826.next_fn
        var inline1097 Option__isize = inline1096()
        for_next827 = inline1097
        switch for_next827._tag {
        case 0:
            break Loop_loop902
        case 1:
            var x828 int = for_next827._v1_0
            var t904 int
            var inline1094 int = ref_get__Ref_3int(range_sum__13)
            t904 = inline1094
            var t905 int = t904 + x828
            ref_set__Ref_3int(range_sum__13, t905)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t882 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(calls__12)
    println__T_i32(t882)
    var t883 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(range_sum__13)
    println__T_isize(t883)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(0)
    var for_source833 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__i32(values__6, 1, 3)
    var for_limit834 int = len(for_source833)
    var for_index835 int = 0
    Loop_loop897:
    for {
        var t898 bool = for_index835 < for_limit834
        if t898 {
            var for_item836 int32 = for_source833[for_index835]
            var t899 int = for_index835 + 1
            for_index835 = t899
            var t900 int32
            var inline1101 int32 = ref_get__Ref_5int32(slice_sum__15)
            t900 = inline1101
            var t901 int32 = t900 + for_item836
            ref_set__Ref_5int32(slice_sum__15, t901)
            continue
        } else {
            break Loop_loop897
        }
    }
    var t885 int32
    var inline1135 int32 = ref_get__Ref_5int32(slice_sum__15)
    t885 = inline1135
    println__T_i32(t885)
    var t886 FnIterator__i32
    var inline1129 int32 = 4
    var inline1130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(inline1129)
    var inline1131 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: inline1130,
    }
    var inline1132 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(inline1131)
    }
    var inline1133 FnIterator__i32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__i32(inline1132)
    t886 = inline1133
    var for_iter841 FnIterator__i32
    for_iter841 = t886
    Loop_loop893:
    for {
        var for_next842 Option__i32
        var inline1106 func() Option__i32 = for_iter841.next_fn
        var inline1107 Option__i32 = inline1106()
        for_next842 = inline1107
        switch for_next842._tag {
        case 0:
            break Loop_loop893
        case 1:
            var x843 int32 = for_next842._v1_0
            var t896 bool = x843 == 2
            if t896 {
                break Loop_loop893
            } else {
                var inline1103 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x843)
                _goml_runtime_core_string_println(inline1103)
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var empty__18 FnIterator__isize
    var inline1124 int = 0
    var inline1125 int = 0
    var inline1126 FnIterator__isize = __goml_builtin_range(inline1124, inline1125)
    empty__18 = inline1126
    var for_iter847 FnIterator__isize
    for_iter847 = empty__18
    Loop_loop891:
    for {
        var for_next848 Option__isize
        var inline1109 func() Option__isize = for_iter847.next_fn
        var inline1110 Option__isize = inline1109()
        for_next848 = inline1110
        switch for_next848._tag {
        case 0:
            break Loop_loop891
        case 1:
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t889 FnIterator__isize
    var inline1119 int = 3
    var inline1120 int = 8
    var inline1121 FnIterator__isize = __goml_builtin_range(inline1119, inline1120)
    t889 = inline1121
    var t890 int = first_even(t889)
    var inline1116 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t890)
    _goml_runtime_core_string_println(inline1116)
    var inline1112 string = "done"
    var inline1113 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1112)
    _goml_runtime_core_string_println(inline1113)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(value__684 int32) *ref_int32_x {
    var t920 *ref_int32_x = ref__Ref_5int32(value__684)
    return t920
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(self__685 *ref_int32_x) int32 {
    var t923 int32 = ref_get__Ref_5int32(self__685)
    return t923
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__i32(next_fn__507 func() Option__i32) FnIterator__i32 {
    var t928 FnIterator__i32 = FnIterator__i32{
        next_fn: next_fn__507,
    }
    return t928
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____isize_i_into__iter(self__509 FnIterator__isize) FnIterator__isize {
    return self__509
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32() *_goml_vec_int32 {
    var t940 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t940
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(self__511 *_goml_vec_int32, elem__512 int32) struct{} {
    vec_push__Vec_5int32(self__511, elem__512)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t944 string
    var inline1142 string = __goml_builtin_int32_to_string(value__1)
    t944 = inline1142
    _goml_runtime_core_string_println(t944)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_i32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var t948 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    return t948
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_i32_c_string_q_(self__511 *_goml_vec_Tuple2_5int32_6string, elem__512 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__511, elem__512)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__684 int) *ref_int_x {
    var t959 *ref_int_x = ref__Ref_3int(value__684)
    return t959
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__685 *ref_int_x) int {
    var t962 int = ref_get__Ref_3int(self__685)
    return t962
}

func println__T_isize(value__1 int) struct{} {
    var t966 string
    var inline1148 string = __goml_builtin_int_to_string(value__1)
    t966 = inline1148
    _goml_runtime_core_string_println(t966)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__i32(self__564 *_goml_vec_int32, start__565 int, end__566 int) []int32 {
    var t970 []int32 = self__564.items[start__565:end__566]
    return t970
}

func __goml_builtin_range(start__756 int, end__757 int) FnIterator__isize {
    var current__758 *ref_int_x = ref__Ref_3int(start__756)
    var t979 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__758,
        end_1: end__757,
    }
    var t980 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t979)
    }
    var inline1150 FnIterator__isize = FnIterator__isize{
        next_fn: t980,
    }
    return inline1150
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline1152 int64 = int64(int32(self__407))
    var inline1153 string = signed_decimal_string(inline1152)
    return inline1153
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t989 int64 = int64(int32(value__225))
    var inline1155 bool = t989 < 0
    if inline1155 {
        var inline1156 uint64 = uint64(int64(t989))
        var inline1157 uint64 = 0 - inline1156
        var inline1158 string = decimal_string(inline1157)
        var inline1159 string = "-" + inline1158
        return inline1159
    } else {
        var inline1160 uint64 = uint64(int64(t989))
        var inline1161 string = decimal_string(inline1160)
        return inline1161
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1163 int64 = int64(int(self__404))
    var inline1164 string = signed_decimal_string(inline1163)
    return inline1164
}

func signed_decimal_string(value__214 int64) string {
    var t1001 bool = value__214 < 0
    if t1001 {
        var t1002 uint64 = uint64(int64(value__214))
        var t1003 uint64 = 0 - t1002
        var t1004 string = decimal_string(t1003)
        var t1005 string = "-" + t1004
        return t1005
    } else {
        var t1006 uint64 = uint64(int64(value__214))
        var t1007 string = decimal_string(t1006)
        return t1007
    }
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t1010 int64 = int64(int(value__222))
    var inline1166 bool = t1010 < 0
    if inline1166 {
        var inline1167 uint64 = uint64(int64(t1010))
        var inline1168 uint64 = 0 - inline1167
        var inline1169 string = decimal_string(inline1168)
        var inline1170 string = "-" + inline1169
        return inline1170
    } else {
        var inline1171 uint64 = uint64(int64(t1010))
        var inline1172 string = decimal_string(inline1171)
        return inline1172
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
        var inline1182 int = vec_len__Vec_5uint8(reversed__209)
        t1016 = inline1182
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1016)
        var offset__212 int = 0
        Loop_loop1018:
        for {
            var t1019 int
            var inline1180 int = vec_len__Vec_5uint8(reversed__209)
            t1019 = inline1180
            var t1020 bool = offset__212 < t1019
            if t1020 {
                var t1021 int
                var inline1178 int = vec_len__Vec_5uint8(reversed__209)
                t1021 = inline1178
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

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env853 closure_env_countdown_0) Option__i32 {
    var current__1 *ref_int32_x = env853.current_0
    var value__2 int32
    var inline1186 int32 = ref_get__Ref_5int32(current__1)
    value__2 = inline1186
    var t1056 bool = value__2 > 0
    if t1056 {
        var t1057 int32 = value__2 - 1
        ref_set__Ref_5int32(current__1, t1057)
        var t1058 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: value__2,
        }
        return t1058
    } else {
        return Option__i32{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env854 closure_env_goml_builtin_range_1) Option__isize {
    var current__758 *ref_int_x = env854.current_0
    var end__757 int = env854.end_1
    var value__759 int = ref_get__Ref_3int(current__758)
    var t1063 bool = value__759 < end__757
    if t1063 {
        var t1064 int = value__759 + 1
        ref_set__Ref_3int(current__758, t1064)
        var t1065 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__759,
        }
        return t1065
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
