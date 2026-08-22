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

type Numbers struct {
    values *_goml_vec_int32
    conversions *ref_int32_x
}

type FnIterator__i32 struct {
    next_fn func() Option__i32
}

type closure_env_inherent_Vec_Vec_T_iter_T_i32_0 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_int32
}

type closure_env_inherent_Slice_Slice_T_iter_T_i32_1 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 []int32
}

type Ordering int32

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func main0() struct{} {
    var builds__7 *ref_int32_x
    var inline1056 int32 = 0
    var inline1057 *ref_int32_x = ref__Ref_5int32(inline1056)
    builds__7 = inline1057
    var conversions__8 *ref_int32_x
    var inline1053 int32 = 0
    var inline1054 *ref_int32_x = ref__Ref_5int32(inline1053)
    conversions__8 = inline1054
    var t830 Numbers
    var inline1044 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(builds__7)
    var inline1045 int32 = inline1044 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(builds__7, inline1045)
    var inline1047 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(inline1047, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(inline1047, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(inline1047, 3)
    var inline1051 Numbers = Numbers{
        values: inline1047,
        conversions: conversions__8,
    }
    t830 = inline1051
    var t831 int32 = sum__S_Numbers(t830)
    var inline1041 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t831)
    _goml_runtime_core_string_println(inline1041)
    var t832 int32
    var inline1039 int32 = ref_get__Ref_5int32(builds__7)
    t832 = inline1039
    var inline1036 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t832)
    _goml_runtime_core_string_println(inline1036)
    var t833 int32
    var inline1034 int32 = ref_get__Ref_5int32(conversions__8)
    t833 = inline1034
    var inline1031 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t833)
    _goml_runtime_core_string_println(inline1031)
    var values__9 *_goml_vec_int32
    var inline1029 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__9 = inline1029
    var inline1026 int32 = 10
    vec_push__Vec_5int32(values__9, inline1026)
    var inline1023 int32 = 20
    vec_push__Vec_5int32(values__9, inline1023)
    var inline1020 int32 = 30
    vec_push__Vec_5int32(values__9, inline1020)
    var t834 int32 = _goml_m_sum____S__Vec_l_i32_r_(values__9)
    var inline1017 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t834)
    _goml_runtime_core_string_println(inline1017)
    var t835 []int32
    var inline1013 int = 1
    var inline1014 int = 3
    var inline1015 []int32 = values__9.items[inline1013:inline1014]
    t835 = inline1015
    var t836 int32 = _goml_m_sum____S__Slice_l_i32_r_(t835)
    var inline1010 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t836)
    _goml_runtime_core_string_println(inline1010)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(self__685 *ref_int32_x) int32 {
    var t839 int32 = ref_get__Ref_5int32(self__685)
    return t839
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(self__686 *ref_int32_x, value__687 int32) struct{} {
    ref_set__Ref_5int32(self__686, value__687)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__i32(self__567 *_goml_vec_int32) FnIterator__i32 {
    var index__568 *ref_int_x = ref__Ref_3int(0)
    var len__569 int
    var inline1061 int = vec_len__Vec_5int32(self__567)
    len__569 = inline1061
    var t844 closure_env_inherent_Vec_Vec_T_iter_T_i32_0 = closure_env_inherent_Vec_Vec_T_iter_T_i32_0{
        index_0: index__568,
        len_1: len__569,
        self_2: self__567,
    }
    var t845 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__en_h1275f72f5de770912182f2a5cc7ddfae__i32__0_i_apply(t844)
    }
    var inline1059 FnIterator__i32 = FnIterator__i32{
        next_fn: t845,
    }
    return inline1059
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32() *_goml_vec_int32 {
    var t849 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t849
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(self__511 *_goml_vec_int32, elem__512 int32) struct{} {
    vec_push__Vec_5int32(self__511, elem__512)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var total__5 *ref_int32_x
    var inline1082 int32 = 0
    var inline1083 *ref_int32_x = ref__Ref_5int32(inline1082)
    total__5 = inline1083
    var for_iter801 FnIterator__i32
    var inline1074 *ref_int32_x = source__4.conversions
    var inline1075 *ref_int32_x = source__4.conversions
    var inline1076 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1075)
    var inline1077 int32 = inline1076 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline1074, inline1077)
    var inline1079 *_goml_vec_int32 = source__4.values
    var inline1080 FnIterator__i32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__i32(inline1079)
    for_iter801 = inline1080
    Loop_loop862:
    for {
        var for_next802 Option__i32
        var inline1069 func() Option__i32 = for_iter801.next_fn
        var inline1070 Option__i32 = inline1069()
        for_next802 = inline1070
        switch for_next802._tag {
        case 0:
            break Loop_loop862
        case 1:
            var x803 int32 = for_next802._v1_0
            var t864 int32
            var inline1067 int32 = ref_get__Ref_5int32(total__5)
            t864 = inline1067
            var t865 int32 = t864 + x803
            ref_set__Ref_5int32(total__5, t865)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline1072 int32 = ref_get__Ref_5int32(total__5)
    return inline1072
}

func _goml_m_sum____S__Vec_l_i32_r_(source__4 *_goml_vec_int32) int32 {
    var total__5 *ref_int32_x
    var inline1096 int32 = 0
    var inline1097 *ref_int32_x = ref__Ref_5int32(inline1096)
    total__5 = inline1097
    var for_iter801 FnIterator__i32
    var inline1094 FnIterator__i32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__i32(source__4)
    for_iter801 = inline1094
    Loop_loop870:
    for {
        var for_next802 Option__i32
        var inline1089 func() Option__i32 = for_iter801.next_fn
        var inline1090 Option__i32 = inline1089()
        for_next802 = inline1090
        switch for_next802._tag {
        case 0:
            break Loop_loop870
        case 1:
            var x803 int32 = for_next802._v1_0
            var t872 int32
            var inline1087 int32 = ref_get__Ref_5int32(total__5)
            t872 = inline1087
            var t873 int32 = t872 + x803
            ref_set__Ref_5int32(total__5, t873)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline1092 int32 = ref_get__Ref_5int32(total__5)
    return inline1092
}

func _goml_m_sum____S__Slice_l_i32_r_(source__4 []int32) int32 {
    var total__5 *ref_int32_x
    var inline1110 int32 = 0
    var inline1111 *ref_int32_x = ref__Ref_5int32(inline1110)
    total__5 = inline1111
    var for_iter801 FnIterator__i32
    var inline1108 FnIterator__i32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__i32(source__4)
    for_iter801 = inline1108
    Loop_loop878:
    for {
        var for_next802 Option__i32
        var inline1103 func() Option__i32 = for_iter801.next_fn
        var inline1104 Option__i32 = inline1103()
        for_next802 = inline1104
        switch for_next802._tag {
        case 0:
            break Loop_loop878
        case 1:
            var x803 int32 = for_next802._v1_0
            var t880 int32
            var inline1101 int32 = ref_get__Ref_5int32(total__5)
            t880 = inline1101
            var t881 int32 = t880 + x803
            ref_set__Ref_5int32(total__5, t881)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline1106 int32 = ref_get__Ref_5int32(total__5)
    return inline1106
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline1113 int64 = int64(int32(self__407))
    var inline1114 string = signed_decimal_string(inline1113)
    return inline1114
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__i32(self__667 []int32) FnIterator__i32 {
    var index__668 *ref_int_x = ref__Ref_3int(0)
    var len__669 int
    var inline1138 int = len(self__667)
    len__669 = inline1138
    var t910 closure_env_inherent_Slice_Slice_T_iter_T_i32_1 = closure_env_inherent_Slice_Slice_T_iter_T_i32_1{
        index_0: index__668,
        len_1: len__669,
        self_2: self__667,
    }
    var t911 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__en_h05f447217e0cf6cdcd746af967c79493__i32__1_i_apply(t910)
    }
    var inline1136 FnIterator__i32 = FnIterator__i32{
        next_fn: t911,
    }
    return inline1136
}

func signed_decimal_string(value__214 int64) string {
    var t917 bool = value__214 < 0
    if t917 {
        var t918 uint64 = uint64(int64(value__214))
        var t919 uint64 = 0 - t918
        var t920 string = decimal_string(t919)
        var t921 string = "-" + t920
        return t921
    } else {
        var t922 uint64 = uint64(int64(value__214))
        var t923 string = decimal_string(t922)
        return t923
    }
}

func decimal_string(value__208 uint64) string {
    var t952 bool = value__208 == 0
    if t952 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop945:
        for {
            var t946 bool = remaining__210 > 0
            if t946 {
                var t947_rhs uint64 = 10
                var t947 uint64 = remaining__210 % t947_rhs
                var t948 uint8 = uint8(uint64(t947))
                var t949 uint8 = t948 + 48
                vec_push__Vec_5uint8(reversed__209, t949)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t950 uint64 = compound_old353 / compound_value354
                remaining__210 = t950
                continue
            } else {
                break Loop_loop945
            }
        }
        var t934 int
        var inline1148 int = vec_len__Vec_5uint8(reversed__209)
        t934 = inline1148
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t934)
        var offset__212 int = 0
        Loop_loop936:
        for {
            var t937 int
            var inline1146 int = vec_len__Vec_5uint8(reversed__209)
            t937 = inline1146
            var t938 bool = offset__212 < t937
            if t938 {
                var t939 int
                var inline1144 int = vec_len__Vec_5uint8(reversed__209)
                t939 = inline1144
                var t940 int = t939 - offset__212
                var t941 int = t940 - 1
                var t942 uint8 = vec_get__Vec_5uint8(reversed__209, t941)
                vec_push__Vec_5uint8(bytes__211, t942)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t943 int = compound_old358 + compound_value359
                offset__212 = t943
                continue
            } else {
                break Loop_loop936
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__en_h1275f72f5de770912182f2a5cc7ddfae__i32__0_i_apply(env814 closure_env_inherent_Vec_Vec_T_iter_T_i32_0) Option__i32 {
    var index__568 *ref_int_x = env814.index_0
    var len__569 int = env814.len_1
    var self__567 *_goml_vec_int32 = env814.self_2
    var current__570 int = ref_get__Ref_3int(index__568)
    var t974 bool = current__570 < len__569
    if t974 {
        var value__571 int32 = vec_get__Vec_5int32(self__567, current__570)
        var t975 int = current__570 + 1
        ref_set__Ref_3int(index__568, t975)
        var t976 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: value__571,
        }
        return t976
    } else {
        return Option__i32{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h05f447217e0cf6cdcd746af967c79493__i32__1_i_apply(env815 closure_env_inherent_Slice_Slice_T_iter_T_i32_1) Option__i32 {
    var index__668 *ref_int_x = env815.index_0
    var len__669 int = env815.len_1
    var self__667 []int32 = env815.self_2
    var current__670 int = ref_get__Ref_3int(index__668)
    var t981 bool = current__670 < len__669
    if t981 {
        var value__671 int32
        var inline1150 int32 = self__667[current__670]
        value__671 = inline1150
        var t982 int = current__670 + 1
        ref_set__Ref_3int(index__668, t982)
        var t983 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: value__671,
        }
        return t983
    } else {
        return Option__i32{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
