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
}

type FnIterator__i32 struct {
    next_fn func() Option__i32
}

type closure_env_inherent_Vec_Vec_T_iter_T_i32_0 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_int32
}

type Ordering int32

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func main0() struct{} {
    var values__3 *_goml_vec_int32
    var inline933 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline933
    var inline930 int32 = 10
    vec_push__Vec_5int32(values__3, inline930)
    var inline927 int32 = 20
    vec_push__Vec_5int32(values__3, inline927)
    var inline924 int32 = 30
    vec_push__Vec_5int32(values__3, inline924)
    var t810 Numbers = Numbers{
        values: values__3,
    }
    var t811 int32 = count__B_Numbers(t810)
    var inline921 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t811)
    _goml_runtime_core_string_println(inline921)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var total__2 *ref_int32_x
    var inline950 int32 = 0
    var inline951 *ref_int32_x = ref__Ref_5int32(inline950)
    total__2 = inline951
    var t822 *_goml_vec_int32
    var inline948 *_goml_vec_int32 = batch__1.values
    t822 = inline948
    var for_iter796 FnIterator__i32
    var inline946 FnIterator__i32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__i32(t822)
    for_iter796 = inline946
    Loop_loop825:
    for {
        var for_next797 Option__i32
        var inline941 func() Option__i32 = for_iter796.next_fn
        var inline942 Option__i32 = inline941()
        for_next797 = inline942
        switch for_next797._tag {
        case 0:
            break Loop_loop825
        case 1:
            var t827 int32
            var inline939 int32 = ref_get__Ref_5int32(total__2)
            t827 = inline939
            var t828 int32 = t827 + 1
            ref_set__Ref_5int32(total__2, t828)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline944 int32 = ref_get__Ref_5int32(total__2)
    return inline944
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline953 int64 = int64(int32(self__407))
    var inline954 string = signed_decimal_string(inline953)
    return inline954
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__i32(self__567 *_goml_vec_int32) FnIterator__i32 {
    var index__568 *ref_int_x = ref__Ref_3int(0)
    var len__569 int
    var inline972 int = vec_len__Vec_5int32(self__567)
    len__569 = inline972
    var t853 closure_env_inherent_Vec_Vec_T_iter_T_i32_0 = closure_env_inherent_Vec_Vec_T_iter_T_i32_0{
        index_0: index__568,
        len_1: len__569,
        self_2: self__567,
    }
    var t854 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__en_h1275f72f5de770912182f2a5cc7ddfae__i32__0_i_apply(t853)
    }
    var inline970 FnIterator__i32 = FnIterator__i32{
        next_fn: t854,
    }
    return inline970
}

func signed_decimal_string(value__214 int64) string {
    var t860 bool = value__214 < 0
    if t860 {
        var t861 uint64 = uint64(int64(value__214))
        var t862 uint64 = 0 - t861
        var t863 string = decimal_string(t862)
        var t864 string = "-" + t863
        return t864
    } else {
        var t865 uint64 = uint64(int64(value__214))
        var t866 string = decimal_string(t865)
        return t866
    }
}

func decimal_string(value__208 uint64) string {
    var t895 bool = value__208 == 0
    if t895 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop888:
        for {
            var t889 bool = remaining__210 > 0
            if t889 {
                var t890_rhs uint64 = 10
                var t890 uint64 = remaining__210 % t890_rhs
                var t891 uint8 = uint8(uint64(t890))
                var t892 uint8 = t891 + 48
                vec_push__Vec_5uint8(reversed__209, t892)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t893 uint64 = compound_old353 / compound_value354
                remaining__210 = t893
                continue
            } else {
                break Loop_loop888
            }
        }
        var t877 int
        var inline982 int = vec_len__Vec_5uint8(reversed__209)
        t877 = inline982
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t877)
        var offset__212 int = 0
        Loop_loop879:
        for {
            var t880 int
            var inline980 int = vec_len__Vec_5uint8(reversed__209)
            t880 = inline980
            var t881 bool = offset__212 < t880
            if t881 {
                var t882 int
                var inline978 int = vec_len__Vec_5uint8(reversed__209)
                t882 = inline978
                var t883 int = t882 - offset__212
                var t884 int = t883 - 1
                var t885 uint8 = vec_get__Vec_5uint8(reversed__209, t884)
                vec_push__Vec_5uint8(bytes__211, t885)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t886 int = compound_old358 + compound_value359
                offset__212 = t886
                continue
            } else {
                break Loop_loop879
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__en_h1275f72f5de770912182f2a5cc7ddfae__i32__0_i_apply(env805 closure_env_inherent_Vec_Vec_T_iter_T_i32_0) Option__i32 {
    var index__568 *ref_int_x = env805.index_0
    var len__569 int = env805.len_1
    var self__567 *_goml_vec_int32 = env805.self_2
    var current__570 int = ref_get__Ref_3int(index__568)
    var t917 bool = current__570 < len__569
    if t917 {
        var value__571 int32 = vec_get__Vec_5int32(self__567, current__570)
        var t918 int = current__570 + 1
        ref_set__Ref_3int(index__568, t918)
        var t919 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: value__571,
        }
        return t919
    } else {
        return Option__i32{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
