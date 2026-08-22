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

type Ordering int32

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func accumulate(limit__1 int32) Option__i32 {
    var sum__2 *ref_int32_x
    var inline923 int32 = 0
    var inline924 *ref_int32_x = ref__Ref_5int32(inline923)
    sum__2 = inline924
    var i__3 *ref_int32_x
    var inline920 int32 = 0
    var inline921 *ref_int32_x = ref__Ref_5int32(inline920)
    i__3 = inline921
    Loop_loop817:
    for {
        var t818 int32
        var inline916 int32 = ref_get__Ref_5int32(i__3)
        t818 = inline916
        var t819 bool = t818 < limit__1
        if t819 {
            var cur__4 int32
            var inline914 int32 = ref_get__Ref_5int32(i__3)
            cur__4 = inline914
            var t820 int32 = cur__4 + 1
            ref_set__Ref_5int32(i__3, t820)
            var t826 bool = cur__4 == 1
            if t826 {
                continue
            } else {
                var mtmp798 Option__i32
                var inline908 bool = cur__4 == 2
                if inline908 {
                    mtmp798 = Option__i32{
                        _tag: 0,
                    }
                } else {
                    var inline909 int32 = cur__4 + 10
                    var inline910 Option__i32 = Option__i32{
                        _tag: 1,
                        _v1_0: inline909,
                    }
                    mtmp798 = inline910
                }
                var jp823 int32
                switch mtmp798._tag {
                case 0:
                    return Option__i32{
                        _tag: 0,
                    }
                case 1:
                    var x799 int32 = mtmp798._v1_0
                    jp823 = x799
                    var t824 int32
                    var inline906 int32 = ref_get__Ref_5int32(sum__2)
                    t824 = inline906
                    var t825 int32 = t824 + jp823
                    ref_set__Ref_5int32(sum__2, t825)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop817
        }
    }
    var t815 int32
    var inline918 int32 = ref_get__Ref_5int32(sum__2)
    t815 = inline918
    var t816 Option__i32 = Option__i32{
        _tag: 1,
        _v1_0: t815,
    }
    return t816
}

func main0() struct{} {
    var t834 Option__i32 = accumulate(2)
    var t835 string
    switch t834._tag {
    case 0:
        t835 = "none"
    case 1:
        var inline939 int32 = t834._v1_0
        var inline941 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline939)
        var inline942 string = "some=" + inline941
        t835 = inline942
    default:
        panic("non-exhaustive match")
    }
    var inline936 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t835)
    _goml_runtime_core_string_println(inline936)
    var t836 Option__i32 = accumulate(4)
    var t837 string
    switch t836._tag {
    case 0:
        t837 = "none"
    case 1:
        var inline931 int32 = t836._v1_0
        var inline933 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline931)
        var inline934 string = "some=" + inline933
        t837 = inline934
    default:
        panic("non-exhaustive match")
    }
    var inline928 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t837)
    _goml_runtime_core_string_println(inline928)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline944 int64 = int64(int32(self__286))
    var inline945 string = signed_decimal_string(inline944)
    return inline945
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t862 bool = value__214 < 0
    if t862 {
        var t863 uint64 = uint64(int64(value__214))
        var t864 uint64 = 0 - t863
        var t865 string = decimal_string(t864)
        var t866 string = "-" + t865
        return t866
    } else {
        var t867 uint64 = uint64(int64(value__214))
        var t868 string = decimal_string(t867)
        return t868
    }
}

func decimal_string(value__208 uint64) string {
    var t891 bool = value__208 == 0
    if t891 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop884:
        for {
            var t885 bool = remaining__210 > 0
            if t885 {
                var t886_rhs uint64 = 10
                var t886 uint64 = remaining__210 % t886_rhs
                var t887 uint8 = uint8(uint64(t886))
                var t888 uint8 = t887 + 48
                vec_push__Vec_5uint8(reversed__209, t888)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t889 uint64 = compound_old353 / compound_value354
                remaining__210 = t889
                continue
            } else {
                break Loop_loop884
            }
        }
        var t873 int
        var inline964 int = vec_len__Vec_5uint8(reversed__209)
        t873 = inline964
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t873)
        var offset__212 int = 0
        Loop_loop875:
        for {
            var t876 int
            var inline962 int = vec_len__Vec_5uint8(reversed__209)
            t876 = inline962
            var t877 bool = offset__212 < t876
            if t877 {
                var t878 int
                var inline960 int = vec_len__Vec_5uint8(reversed__209)
                t878 = inline960
                var t879 int = t878 - offset__212
                var t880 int = t879 - 1
                var t881 uint8 = vec_get__Vec_5uint8(reversed__209, t880)
                vec_push__Vec_5uint8(bytes__211, t881)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t882 int = compound_old358 + compound_value359
                offset__212 = t882
                continue
            } else {
                break Loop_loop875
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
