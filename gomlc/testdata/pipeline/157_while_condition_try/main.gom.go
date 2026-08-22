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

type Option__bool struct {
    _tag int32
    _v1_0 bool
}

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func run_some() Option__i32 {
    var i__2 *ref_int32_x
    var inline948 int32 = 0
    var inline949 *ref_int32_x = ref__Ref_5int32(inline948)
    i__2 = inline949
    var total__3 *ref_int32_x
    var inline945 int32 = 0
    var inline946 *ref_int32_x = ref__Ref_5int32(inline945)
    total__3 = inline946
    Loop_loop827:
    for {
        var t828 int32
        var inline941 int32 = ref_get__Ref_5int32(i__2)
        t828 = inline941
        var mtmp796 Option__bool
        var inline937 bool = t828 < 3
        if inline937 {
            var inline938 Option__bool = Option__bool{
                _tag: 1,
                _v1_0: true,
            }
            mtmp796 = inline938
        } else {
            var inline939 Option__bool = Option__bool{
                _tag: 1,
                _v1_0: false,
            }
            mtmp796 = inline939
        }
        var jp830 bool
        switch mtmp796._tag {
        case 0:
            return Option__i32{
                _tag: 0,
            }
        case 1:
            var x797 bool = mtmp796._v1_0
            jp830 = x797
            if jp830 {
                var t831 int32
                var inline935 int32 = ref_get__Ref_5int32(total__3)
                t831 = inline935
                var t832 int32
                var inline933 int32 = ref_get__Ref_5int32(i__2)
                t832 = inline933
                var t833 int32 = t831 + t832
                ref_set__Ref_5int32(total__3, t833)
                var t834 int32
                var inline929 int32 = ref_get__Ref_5int32(i__2)
                t834 = inline929
                var t835 int32 = t834 + 1
                ref_set__Ref_5int32(i__2, t835)
                continue
            } else {
                break Loop_loop827
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t825 int32
    var inline943 int32 = ref_get__Ref_5int32(total__3)
    t825 = inline943
    var t826 Option__i32 = Option__i32{
        _tag: 1,
        _v1_0: t825,
    }
    return t826
}

func run_none() Option__i32 {
    var i__4 *ref_int32_x
    var inline971 int32 = 0
    var inline972 *ref_int32_x = ref__Ref_5int32(inline971)
    i__4 = inline972
    var total__5 *ref_int32_x
    var inline968 int32 = 0
    var inline969 *ref_int32_x = ref__Ref_5int32(inline968)
    total__5 = inline969
    Loop_loop841:
    for {
        var t842 int32
        var inline964 int32 = ref_get__Ref_5int32(i__4)
        t842 = inline964
        var mtmp801 Option__bool
        var inline961 bool = t842 < 2
        if inline961 {
            var inline962 Option__bool = Option__bool{
                _tag: 1,
                _v1_0: true,
            }
            mtmp801 = inline962
        } else {
            mtmp801 = Option__bool{
                _tag: 0,
            }
        }
        var jp844 bool
        switch mtmp801._tag {
        case 0:
            return Option__i32{
                _tag: 0,
            }
        case 1:
            var x802 bool = mtmp801._v1_0
            jp844 = x802
            if jp844 {
                var t845 int32
                var inline959 int32 = ref_get__Ref_5int32(total__5)
                t845 = inline959
                var t846 int32
                var inline957 int32 = ref_get__Ref_5int32(i__4)
                t846 = inline957
                var t847 int32 = t845 + t846
                ref_set__Ref_5int32(total__5, t847)
                var t848 int32
                var inline953 int32 = ref_get__Ref_5int32(i__4)
                t848 = inline953
                var t849 int32 = t848 + 1
                ref_set__Ref_5int32(i__4, t849)
                continue
            } else {
                break Loop_loop841
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t839 int32
    var inline966 int32 = ref_get__Ref_5int32(total__5)
    t839 = inline966
    var t840 Option__i32 = Option__i32{
        _tag: 1,
        _v1_0: t839,
    }
    return t840
}

func main0() struct{} {
    var t857 Option__i32 = run_some()
    var t858 string
    switch t857._tag {
    case 0:
        t858 = "none"
    case 1:
        var inline987 int32 = t857._v1_0
        var inline989 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline987)
        var inline990 string = "some=" + inline989
        t858 = inline990
    default:
        panic("non-exhaustive match")
    }
    var inline984 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t858)
    _goml_runtime_core_string_println(inline984)
    var t859 Option__i32 = run_none()
    var t860 string
    switch t859._tag {
    case 0:
        t860 = "none"
    case 1:
        var inline979 int32 = t859._v1_0
        var inline981 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline979)
        var inline982 string = "some=" + inline981
        t860 = inline982
    default:
        panic("non-exhaustive match")
    }
    var inline976 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t860)
    _goml_runtime_core_string_println(inline976)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline992 int64 = int64(int32(self__286))
    var inline993 string = signed_decimal_string(inline992)
    return inline993
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t885 bool = value__214 < 0
    if t885 {
        var t886 uint64 = uint64(int64(value__214))
        var t887 uint64 = 0 - t886
        var t888 string = decimal_string(t887)
        var t889 string = "-" + t888
        return t889
    } else {
        var t890 uint64 = uint64(int64(value__214))
        var t891 string = decimal_string(t890)
        return t891
    }
}

func decimal_string(value__208 uint64) string {
    var t914 bool = value__208 == 0
    if t914 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop907:
        for {
            var t908 bool = remaining__210 > 0
            if t908 {
                var t909_rhs uint64 = 10
                var t909 uint64 = remaining__210 % t909_rhs
                var t910 uint8 = uint8(uint64(t909))
                var t911 uint8 = t910 + 48
                vec_push__Vec_5uint8(reversed__209, t911)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t912 uint64 = compound_old353 / compound_value354
                remaining__210 = t912
                continue
            } else {
                break Loop_loop907
            }
        }
        var t896 int
        var inline1012 int = vec_len__Vec_5uint8(reversed__209)
        t896 = inline1012
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t896)
        var offset__212 int = 0
        Loop_loop898:
        for {
            var t899 int
            var inline1010 int = vec_len__Vec_5uint8(reversed__209)
            t899 = inline1010
            var t900 bool = offset__212 < t899
            if t900 {
                var t901 int
                var inline1008 int = vec_len__Vec_5uint8(reversed__209)
                t901 = inline1008
                var t902 int = t901 - offset__212
                var t903 int = t902 - 1
                var t904 uint8 = vec_get__Vec_5uint8(reversed__209, t903)
                vec_push__Vec_5uint8(bytes__211, t904)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t905 int = compound_old358 + compound_value359
                offset__212 = t905
                continue
            } else {
                break Loop_loop898
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
