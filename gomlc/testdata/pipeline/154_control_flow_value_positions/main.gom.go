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

type Ordering int32

func main0() struct{} {
    var i__0 *ref_int_x
    var inline939 int = 0
    var inline940 *ref_int_x = ref__Ref_3int(inline939)
    i__0 = inline940
    var sum__1 *ref_int_x
    var inline936 int = 0
    var inline937 *ref_int_x = ref__Ref_3int(inline936)
    sum__1 = inline937
    Loop_loop818:
    for {
        var t819 int
        var inline906 int = ref_get__Ref_3int(i__0)
        t819 = inline906
        var t820 bool = t819 < 5
        if t820 {
            var t821 int
            var inline904 int = ref_get__Ref_3int(i__0)
            t821 = inline904
            var t822 int = t821 + 1
            ref_set__Ref_3int(i__0, t822)
            var t827 int
            var inline900 int = ref_get__Ref_3int(i__0)
            t827 = inline900
            var t828 bool = t827 == 3
            var jp824 int
            if t828 {
                continue
            } else {
                var inline894 int = ref_get__Ref_3int(i__0)
                jp824 = inline894
                var t825 int
                var inline898 int = ref_get__Ref_3int(sum__1)
                t825 = inline898
                var t826 int = t825 + jp824
                ref_set__Ref_3int(sum__1, t826)
                continue
            }
        } else {
            break Loop_loop818
        }
    }
    var t807 int
    var inline934 int = ref_get__Ref_3int(sum__1)
    t807 = inline934
    var inline931 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t807)
    _goml_runtime_core_string_println(inline931)
    var j__3 *ref_int_x
    var inline928 int = 0
    var inline929 *ref_int_x = ref__Ref_3int(inline928)
    j__3 = inline929
    var total__4 *ref_int_x
    var inline925 int = 0
    var inline926 *ref_int_x = ref__Ref_3int(inline925)
    total__4 = inline926
    Loop_loop810:
    for {
        var t811 int
        var inline918 int = ref_get__Ref_3int(j__3)
        t811 = inline918
        var t812 int = t811 + 1
        ref_set__Ref_3int(j__3, t812)
        var mtmp801 int
        var inline914 int = ref_get__Ref_3int(j__3)
        mtmp801 = inline914
        var jp814 int
        switch mtmp801 {
        case 5:
            break Loop_loop810
        default:
            var inline908 int = ref_get__Ref_3int(j__3)
            jp814 = inline908
            var t815 int
            var inline912 int = ref_get__Ref_3int(total__4)
            t815 = inline912
            var t816 int = t815 + jp814
            ref_set__Ref_3int(total__4, t816)
            continue
        }
    }
    var t809 int
    var inline923 int = ref_get__Ref_3int(total__4)
    t809 = inline923
    var inline920 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t809)
    _goml_runtime_core_string_println(inline920)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline944 int64 = int64(int(self__404))
    var inline945 string = signed_decimal_string(inline944)
    return inline945
}

func signed_decimal_string(value__214 int64) string {
    var t852 bool = value__214 < 0
    if t852 {
        var t853 uint64 = uint64(int64(value__214))
        var t854 uint64 = 0 - t853
        var t855 string = decimal_string(t854)
        var t856 string = "-" + t855
        return t856
    } else {
        var t857 uint64 = uint64(int64(value__214))
        var t858 string = decimal_string(t857)
        return t858
    }
}

func decimal_string(value__208 uint64) string {
    var t881 bool = value__208 == 0
    if t881 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop874:
        for {
            var t875 bool = remaining__210 > 0
            if t875 {
                var t876_rhs uint64 = 10
                var t876 uint64 = remaining__210 % t876_rhs
                var t877 uint8 = uint8(uint64(t876))
                var t878 uint8 = t877 + 48
                vec_push__Vec_5uint8(reversed__209, t878)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t879 uint64 = compound_old353 / compound_value354
                remaining__210 = t879
                continue
            } else {
                break Loop_loop874
            }
        }
        var t863 int
        var inline963 int = vec_len__Vec_5uint8(reversed__209)
        t863 = inline963
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t863)
        var offset__212 int = 0
        Loop_loop865:
        for {
            var t866 int
            var inline961 int = vec_len__Vec_5uint8(reversed__209)
            t866 = inline961
            var t867 bool = offset__212 < t866
            if t867 {
                var t868 int
                var inline959 int = vec_len__Vec_5uint8(reversed__209)
                t868 = inline959
                var t869 int = t868 - offset__212
                var t870 int = t869 - 1
                var t871 uint8 = vec_get__Vec_5uint8(reversed__209, t870)
                vec_push__Vec_5uint8(bytes__211, t871)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t872 int = compound_old358 + compound_value359
                offset__212 = t872
                continue
            } else {
                break Loop_loop865
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
