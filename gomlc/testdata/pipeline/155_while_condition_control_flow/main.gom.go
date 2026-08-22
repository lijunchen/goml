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
    var i__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    Loop_loop842:
    for {
        var t848 int
        var inline923 int = ref_get__Ref_3int(i__0)
        t848 = inline923
        var t849 bool = t848 < 3
        var jp844 bool
        if t849 {
            jp844 = true
        } else {
            jp844 = false
        }
        if jp844 {
            var t845 int
            var inline921 int = ref_get__Ref_3int(i__0)
            t845 = inline921
            var inline918 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t845)
            _goml_runtime_core_string_println(inline918)
            var t846 int
            var inline916 int = ref_get__Ref_3int(i__0)
            t846 = inline916
            var t847 int = t846 + 1
            ref_set__Ref_3int(i__0, t847)
            continue
        } else {
            break Loop_loop842
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    Loop_loop826:
    for {
        var t834 int
        var inline939 int = ref_get__Ref_3int(j__1)
        t834 = inline939
        var t835 bool = t834 < 4
        var jp828 bool
        if t835 {
            var t838 int
            var inline927 int = ref_get__Ref_3int(j__1)
            t838 = inline927
            var t839 bool = t838 == 1
            if t839 {
                jp828 = true
            } else {
                var t840 int
                var inline925 int = ref_get__Ref_3int(j__1)
                t840 = inline925
                var t841 bool = t840 != 3
                jp828 = t841
            }
        } else {
            jp828 = false
        }
        if jp828 {
            var t829 int
            var inline937 int = ref_get__Ref_3int(total__2)
            t829 = inline937
            var t830 int
            var inline935 int = ref_get__Ref_3int(j__1)
            t830 = inline935
            var t831 int = t829 + t830
            ref_set__Ref_3int(total__2, t831)
            var t832 int
            var inline931 int = ref_get__Ref_3int(j__1)
            t832 = inline931
            var t833 int = t832 + 1
            ref_set__Ref_3int(j__1, t833)
            continue
        } else {
            break Loop_loop826
        }
    }
    var t811 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(total__2)
    println__T_isize(t811)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    var sum__4 *ref_int_x
    var inline960 int = 0
    var inline961 *ref_int_x = ref__Ref_3int(inline960)
    sum__4 = inline961
    Loop_loop814:
    for {
        var mtmp803 int
        var inline953 int = ref_get__Ref_3int(k__3)
        mtmp803 = inline953
        var jp816 bool
        switch mtmp803 {
        case 0:
            jp816 = true
        case 1:
            var t824 int
            var inline941 int = ref_get__Ref_3int(sum__4)
            t824 = inline941
            var t825 bool = t824 == 0
            if t825 {
                jp816 = true
            } else {
                jp816 = false
            }
        case 2:
            jp816 = true
        default:
            jp816 = false
        }
        if jp816 {
            var t817 int
            var inline951 int = ref_get__Ref_3int(sum__4)
            t817 = inline951
            var t818 int
            var inline949 int = ref_get__Ref_3int(k__3)
            t818 = inline949
            var t819 int = t817 + t818
            ref_set__Ref_3int(sum__4, t819)
            var t820 int
            var inline945 int = ref_get__Ref_3int(k__3)
            t820 = inline945
            var t821 int = t820 + 1
            ref_set__Ref_3int(k__3, t821)
            continue
        } else {
            break Loop_loop814
        }
    }
    var t813 int
    var inline958 int = ref_get__Ref_3int(sum__4)
    t813 = inline958
    var inline955 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t813)
    _goml_runtime_core_string_println(inline955)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__684 int) *ref_int_x {
    var t852 *ref_int_x = ref__Ref_3int(value__684)
    return t852
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__685 *ref_int_x) int {
    var t855 int = ref_get__Ref_3int(self__685)
    return t855
}

func println__T_isize(value__1 int) struct{} {
    var t857 string
    var inline963 string = __goml_builtin_int_to_string(value__1)
    t857 = inline963
    _goml_runtime_core_string_println(t857)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline965 int64 = int64(int(self__404))
    var inline966 string = signed_decimal_string(inline965)
    return inline966
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t866 int64 = int64(int(value__222))
    var inline968 bool = t866 < 0
    if inline968 {
        var inline969 uint64 = uint64(int64(t866))
        var inline970 uint64 = 0 - inline969
        var inline971 string = decimal_string(inline970)
        var inline972 string = "-" + inline971
        return inline972
    } else {
        var inline973 uint64 = uint64(int64(t866))
        var inline974 string = decimal_string(inline973)
        return inline974
    }
}

func signed_decimal_string(value__214 int64) string {
    var t872 bool = value__214 < 0
    if t872 {
        var t873 uint64 = uint64(int64(value__214))
        var t874 uint64 = 0 - t873
        var t875 string = decimal_string(t874)
        var t876 string = "-" + t875
        return t876
    } else {
        var t877 uint64 = uint64(int64(value__214))
        var t878 string = decimal_string(t877)
        return t878
    }
}

func decimal_string(value__208 uint64) string {
    var t901 bool = value__208 == 0
    if t901 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop894:
        for {
            var t895 bool = remaining__210 > 0
            if t895 {
                var t896_rhs uint64 = 10
                var t896 uint64 = remaining__210 % t896_rhs
                var t897 uint8 = uint8(uint64(t896))
                var t898 uint8 = t897 + 48
                vec_push__Vec_5uint8(reversed__209, t898)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t899 uint64 = compound_old353 / compound_value354
                remaining__210 = t899
                continue
            } else {
                break Loop_loop894
            }
        }
        var t883 int
        var inline984 int = vec_len__Vec_5uint8(reversed__209)
        t883 = inline984
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t883)
        var offset__212 int = 0
        Loop_loop885:
        for {
            var t886 int
            var inline982 int = vec_len__Vec_5uint8(reversed__209)
            t886 = inline982
            var t887 bool = offset__212 < t886
            if t887 {
                var t888 int
                var inline980 int = vec_len__Vec_5uint8(reversed__209)
                t888 = inline980
                var t889 int = t888 - offset__212
                var t890 int = t889 - 1
                var t891 uint8 = vec_get__Vec_5uint8(reversed__209, t890)
                vec_push__Vec_5uint8(bytes__211, t891)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t892 int = compound_old358 + compound_value359
                offset__212 = t892
                continue
            } else {
                break Loop_loop885
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
