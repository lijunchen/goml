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
    var total__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    Loop_loop825:
    for {
        var t836 int
        var inline926 int = ref_get__Ref_3int(i__0)
        t836 = inline926
        var t837 bool = t836 == 0
        var jp827 bool
        if t837 {
            var inline906 int = 1
            ref_set__Ref_3int(i__0, inline906)
            jp827 = true
        } else {
            var t840 int
            var inline909 int = ref_get__Ref_3int(i__0)
            t840 = inline909
            var t841 bool = t840 < 4
            if t841 {
                jp827 = true
            } else {
                jp827 = false
            }
        }
        if jp827 {
            var t828 int
            var inline924 int = ref_get__Ref_3int(total__1)
            t828 = inline924
            var t829 int
            var inline922 int = ref_get__Ref_3int(i__0)
            t829 = inline922
            var t830 int = t828 + t829
            ref_set__Ref_3int(total__1, t830)
            var t834 int
            var inline918 int = ref_get__Ref_3int(i__0)
            t834 = inline918
            var t835 bool = t834 == 1
            if t835 {
                var inline911 int = 2
                ref_set__Ref_3int(i__0, inline911)
                continue
            } else {
                var t832 int
                var inline916 int = ref_get__Ref_3int(i__0)
                t832 = inline916
                var t833 int = t832 + 1
                ref_set__Ref_3int(i__0, t833)
                continue
            }
        } else {
            break Loop_loop825
        }
    }
    var t813 int
    var inline958 int = ref_get__Ref_3int(total__1)
    t813 = inline958
    var inline955 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t813)
    _goml_runtime_core_string_println(inline955)
    var j__2 *ref_int_x
    var inline952 int = 0
    var inline953 *ref_int_x = ref__Ref_3int(inline952)
    j__2 = inline953
    var total2__3 *ref_int_x
    var inline949 int = 0
    var inline950 *ref_int_x = ref__Ref_3int(inline949)
    total2__3 = inline950
    Loop_loop816:
    for {
        var mtmp803 int
        var inline942 int = ref_get__Ref_3int(j__2)
        mtmp803 = inline942
        var jp818 bool
        switch mtmp803 {
        case 0:
            var inline928 int = 1
            ref_set__Ref_3int(j__2, inline928)
            jp818 = true
        case 1:
            var inline931 int = 2
            ref_set__Ref_3int(j__2, inline931)
            jp818 = true
        case 2:
            jp818 = true
        default:
            jp818 = false
        }
        if jp818 {
            var t819 int
            var inline940 int = ref_get__Ref_3int(total2__3)
            t819 = inline940
            var t820 int
            var inline938 int = ref_get__Ref_3int(j__2)
            t820 = inline938
            var t821 int = t819 + t820
            ref_set__Ref_3int(total2__3, t821)
            var t823 int
            var inline934 int = ref_get__Ref_3int(j__2)
            t823 = inline934
            var t824 bool = t823 == 2
            if t824 {
                break Loop_loop816
            } else {
                continue
            }
        } else {
            break Loop_loop816
        }
    }
    var t815 int
    var inline947 int = ref_get__Ref_3int(total2__3)
    t815 = inline947
    var inline944 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t815)
    _goml_runtime_core_string_println(inline944)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__684 int) *ref_int_x {
    var t844 *ref_int_x = ref__Ref_3int(value__684)
    return t844
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline962 int64 = int64(int(self__404))
    var inline963 string = signed_decimal_string(inline962)
    return inline963
}

func signed_decimal_string(value__214 int64) string {
    var t864 bool = value__214 < 0
    if t864 {
        var t865 uint64 = uint64(int64(value__214))
        var t866 uint64 = 0 - t865
        var t867 string = decimal_string(t866)
        var t868 string = "-" + t867
        return t868
    } else {
        var t869 uint64 = uint64(int64(value__214))
        var t870 string = decimal_string(t869)
        return t870
    }
}

func decimal_string(value__208 uint64) string {
    var t893 bool = value__208 == 0
    if t893 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop886:
        for {
            var t887 bool = remaining__210 > 0
            if t887 {
                var t888_rhs uint64 = 10
                var t888 uint64 = remaining__210 % t888_rhs
                var t889 uint8 = uint8(uint64(t888))
                var t890 uint8 = t889 + 48
                vec_push__Vec_5uint8(reversed__209, t890)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t891 uint64 = compound_old353 / compound_value354
                remaining__210 = t891
                continue
            } else {
                break Loop_loop886
            }
        }
        var t875 int
        var inline981 int = vec_len__Vec_5uint8(reversed__209)
        t875 = inline981
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t875)
        var offset__212 int = 0
        Loop_loop877:
        for {
            var t878 int
            var inline979 int = vec_len__Vec_5uint8(reversed__209)
            t878 = inline979
            var t879 bool = offset__212 < t878
            if t879 {
                var t880 int
                var inline977 int = vec_len__Vec_5uint8(reversed__209)
                t880 = inline977
                var t881 int = t880 - offset__212
                var t882 int = t881 - 1
                var t883 uint8 = vec_get__Vec_5uint8(reversed__209, t882)
                vec_push__Vec_5uint8(bytes__211, t883)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t884 int = compound_old358 + compound_value359
                offset__212 = t884
                continue
            } else {
                break Loop_loop877
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
