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

func array_get__Array_3_5uint8(arr [3]uint8, index int) uint8 {
    return arr[index]
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

type Ordering int32

func answer() int {
    var base__0 int = 40
    var t809 int = base__0 + 2
    return t809
}

func loop_answer() int {
    var jp813 int
    var base__1 int = 6
    var t815 int = base__1 * 7
    jp813 = t815
    return jp813
}

func main0() struct{} {
    var plain__2 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{65, 10, 66},
    }
    var empty__3 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{},
    }
    var raw__4 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{114, 97, 119, 32, 92, 110, 32, 98, 121, 116, 101, 115},
    }
    var quoted__5 *_goml_vec_uint8 = &_goml_vec_uint8{
        items: []uint8{113, 117, 111, 116, 101, 100, 32, 34, 116, 101, 120, 116, 34, 32, 97, 110, 100, 32, 35},
    }
    var fixed__6 [3]uint8 = [3]uint8{65, 66, 67}
    var value__7 int = answer()
    var t817 int = loop_answer()
    var t818 int = value__7 + t817
    var t819 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(plain__2)
    var t820 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t819)
    println__T_string(t820)
    var t821 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(empty__3)
    var t822 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t821)
    println__T_string(t822)
    var t823 uint8
    var inline946 int = 0
    var inline947 uint8 = vec_get__Vec_5uint8(plain__2, inline946)
    t823 = inline947
    var t824 int = int(uint8(t823))
    var t825 string
    var inline944 string = __goml_builtin_int_to_string(t824)
    t825 = inline944
    var inline941 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t825)
    _goml_runtime_core_string_println(inline941)
    var t826 uint8
    var inline938 int = 1
    var inline939 uint8 = vec_get__Vec_5uint8(plain__2, inline938)
    t826 = inline939
    var t827 int = int(uint8(t826))
    var t828 string
    var inline936 string = __goml_builtin_int_to_string(t827)
    t828 = inline936
    var inline933 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t828)
    _goml_runtime_core_string_println(inline933)
    var t829 uint8
    var inline930 int = 2
    var inline931 uint8 = vec_get__Vec_5uint8(plain__2, inline930)
    t829 = inline931
    var t830 int = int(uint8(t829))
    var t831 string
    var inline928 string = __goml_builtin_int_to_string(t830)
    t831 = inline928
    var inline925 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t831)
    _goml_runtime_core_string_println(inline925)
    var t832 int
    var inline923 int = vec_len__Vec_5uint8(raw__4)
    t832 = inline923
    var t833 string
    var inline921 string = __goml_builtin_int_to_string(t832)
    t833 = inline921
    var inline918 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t833)
    _goml_runtime_core_string_println(inline918)
    var t834 int
    var inline916 int = vec_len__Vec_5uint8(quoted__5)
    t834 = inline916
    var t835 string
    var inline914 string = __goml_builtin_int_to_string(t834)
    t835 = inline914
    var inline911 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t835)
    _goml_runtime_core_string_println(inline911)
    var t836 uint8 = array_get__Array_3_5uint8(fixed__6, 2)
    var t837 int = int(uint8(t836))
    var t838 string
    var inline909 string = __goml_builtin_int_to_string(t837)
    t838 = inline909
    var inline906 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t838)
    _goml_runtime_core_string_println(inline906)
    var t839 string
    var inline904 string = __goml_builtin_int_to_string(t818)
    t839 = inline904
    var inline901 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t839)
    _goml_runtime_core_string_println(inline901)
    var inline897 string = "block condition"
    var inline898 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline897)
    _goml_runtime_core_string_println(inline898)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t843 string
    t843 = value__1
    _goml_runtime_core_string_println(t843)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(self__526 *_goml_vec_uint8) int {
    var t847 int = vec_len__Vec_5uint8(self__526)
    return t847
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__285 int) string {
    var inline950 int64 = int64(int(self__285))
    var inline951 string = signed_decimal_string(inline950)
    return inline951
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t858 int64 = int64(int(value__222))
    var inline953 bool = t858 < 0
    if inline953 {
        var inline954 uint64 = uint64(int64(t858))
        var inline955 uint64 = 0 - inline954
        var inline956 string = decimal_string(inline955)
        var inline957 string = "-" + inline956
        return inline957
    } else {
        var inline958 uint64 = uint64(int64(t858))
        var inline959 string = decimal_string(inline958)
        return inline959
    }
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
        var inline969 int = vec_len__Vec_5uint8(reversed__209)
        t875 = inline969
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t875)
        var offset__212 int = 0
        Loop_loop877:
        for {
            var t878 int
            var inline967 int = vec_len__Vec_5uint8(reversed__209)
            t878 = inline967
            var t879 bool = offset__212 < t878
            if t879 {
                var t880 int
                var inline965 int = vec_len__Vec_5uint8(reversed__209)
                t880 = inline965
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
