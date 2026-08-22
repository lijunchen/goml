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

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
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

type Tuple2_3int_3int struct {
    _0 int
    _1 int
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

type NumberSource struct {
    value int
}

type closure_env_increment_0 struct {
    captured_0 *ref_int_x
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type dyn__Source_vtable struct {
    get func(any) int
}

type dyn__Source struct {
    data any
    vtable *dyn__Source_vtable
}

func dyn__Source__wrap__NumberSource__get(self any) int {
    return _goml_m_trait__impl_i_Source_i_NumberSource_i_get(self.(NumberSource))
}

func dyn__Source__vtable__NumberSource() *dyn__Source_vtable {
    return &dyn__Source_vtable{
        get: dyn__Source__wrap__NumberSource__get,
    }
}

func _goml_m_trait__impl_i_Source_i_NumberSource_i_get(self__0 NumberSource) int {
    var t864 int = self__0.value
    return t864
}

func labeled_cleanup() struct{} {
    var inline1016 string = "inner cleanup"
    var inline1017 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1016)
    _goml_runtime_core_string_println(inline1017)
    var inline1012 string = "outer cleanup"
    var inline1013 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1012)
    _goml_runtime_core_string_println(inline1013)
    return struct{}{}
}

func main0() struct{} {
    var t875 NumberSource = NumberSource{
        value: 11,
    }
    var t876 dyn__Source = dyn__Source{
        data: t875,
        vtable: dyn__Source__vtable__NumberSource(),
    }
    var t877 int
    var inline1061 int = t876.vtable.get(t876.data)
    t877 = inline1061
    var inline1058 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t877)
    _goml_runtime_core_string_println(inline1058)
    var x802 int = 1
    var x803 int = 2
    var index__2 int = x802
    var compound_old804 int = index__2
    var t878 int = compound_old804 + x803
    index__2 = t878
    var inline1055 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(index__2)
    _goml_runtime_core_string_println(inline1055)
    var x809 int = 3
    var captured__4 *ref_int_x = ref__Ref_3int(x809)
    var t880 closure_env_increment_0 = closure_env_increment_0{
        captured_0: captured__4,
    }
    var increment__5 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(t880)
    }
    increment__5()
    var t881 int = ref_get__Ref_3int(captured__4)
    var inline1052 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t881)
    _goml_runtime_core_string_println(inline1052)
    var x817 int = 4
    var count__6 int = x817
    var compound_old818 int = count__6
    var compound_value819 int = 1
    var t932 int = compound_old818 + compound_value819
    count__6 = t932
    var inline1020 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(count__6)
    _goml_runtime_core_string_println(inline1020)
    var values__7 *_goml_vec_int
    var inline1050 *_goml_vec_int = vec_new__Vec_3int()
    values__7 = inline1050
    var inline1047 int = 6
    vec_push__Vec_3int(values__7, inline1047)
    var for_limit824 int = vec_len__Vec_3int(values__7)
    var for_index825 int = 0
    Loop_loop926:
    for {
        var t927 bool = for_index825 < for_limit824
        if t927 {
            var for_item826 int = vec_get__Vec_3int(values__7, for_index825)
            var t928 int = for_index825 + 1
            for_index825 = t928
            var item__8 int = for_item826
            var compound_old828 int = item__8
            var compound_value829 int = 1
            var t929 int = compound_old828 + compound_value829
            item__8 = t929
            var inline1023 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(item__8)
            _goml_runtime_core_string_println(inline1023)
            continue
        } else {
            break Loop_loop926
        }
    }
    var legacy__9 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 8,
        _1: 9,
    }
    var place_root832 Tuple2_3int_3int = legacy__9
    var place833 int = place_root832._0
    var value834 int = 1
    var t884 int = place833 + value834
    var t885 int = place_root832._1
    var t886 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t884,
        _1: t885,
    }
    legacy__9 = t886
    var place_root836 Tuple2_3int_3int = legacy__9
    var place837 int = place_root836._1
    var value838 int = 1
    var t888 int = place_root836._0
    var t889 int = place837 + value838
    var t890 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t888,
        _1: t889,
    }
    legacy__9 = t890
    var t892 int = legacy__9._0
    var t893 int = legacy__9._1
    var t894 int = t892 + t893
    var inline1044 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t894)
    _goml_runtime_core_string_println(inline1044)
    var steps__10 int = 0
    Loop_loop919:
    for {
        var t920 bool = steps__10 < 3
        if t920 {
            var compound_old841 int = steps__10
            var compound_value842 int = 1
            var t921 int = compound_old841 + compound_value842
            steps__10 = t921
            continue
        } else {
            break Loop_loop919
        }
    }
    var inline1041 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(steps__10)
    _goml_runtime_core_string_println(inline1041)
    var seen__11 *ref_int_x
    var inline1038 int = 0
    var inline1039 *ref_int_x = ref__Ref_3int(inline1038)
    seen__11 = inline1039
    var for_index847 int = 0
    var for_limit848 int = 3
    Loop_loop905:
    for {
        var t906 bool = for_index847 < for_limit848
        if t906 {
            var for_item849 int = for_index847
            var t907 int = for_index847 + 1
            for_index847 = t907
            var for_index851 int = 0
            var for_limit852 int = 3
            var t917 bool = for_item849 == 1
            Loop_loop909:
            for {
                var t910 bool = for_index851 < for_limit852
                if t910 {
                    var for_item853 int = for_index851
                    var t911 int = for_index851 + 1
                    for_index851 = t911
                    var t912 int
                    var inline1028 int = ref_get__Ref_3int(seen__11)
                    t912 = inline1028
                    var t913 int = t912 + 1
                    ref_set__Ref_3int(seen__11, t913)
                    var jp916 bool
                    if t917 {
                        var t918 bool = for_item853 == 1
                        jp916 = t918
                    } else {
                        jp916 = false
                    }
                    if jp916 {
                        var t897 int
                        var inline1036 int = ref_get__Ref_3int(seen__11)
                        t897 = inline1036
                        var inline1033 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t897)
                        _goml_runtime_core_string_println(inline1033)
                        var jp899 int
                        jp899 = 42
                        var inline1030 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp899)
                        _goml_runtime_core_string_println(inline1030)
                        labeled_cleanup()
                        return struct{}{}
                    } else {
                        continue
                    }
                } else {
                    break Loop_loop909
                }
            }
            continue
        } else {
            break Loop_loop905
        }
    }
    var t897 int
    var inline1036 int = ref_get__Ref_3int(seen__11)
    t897 = inline1036
    var inline1033 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t897)
    _goml_runtime_core_string_println(inline1033)
    var jp899 int
    jp899 = 42
    var inline1030 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp899)
    _goml_runtime_core_string_println(inline1030)
    labeled_cleanup()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1066 int64 = int64(int(self__404))
    var inline1067 string = signed_decimal_string(inline1066)
    return inline1067
}

func signed_decimal_string(value__214 int64) string {
    var t967 bool = value__214 < 0
    if t967 {
        var t968 uint64 = uint64(int64(value__214))
        var t969 uint64 = 0 - t968
        var t970 string = decimal_string(t969)
        var t971 string = "-" + t970
        return t971
    } else {
        var t972 uint64 = uint64(int64(value__214))
        var t973 string = decimal_string(t972)
        return t973
    }
}

func decimal_string(value__208 uint64) string {
    var t996 bool = value__208 == 0
    if t996 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop989:
        for {
            var t990 bool = remaining__210 > 0
            if t990 {
                var t991_rhs uint64 = 10
                var t991 uint64 = remaining__210 % t991_rhs
                var t992 uint8 = uint8(uint64(t991))
                var t993 uint8 = t992 + 48
                vec_push__Vec_5uint8(reversed__209, t993)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t994 uint64 = compound_old353 / compound_value354
                remaining__210 = t994
                continue
            } else {
                break Loop_loop989
            }
        }
        var t978 int
        var inline1085 int = vec_len__Vec_5uint8(reversed__209)
        t978 = inline1085
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t978)
        var offset__212 int = 0
        Loop_loop980:
        for {
            var t981 int
            var inline1083 int = vec_len__Vec_5uint8(reversed__209)
            t981 = inline1083
            var t982 bool = offset__212 < t981
            if t982 {
                var t983 int
                var inline1081 int = vec_len__Vec_5uint8(reversed__209)
                t983 = inline1081
                var t984 int = t983 - offset__212
                var t985 int = t984 - 1
                var t986 uint8 = vec_get__Vec_5uint8(reversed__209, t985)
                vec_push__Vec_5uint8(bytes__211, t986)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t987 int = compound_old358 + compound_value359
                offset__212 = t987
                continue
            } else {
                break Loop_loop980
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(env861 closure_env_increment_0) struct{} {
    var captured__4 *ref_int_x = env861.captured_0
    var compound_old811 int = ref_get__Ref_3int(captured__4)
    var compound_value812 int = 1
    var t1009 int = compound_old811 + compound_value812
    ref_set__Ref_3int(captured__4, t1009)
    return struct{}{}
}

func main() {
    main0()
}
