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

type Target struct {}

type Convertible struct {}

type Number struct {
    value int32
}

type Selected struct {
    value int32
}

type Ordering int32

func _goml_m_trait__impl_i_Source_i__l_i32_r__x40_Number_i_get(self__6 Number) int32 {
    var t815 int32 = self__6.value
    return t815
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var t818 int32 = self__9.value
    return t818
}

func main0() struct{} {
    var t821 string
    var inline929 int32 = 3
    var inline930 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline929)
    var inline931 string = "number:" + inline930
    t821 = inline931
    var inline926 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
    _goml_runtime_core_string_println(inline926)
    var t823 string
    var inline923 string = "goml"
    var inline924 string = "text:" + inline923
    t823 = inline924
    var inline920 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t823)
    _goml_runtime_core_string_println(inline920)
    var text__12 string
    text__12 = "converted"
    var number__13 int32
    number__13 = 7
    var inline915 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__12)
    _goml_runtime_core_string_println(inline915)
    var inline912 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(number__13)
    _goml_runtime_core_string_println(inline912)
    var t826 Number = Number{
        value: 8,
    }
    var t827 int32
    var inline910 int32 = _goml_m_trait__impl_i_Source_i__l_i32_r__x40_Number_i_get(t826)
    t827 = inline910
    var inline907 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t827)
    _goml_runtime_core_string_println(inline907)
    var t828 Selected = Selected{
        value: 9,
    }
    var t829 int32
    var inline905 int32 = invoke__S_Selected__T_i32(t828)
    t829 = inline905
    var inline902 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t829)
    _goml_runtime_core_string_println(inline902)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline933 int64 = int64(int32(self__286))
    var inline934 string = signed_decimal_string(inline933)
    return inline934
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline951 int64 = int64(int32(self__407))
    var inline952 string = signed_decimal_string(inline951)
    return inline952
}

func invoke__S_Selected__T_i32(source__10 Selected) int32 {
    var inline954 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(source__10)
    return inline954
}

func signed_decimal_string(value__214 int64) string {
    var t861 bool = value__214 < 0
    if t861 {
        var t862 uint64 = uint64(int64(value__214))
        var t863 uint64 = 0 - t862
        var t864 string = decimal_string(t863)
        var t865 string = "-" + t864
        return t865
    } else {
        var t866 uint64 = uint64(int64(value__214))
        var t867 string = decimal_string(t866)
        return t867
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
        var inline966 int = vec_len__Vec_5uint8(reversed__209)
        t875 = inline966
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t875)
        var offset__212 int = 0
        Loop_loop877:
        for {
            var t878 int
            var inline964 int = vec_len__Vec_5uint8(reversed__209)
            t878 = inline964
            var t879 bool = offset__212 < t878
            if t879 {
                var t880 int
                var inline962 int = vec_len__Vec_5uint8(reversed__209)
                t880 = inline962
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
