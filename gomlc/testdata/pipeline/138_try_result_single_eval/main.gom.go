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

type Result__i32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__i32__string {
    var mtmp797 Result__i32__string
    var inline912 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(counter__2)
    var inline913 int32 = inline912 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(counter__2, inline913)
    if ok__3 {
        var inline915 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(counter__2)
        var inline916 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: inline915,
        }
        mtmp797 = inline916
    } else {
        var inline917 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: "bump failed",
        }
        mtmp797 = inline917
    }
    var jp816 int32
    switch mtmp797._tag {
    case 0:
        var x798 int32 = mtmp797._v0_0
        jp816 = x798
        var t817 int32
        var inline910 int32 = ref_get__Ref_5int32(counter__2)
        t817 = inline910
        var t818 int32 = jp816 + t817
        var t819 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: t818,
        }
        return t819
    case 1:
        var x799 string = mtmp797._v1_0
        var t820 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: x799,
        }
        return t820
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__i32__string) string {
    switch res__5._tag {
    case 0:
        var x800 int32 = res__5._v0_0
        var t825 string
        var inline919 string = __goml_builtin_int32_to_string(x800)
        t825 = inline919
        var t826 string = "ok " + t825
        return t826
    case 1:
        var x801 string = res__5._v1_0
        var t827 string = "err " + x801
        return t827
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t836 string
    var inline964 bool = true
    var inline965 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(0)
    var inline966 Result__i32__string = use_try(inline965, inline964)
    var inline967 string = show(inline966)
    var inline968 string = inline967 + " count="
    var inline969 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline965)
    var inline970 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline969)
    var inline971 string = inline968 + inline970
    t836 = inline971
    var inline961 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t836)
    _goml_runtime_core_string_println(inline961)
    var t837 string
    var inline952 bool = false
    var inline953 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(0)
    var inline954 Result__i32__string = use_try(inline953, inline952)
    var inline955 string = show(inline954)
    var inline956 string = inline955 + " count="
    var inline957 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline953)
    var inline958 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline957)
    var inline959 string = inline956 + inline958
    t837 = inline959
    var inline949 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t837)
    _goml_runtime_core_string_println(inline949)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(self__685 *ref_int32_x) int32 {
    var t840 int32 = ref_get__Ref_5int32(self__685)
    return t840
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(self__686 *ref_int32_x, value__687 int32) struct{} {
    ref_set__Ref_5int32(self__686, value__687)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline973 int64 = int64(int32(self__286))
    var inline974 string = signed_decimal_string(inline973)
    return inline974
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(value__684 int32) *ref_int32_x {
    var t848 *ref_int32_x = ref__Ref_5int32(value__684)
    return t848
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t854 int64 = int64(int32(value__225))
    var inline977 bool = t854 < 0
    if inline977 {
        var inline978 uint64 = uint64(int64(t854))
        var inline979 uint64 = 0 - inline978
        var inline980 string = decimal_string(inline979)
        var inline981 string = "-" + inline980
        return inline981
    } else {
        var inline982 uint64 = uint64(int64(t854))
        var inline983 string = decimal_string(inline982)
        return inline983
    }
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
        var inline993 int = vec_len__Vec_5uint8(reversed__209)
        t873 = inline993
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t873)
        var offset__212 int = 0
        Loop_loop875:
        for {
            var t876 int
            var inline991 int = vec_len__Vec_5uint8(reversed__209)
            t876 = inline991
            var t877 bool = offset__212 < t876
            if t877 {
                var t878 int
                var inline989 int = vec_len__Vec_5uint8(reversed__209)
                t878 = inline989
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
