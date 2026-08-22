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

type Box__isize struct {
    value int
}

type Box__string struct {
    value string
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type Ordering int32

func main0() struct{} {
    var t805 closure_env_main_0 = closure_env_main_0{}
    var t806 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t805, p0)
    }
    var text__6 Box__string
    var inline912 int = 42
    var inline913 string = t806(inline912)
    var inline914 Box__string = Box__string{
        value: inline913,
    }
    text__6 = inline914
    var t807 string = text__6.value
    var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t807)
    _goml_runtime_core_string_println(inline909)
    var t808 closure_env_main_1 = closure_env_main_1{}
    var t809 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t808, p0)
    }
    var explicit__9 Box__string
    var inline905 int = 7
    var inline906 string = t809(inline905)
    var inline907 Box__string = Box__string{
        value: inline906,
    }
    explicit__9 = inline907
    var t810 string = explicit__9.value
    var inline902 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline902)
    var t811 closure_env_main_2 = closure_env_main_2{}
    var t812 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t811, p0)
    }
    var static_call__12 Box__string
    var inline898 int = 9
    var inline899 string = t812(inline898)
    var inline900 Box__string = Box__string{
        value: inline899,
    }
    static_call__12 = inline900
    var t813 string = static_call__12.value
    var inline895 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t813)
    _goml_runtime_core_string_println(inline895)
    var rendered__13 string
    var inline891 int = 5
    var inline892 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline891)
    var inline893 string = "value:" + inline892
    rendered__13 = inline893
    var inline888 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(rendered__13)
    _goml_runtime_core_string_println(inline888)
    return struct{}{}
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t832 int64 = int64(int(value__222))
    var inline922 bool = t832 < 0
    if inline922 {
        var inline923 uint64 = uint64(int64(t832))
        var inline924 uint64 = 0 - inline923
        var inline925 string = decimal_string(inline924)
        var inline926 string = "-" + inline925
        return inline926
    } else {
        var inline927 uint64 = uint64(int64(t832))
        var inline928 string = decimal_string(inline927)
        return inline928
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline930 int64 = int64(int(self__404))
    var inline931 string = signed_decimal_string(inline930)
    return inline931
}

func signed_decimal_string(value__214 int64) string {
    var t843 bool = value__214 < 0
    if t843 {
        var t844 uint64 = uint64(int64(value__214))
        var t845 uint64 = 0 - t844
        var t846 string = decimal_string(t845)
        var t847 string = "-" + t846
        return t847
    } else {
        var t848 uint64 = uint64(int64(value__214))
        var t849 string = decimal_string(t848)
        return t849
    }
}

func decimal_string(value__208 uint64) string {
    var t872 bool = value__208 == 0
    if t872 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop865:
        for {
            var t866 bool = remaining__210 > 0
            if t866 {
                var t867_rhs uint64 = 10
                var t867 uint64 = remaining__210 % t867_rhs
                var t868 uint8 = uint8(uint64(t867))
                var t869 uint8 = t868 + 48
                vec_push__Vec_5uint8(reversed__209, t869)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t870 uint64 = compound_old353 / compound_value354
                remaining__210 = t870
                continue
            } else {
                break Loop_loop865
            }
        }
        var t854 int
        var inline941 int = vec_len__Vec_5uint8(reversed__209)
        t854 = inline941
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t854)
        var offset__212 int = 0
        Loop_loop856:
        for {
            var t857 int
            var inline939 int = vec_len__Vec_5uint8(reversed__209)
            t857 = inline939
            var t858 bool = offset__212 < t857
            if t858 {
                var t859 int
                var inline937 int = vec_len__Vec_5uint8(reversed__209)
                t859 = inline937
                var t860 int = t859 - offset__212
                var t861 int = t860 - 1
                var t862 uint8 = vec_get__Vec_5uint8(reversed__209, t861)
                vec_push__Vec_5uint8(bytes__211, t862)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t863 int = compound_old358 + compound_value359
                offset__212 = t863
                continue
            } else {
                break Loop_loop856
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env801 closure_env_main_0, value__5 int) string {
    var inline943 string = __goml_builtin_int_to_string(value__5)
    return inline943
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env802 closure_env_main_1, value__8 int) string {
    var inline945 string = __goml_builtin_int_to_string(value__8)
    return inline945
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env803 closure_env_main_2, value__11 int) string {
    var inline947 string = __goml_builtin_int_to_string(value__11)
    return inline947
}

func main() {
    main0()
}
