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

type closure_env_make_pairer_T_isize_0 struct {
    x_0 int
}

type closure_env_make_pairer_T_string_1 struct {
    x_0 string
}

type Ordering int32

func main0() struct{} {
    var int_pairer__2 func(string) string
    var inline883 int = 7
    var inline884 closure_env_make_pairer_T_isize_0 = closure_env_make_pairer_T_isize_0{
        x_0: inline883,
    }
    var inline885 func(string) string = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_he5793d1ba8a1f5ce312b0e3782c192b7_size__0_i_apply(inline884, p0)
    }
    int_pairer__2 = inline885
    var string_pairer__3 func(string) string
    var inline879 string = "ok"
    var inline880 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: inline879,
    }
    var inline881 func(string) string = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(inline880, p0)
    }
    string_pairer__3 = inline881
    var t801 string = int_pairer__2("a")
    var inline876 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t801)
    _goml_runtime_core_string_println(inline876)
    var t802 string = string_pairer__3("b")
    var inline873 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t802)
    _goml_runtime_core_string_println(inline873)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t821 int64 = int64(int(value__222))
    var inline891 bool = t821 < 0
    if inline891 {
        var inline892 uint64 = uint64(int64(t821))
        var inline893 uint64 = 0 - inline892
        var inline894 string = decimal_string(inline893)
        var inline895 string = "-" + inline894
        return inline895
    } else {
        var inline896 uint64 = uint64(int64(t821))
        var inline897 string = decimal_string(inline896)
        return inline897
    }
}

func decimal_string(value__208 uint64) string {
    var t856 bool = value__208 == 0
    if t856 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop849:
        for {
            var t850 bool = remaining__210 > 0
            if t850 {
                var t851_rhs uint64 = 10
                var t851 uint64 = remaining__210 % t851_rhs
                var t852 uint8 = uint8(uint64(t851))
                var t853 uint8 = t852 + 48
                vec_push__Vec_5uint8(reversed__209, t853)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t854 uint64 = compound_old353 / compound_value354
                remaining__210 = t854
                continue
            } else {
                break Loop_loop849
            }
        }
        var t838 int
        var inline907 int = vec_len__Vec_5uint8(reversed__209)
        t838 = inline907
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t838)
        var offset__212 int = 0
        Loop_loop840:
        for {
            var t841 int
            var inline905 int = vec_len__Vec_5uint8(reversed__209)
            t841 = inline905
            var t842 bool = offset__212 < t841
            if t842 {
                var t843 int
                var inline903 int = vec_len__Vec_5uint8(reversed__209)
                t843 = inline903
                var t844 int = t843 - offset__212
                var t845 int = t844 - 1
                var t846 uint8 = vec_get__Vec_5uint8(reversed__209, t845)
                vec_push__Vec_5uint8(bytes__211, t846)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t847 int = compound_old358 + compound_value359
                offset__212 = t847
                continue
            } else {
                break Loop_loop840
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__en_he5793d1ba8a1f5ce312b0e3782c192b7_size__0_i_apply(env798 closure_env_make_pairer_T_isize_0, tag__1 string) string {
    var x__0 int = env798.x_0
    var t864 string = tag__1 + ":"
    var t865 string
    var inline909 string = __goml_builtin_int_to_string(x__0)
    t865 = inline909
    var t866 string = t864 + t865
    return t866
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env799 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var x__0 string = env799.x_0
    var t869 string = tag__1 + ":"
    var t870 string
    t870 = x__0
    var t871 string = t869 + t870
    return t871
}

func main() {
    main0()
}
