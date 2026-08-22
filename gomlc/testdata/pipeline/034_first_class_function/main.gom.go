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

type closure_env_closure_apply_0 struct {}

type closure_env_global_invoker_1 struct {}

type closure_env_composer_closure_2 struct {}

type Ordering int32

func double(x__0 int32) int32 {
    var t805 int32 = x__0 * 2
    return t805
}

func increment(x__1 int32) int32 {
    var t808 int32 = x__1 + 1
    return t808
}

func main0() struct{} {
    var first__8 int32
    var inline908 int32 = 4
    var inline909 int32 = double(inline908)
    first__8 = inline909
    var composed__9 int32
    var inline905 int32 = increment(first__8)
    var inline906 int32 = double(inline905)
    composed__9 = inline906
    var t817 closure_env_closure_apply_0 = closure_env_closure_apply_0{}
    var closure_apply__11 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(t817, p0)
    }
    var closure_result__12 int32 = closure_apply__11(composed__9)
    var t818 closure_env_global_invoker_1 = closure_env_global_invoker_1{}
    var global_invoker__15 func(func(int32) int32, int32) int32 = func(p0 func(int32) int32, p1 int32) int32 {
        return _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(t818, p0, p1)
    }
    var invoked_with_global__16 int32 = global_invoker__15(double, 3)
    var t819 closure_env_composer_closure_2 = closure_env_composer_closure_2{}
    var composer_closure__18 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(t819, p0)
    }
    var composed_by_closure__19 int32 = composer_closure__18(5)
    var t820 string
    var inline903 string = __goml_builtin_int32_to_string(composed__9)
    t820 = inline903
    var inline900 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
    _goml_runtime_core_string_println(inline900)
    var t821 string
    var inline898 string = __goml_builtin_int32_to_string(closure_result__12)
    t821 = inline898
    var inline895 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
    _goml_runtime_core_string_println(inline895)
    var t822 string
    var inline893 string = __goml_builtin_int32_to_string(invoked_with_global__16)
    t822 = inline893
    var inline890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
    _goml_runtime_core_string_println(inline890)
    var t823 string
    var inline888 string = __goml_builtin_int32_to_string(composed_by_closure__19)
    t823 = inline888
    var inline885 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t823)
    _goml_runtime_core_string_println(inline885)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t834 int64 = int64(int32(value__225))
    var inline915 bool = t834 < 0
    if inline915 {
        var inline916 uint64 = uint64(int64(t834))
        var inline917 uint64 = 0 - inline916
        var inline918 string = decimal_string(inline917)
        var inline919 string = "-" + inline918
        return inline919
    } else {
        var inline920 uint64 = uint64(int64(t834))
        var inline921 string = decimal_string(inline920)
        return inline921
    }
}

func decimal_string(value__208 uint64) string {
    var t869 bool = value__208 == 0
    if t869 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop862:
        for {
            var t863 bool = remaining__210 > 0
            if t863 {
                var t864_rhs uint64 = 10
                var t864 uint64 = remaining__210 % t864_rhs
                var t865 uint8 = uint8(uint64(t864))
                var t866 uint8 = t865 + 48
                vec_push__Vec_5uint8(reversed__209, t866)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t867 uint64 = compound_old353 / compound_value354
                remaining__210 = t867
                continue
            } else {
                break Loop_loop862
            }
        }
        var t851 int
        var inline931 int = vec_len__Vec_5uint8(reversed__209)
        t851 = inline931
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t851)
        var offset__212 int = 0
        Loop_loop853:
        for {
            var t854 int
            var inline929 int = vec_len__Vec_5uint8(reversed__209)
            t854 = inline929
            var t855 bool = offset__212 < t854
            if t855 {
                var t856 int
                var inline927 int = vec_len__Vec_5uint8(reversed__209)
                t856 = inline927
                var t857 int = t856 - offset__212
                var t858 int = t857 - 1
                var t859 uint8 = vec_get__Vec_5uint8(reversed__209, t858)
                vec_push__Vec_5uint8(bytes__211, t859)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t860 int = compound_old358 + compound_value359
                offset__212 = t860
                continue
            } else {
                break Loop_loop853
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__en_haa892b00b0eb7ffc029d576cdd67daaf_pply__0_i_apply(env800 closure_env_closure_apply_0, value__10 int32) int32 {
    var inline933 int32 = increment(value__10)
    return inline933
}

func _goml_m_inherent_i_closure__en_h3286eba341bf64dee5b16b571aed9928_oker__1_i_apply(env801 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var inline935 int32 = func_to_call__13(value__14)
    return inline935
}

func _goml_m_inherent_i_closure__en_h55d634371047d8dd612a447303ccba2c_sure__2_i_apply(env802 closure_env_composer_closure_2, value__17 int32) int32 {
    var inline937 int32 = increment(value__17)
    var inline938 int32 = double(inline937)
    return inline938
}

func main() {
    main0()
}
