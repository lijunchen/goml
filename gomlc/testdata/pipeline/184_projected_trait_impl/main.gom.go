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

type Value struct {
    value int32
}

type closure_env_identity_0 struct {}

type Ordering int32

func _goml_m_trait__impl_i_Source_i_Value_i_get(self__1 Value) int32 {
    var t801 int32 = self__1.value
    return t801
}

func main0() struct{} {
    var t803 Value = Value{
        value: 41,
    }
    var direct__6 int32
    var inline876 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(t803)
    direct__6 = inline876
    var inline873 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(direct__6)
    _goml_runtime_core_string_println(inline873)
    var t804 Value = Value{
        value: 42,
    }
    var t805 int32
    var inline868 int32 = _goml_m_trait__impl_i_Pick_i__l_i32_r__x40_Value_i_pick(t804)
    var inline869 closure_env_identity_0 = closure_env_identity_0{}
    var inline870 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(inline869, p0)
    }
    var inline871 int32 = inline870(inline868)
    t805 = inline871
    var inline865 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t805)
    _goml_runtime_core_string_println(inline865)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_i32_r__x40_Value_i_pick(self__0 Value) int32 {
    var inline878 int32 = self__0.value
    return inline878
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline884 int64 = int64(int32(self__407))
    var inline885 string = signed_decimal_string(inline884)
    return inline885
}

func signed_decimal_string(value__214 int64) string {
    var t827 bool = value__214 < 0
    if t827 {
        var t828 uint64 = uint64(int64(value__214))
        var t829 uint64 = 0 - t828
        var t830 string = decimal_string(t829)
        var t831 string = "-" + t830
        return t831
    } else {
        var t832 uint64 = uint64(int64(value__214))
        var t833 string = decimal_string(t832)
        return t833
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
        var inline903 int = vec_len__Vec_5uint8(reversed__209)
        t838 = inline903
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t838)
        var offset__212 int = 0
        Loop_loop840:
        for {
            var t841 int
            var inline901 int = vec_len__Vec_5uint8(reversed__209)
            t841 = inline901
            var t842 bool = offset__212 < t841
            if t842 {
                var t843 int
                var inline899 int = vec_len__Vec_5uint8(reversed__209)
                t843 = inline899
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

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env798 closure_env_identity_0, item__4 int32) int32 {
    return item__4
}

func main() {
    main0()
}
