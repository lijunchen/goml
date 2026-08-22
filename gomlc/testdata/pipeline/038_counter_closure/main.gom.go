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

type Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit struct {
    _0 func() int32
    _1 func() struct{}
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

type closure_env_next_0 struct {
    cell_0 *ref_int32_x
}

type closure_env_reset_1 struct {
    cell_0 *ref_int32_x
}

type Ordering int32

func main0() struct{} {
    var counter__4 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var inline921 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(0)
    var inline922 closure_env_next_0 = closure_env_next_0{
        cell_0: inline921,
    }
    var inline923 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline922)
    }
    var inline924 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline921,
    }
    var inline925 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline924)
    }
    var inline926 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline923,
        _1: inline925,
    }
    counter__4 = inline926
    var x799 func() int32 = counter__4._0
    var x800 func() struct{} = counter__4._1
    var first__7 int32 = x799()
    var second__8 int32 = x799()
    x800()
    var third__9 int32 = x799()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var inline914 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(0)
    var inline915 closure_env_next_0 = closure_env_next_0{
        cell_0: inline914,
    }
    var inline916 func() int32 = func() int32 {
        return _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(inline915)
    }
    var inline917 closure_env_reset_1 = closure_env_reset_1{
        cell_0: inline914,
    }
    var inline918 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(inline917)
    }
    var inline919 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: inline916,
        _1: inline918,
    }
    new_counter__10 = inline919
    var x803 func() int32 = new_counter__10._0
    var fourth__12 int32 = x803()
    var t817 string
    var inline912 string = __goml_builtin_int32_to_string(first__7)
    t817 = inline912
    var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t817)
    _goml_runtime_core_string_println(inline909)
    var t818 string
    var inline907 string = __goml_builtin_int32_to_string(second__8)
    t818 = inline907
    var inline904 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t818)
    _goml_runtime_core_string_println(inline904)
    var t819 string
    var inline902 string = __goml_builtin_int32_to_string(third__9)
    t819 = inline902
    var inline899 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t819)
    _goml_runtime_core_string_println(inline899)
    var t820 string
    var inline897 string = __goml_builtin_int32_to_string(fourth__12)
    t820 = inline897
    var inline894 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
    _goml_runtime_core_string_println(inline894)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(value__684 int32) *ref_int32_x {
    var t823 *ref_int32_x = ref__Ref_5int32(value__684)
    return t823
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t839 int64 = int64(int32(value__225))
    var inline932 bool = t839 < 0
    if inline932 {
        var inline933 uint64 = uint64(int64(t839))
        var inline934 uint64 = 0 - inline933
        var inline935 string = decimal_string(inline934)
        var inline936 string = "-" + inline935
        return inline936
    } else {
        var inline937 uint64 = uint64(int64(t839))
        var inline938 string = decimal_string(inline937)
        return inline938
    }
}

func decimal_string(value__208 uint64) string {
    var t874 bool = value__208 == 0
    if t874 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop867:
        for {
            var t868 bool = remaining__210 > 0
            if t868 {
                var t869_rhs uint64 = 10
                var t869 uint64 = remaining__210 % t869_rhs
                var t870 uint8 = uint8(uint64(t869))
                var t871 uint8 = t870 + 48
                vec_push__Vec_5uint8(reversed__209, t871)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t872 uint64 = compound_old353 / compound_value354
                remaining__210 = t872
                continue
            } else {
                break Loop_loop867
            }
        }
        var t856 int
        var inline948 int = vec_len__Vec_5uint8(reversed__209)
        t856 = inline948
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t856)
        var offset__212 int = 0
        Loop_loop858:
        for {
            var t859 int
            var inline946 int = vec_len__Vec_5uint8(reversed__209)
            t859 = inline946
            var t860 bool = offset__212 < t859
            if t860 {
                var t861 int
                var inline944 int = vec_len__Vec_5uint8(reversed__209)
                t861 = inline944
                var t862 int = t861 - offset__212
                var t863 int = t862 - 1
                var t864 uint8 = vec_get__Vec_5uint8(reversed__209, t863)
                vec_push__Vec_5uint8(bytes__211, t864)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t865 int = compound_old358 + compound_value359
                offset__212 = t865
                continue
            } else {
                break Loop_loop858
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__next__0_i_closure__env__next__0_i_apply(env809 closure_env_next_0) int32 {
    var cell__0 *ref_int32_x = env809.cell_0
    var t888 int32
    var inline952 int32 = ref_get__Ref_5int32(cell__0)
    t888 = inline952
    var next__1 int32 = t888 + 1
    ref_set__Ref_5int32(cell__0, next__1)
    return next__1
}

func _goml_m_inherent_i_closure__env__reset__1_i_closure__env__reset__1_i_apply(env810 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env810.cell_0
    var inline954 int32 = 0
    ref_set__Ref_5int32(cell__0, inline954)
    return struct{}{}
}

func main() {
    main0()
}
