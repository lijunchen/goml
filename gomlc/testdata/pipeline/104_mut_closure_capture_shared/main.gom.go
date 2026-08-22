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

type closure_env_inc_0 struct {
    x_0 *ref_int_x
}

type Ordering int32

func main0() struct{} {
    var x__0 *ref_int_x = ref__Ref_3int(0)
    var t801 closure_env_inc_0 = closure_env_inc_0{
        x_0: x__0,
    }
    var inc__1 func() int = func() int {
        return _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(t801)
    }
    inc__1()
    var t802 int = ref_get__Ref_3int(x__0)
    var t803 string
    var inline870 string = __goml_builtin_int_to_string(t802)
    t803 = inline870
    var inline867 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t803)
    _goml_runtime_core_string_println(inline867)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t814 int64 = int64(int(value__222))
    var inline876 bool = t814 < 0
    if inline876 {
        var inline877 uint64 = uint64(int64(t814))
        var inline878 uint64 = 0 - inline877
        var inline879 string = decimal_string(inline878)
        var inline880 string = "-" + inline879
        return inline880
    } else {
        var inline881 uint64 = uint64(int64(t814))
        var inline882 string = decimal_string(inline881)
        return inline882
    }
}

func decimal_string(value__208 uint64) string {
    var t849 bool = value__208 == 0
    if t849 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop842:
        for {
            var t843 bool = remaining__210 > 0
            if t843 {
                var t844_rhs uint64 = 10
                var t844 uint64 = remaining__210 % t844_rhs
                var t845 uint8 = uint8(uint64(t844))
                var t846 uint8 = t845 + 48
                vec_push__Vec_5uint8(reversed__209, t846)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t847 uint64 = compound_old353 / compound_value354
                remaining__210 = t847
                continue
            } else {
                break Loop_loop842
            }
        }
        var t831 int
        var inline892 int = vec_len__Vec_5uint8(reversed__209)
        t831 = inline892
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t831)
        var offset__212 int = 0
        Loop_loop833:
        for {
            var t834 int
            var inline890 int = vec_len__Vec_5uint8(reversed__209)
            t834 = inline890
            var t835 bool = offset__212 < t834
            if t835 {
                var t836 int
                var inline888 int = vec_len__Vec_5uint8(reversed__209)
                t836 = inline888
                var t837 int = t836 - offset__212
                var t838 int = t837 - 1
                var t839 uint8 = vec_get__Vec_5uint8(reversed__209, t838)
                vec_push__Vec_5uint8(bytes__211, t839)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t840 int = compound_old358 + compound_value359
                offset__212 = t840
                continue
            } else {
                break Loop_loop833
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(env799 closure_env_inc_0) int {
    var x__0 *ref_int_x = env799.x_0
    var t863 int = ref_get__Ref_3int(x__0)
    var t864 int = t863 + 1
    ref_set__Ref_3int(x__0, t864)
    var t865 int = ref_get__Ref_3int(x__0)
    return t865
}

func main() {
    main0()
}
