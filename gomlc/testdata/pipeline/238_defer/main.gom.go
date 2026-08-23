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

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
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

type closure_env_run_0 struct {}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func early_return() int {
    var defer_return0 int = 7
    var inline3 string = "return:inner"
    var inline4 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3)
    _goml_runtime_core_string_println(inline4)
    var inline0 string = "return:outer"
    var inline1 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline0)
    _goml_runtime_core_string_println(inline1)
    return defer_return0
}

func maybe(value__0 Option__isize) Option__isize {
    var jp0 int
    switch value__0._tag {
    case 0:
        var defer_return0 Option__isize = Option__isize{
            _tag: 0,
        }
        var inline3 string = "try:cleanup"
        var inline4 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3)
        _goml_runtime_core_string_println(inline4)
        return defer_return0
    case 1:
        var x0 int = value__0._v1_0
        jp0 = x0
        var defer_result0 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: jp0,
        }
        var inline0 string = "try:cleanup"
        var inline1 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline0)
        _goml_runtime_core_string_println(inline1)
        return defer_result0
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__0 *ref_int_x
    var inline12 int = 0
    var inline13 *ref_int_x = ref__Ref_3int(inline12)
    index__0 = inline13
    Loop_loop0:
    for {
        var t0 int
        var inline11 int = ref_get__Ref_3int(index__0)
        t0 = inline11
        var t1 bool = t0 < 3
        if t1 {
            var current__0 int
            var inline10 int = ref_get__Ref_3int(index__0)
            current__0 = inline10
            var t2 int = current__0 + 1
            ref_set__Ref_3int(index__0, t2)
            var t3 bool = current__0 == 0
            if t3 {
                var t6 string
                var inline5 string = __goml_builtin_int_to_string(current__0)
                t6 = inline5
                var t7 string = "loop:" + t6
                var inline3 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t7)
                _goml_runtime_core_string_println(inline3)
                continue
            } else {
                var t8 bool = current__0 == 1
                if t8 {
                    var t9 string
                    var inline8 string = __goml_builtin_int_to_string(current__0)
                    t9 = inline8
                    var t10 string = "loop:" + t9
                    var inline6 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t10)
                    _goml_runtime_core_string_println(inline6)
                    break Loop_loop0
                } else {
                    var t4 string
                    var inline2 string = __goml_builtin_int_to_string(current__0)
                    t4 = inline2
                    var t5 string = "loop:" + t4
                    var inline0 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t5)
                    _goml_runtime_core_string_println(inline0)
                    continue
                }
            }
        } else {
            break Loop_loop0
        }
    }
    return struct{}{}
}

func pattern_cleanup(value__0 Option__isize) int {
    switch value__0._tag {
    case 1:
        var x0 int = value__0._v1_0
        var x1 int = 2
        var defer_tast_result0 int = x0 + x1
        var inline0 string = "pattern:cleanup"
        var inline1 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline0)
        _goml_runtime_core_string_println(inline1)
        return defer_tast_result0
    default:
        var defer_return0 int = 0
        var inline3 string = "pattern:cleanup"
        var inline4 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3)
        _goml_runtime_core_string_println(inline4)
        return defer_return0
    }
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t0 int = early_return()
    var t1 string
    var inline26 string = __goml_builtin_int_to_string(t0)
    t1 = inline26
    var inline24 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1)
    _goml_runtime_core_string_println(inline24)
    maybe(Option__isize{
        _tag: 0,
    })
    loop_cleanup()
    var inline19 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(inline19, "after")
    var inline21 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(inline19)
    var inline22 string = "observed:" + inline21
    println__T_string(inline22)
    var t2 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: 3,
    }
    var t3 int = pattern_cleanup(t2)
    var t4 string
    var inline18 string = __goml_builtin_int_to_string(t3)
    t4 = inline18
    var inline16 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t4)
    _goml_runtime_core_string_println(inline16)
    var t5 int
    var inline14 int = 0
    println__T_string("pattern:cleanup")
    t5 = inline14
    var t6 string
    var inline13 string = __goml_builtin_int_to_string(t5)
    t6 = inline13
    var inline11 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t6)
    _goml_runtime_core_string_println(inline11)
    var inline6 closure_env_run_0 = closure_env_run_0{}
    var inline7 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline6)
    }
    inline7()
    println__T_string("closure:after")
    println__T_string("closure:outer")
    var inline3 string = "main:second"
    var inline4 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3)
    _goml_runtime_core_string_println(inline4)
    var inline0 string = "main:first"
    var inline1 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline0)
    _goml_runtime_core_string_println(inline1)
    return struct{}{}
}

func println__T_string(value__0 string) struct{} {
    var t0 string
    t0 = value__0
    _goml_runtime_core_string_println(t0)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__0 string) *ref_string_x {
    var t0 *ref_string_x = ref__Ref_6string(value__0)
    return t0
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__0 *ref_string_x, value__0 string) struct{} {
    ref_set__Ref_6string(self__0, value__0)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__0 *ref_string_x) string {
    var t0 string = ref_get__Ref_6string(self__0)
    return t0
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__0 string) string {
    return self__0
}

func __goml_builtin_int_to_string(value__0 int) string {
    var t0 int64 = int64(int(value__0))
    var inline0 bool = t0 < 0
    if inline0 {
        var inline1 uint64 = uint64(int64(t0))
        var inline2 uint64 = 0 - inline1
        var inline3 string = decimal_string(inline2)
        var inline4 string = "-" + inline3
        return inline4
    } else {
        var inline5 uint64 = uint64(int64(t0))
        var inline6 string = decimal_string(inline5)
        return inline6
    }
}

func decimal_string(value__0 uint64) string {
    var t0 bool = value__0 == 0
    if t0 {
        return "0"
    } else {
        var reversed__0 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__0 uint64 = value__0
        Loop_loop0:
        for {
            var t10 bool = remaining__0 > 0
            if t10 {
                var t11 uint64 = remaining__0 % 10
                var t12 uint8 = uint8(uint64(t11))
                var t13 uint8 = t12 + 48
                vec_push__Vec_5uint8(reversed__0, t13)
                var compound_old1 uint64 = remaining__0
                var compound_value1 uint64 = 10
                var t14 uint64 = compound_old1 / compound_value1
                remaining__0 = t14
                continue
            } else {
                break Loop_loop0
            }
        }
        var t1 int
        var inline3 int = vec_len__Vec_5uint8(reversed__0)
        t1 = inline3
        var bytes__0 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1)
        var offset__0 int = 0
        Loop_loop1:
        for {
            var t2 int
            var inline2 int = vec_len__Vec_5uint8(reversed__0)
            t2 = inline2
            var t3 bool = offset__0 < t2
            if t3 {
                var t4 int
                var inline1 int = vec_len__Vec_5uint8(reversed__0)
                t4 = inline1
                var t5 int = t4 - offset__0
                var t6 int = t5 - 1
                var t7 uint8 = vec_get__Vec_5uint8(reversed__0, t6)
                vec_push__Vec_5uint8(bytes__0, t7)
                var compound_old0 int = offset__0
                var compound_value0 int = 1
                var t8 int = compound_old0 + compound_value0
                offset__0 = t8
                continue
            } else {
                break Loop_loop1
            }
        }
        var mtmp0 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__0)
        var x0 string = mtmp0._1
        return x0
    }
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env0 closure_env_run_0) struct{} {
    var inline3 string = "closure:body"
    var inline4 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3)
    _goml_runtime_core_string_println(inline4)
    var inline0 string = "closure:inner"
    var inline1 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline0)
    _goml_runtime_core_string_println(inline1)
    return struct{}{}
}

func main() {
    main0()
}
