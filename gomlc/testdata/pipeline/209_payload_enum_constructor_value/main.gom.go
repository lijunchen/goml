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

type closure_env_cons_0 struct {}

type Ordering int32

type List__i32 interface {
    isList__i32()
}

type Nil struct {}

func (_ Nil) isList__i32() {}

type Cons struct {
    _0 int32
    _1 List__i32
}

func (_ Cons) isList__i32() {}

func sum(values__3 List__i32) int32 {
    switch values__3.(type) {
    case Nil:
        return 0
    case Cons:
        var x796 int32 = values__3.(Cons)._0
        var x797 List__i32 = values__3.(Cons)._1
        var t806 int32 = sum(x797)
        var t807 int32 = x796 + t806
        return t807
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t809 closure_env_cons_0 = closure_env_cons_0{}
    var cons__6 func(int32, List__i32) List__i32 = func(p0 int32, p1 List__i32) List__i32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(t809, p0, p1)
    }
    var t810 List__i32
    var inline877 int32 = 2
    var inline878 List__i32 = cons__6(inline877, Nil{})
    t810 = inline878
    var values__7 List__i32
    var inline874 int32 = 1
    var inline875 List__i32 = cons__6(inline874, t810)
    values__7 = inline875
    var t811 int32 = sum(values__7)
    var t812 string
    var inline872 string = __goml_builtin_int32_to_string(t811)
    t812 = inline872
    var inline869 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
    _goml_runtime_core_string_println(inline869)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t824 int64 = int64(int32(value__225))
    var inline884 bool = t824 < 0
    if inline884 {
        var inline885 uint64 = uint64(int64(t824))
        var inline886 uint64 = 0 - inline885
        var inline887 string = decimal_string(inline886)
        var inline888 string = "-" + inline887
        return inline888
    } else {
        var inline889 uint64 = uint64(int64(t824))
        var inline890 string = decimal_string(inline889)
        return inline890
    }
}

func decimal_string(value__208 uint64) string {
    var t859 bool = value__208 == 0
    if t859 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop852:
        for {
            var t853 bool = remaining__210 > 0
            if t853 {
                var t854_rhs uint64 = 10
                var t854 uint64 = remaining__210 % t854_rhs
                var t855 uint8 = uint8(uint64(t854))
                var t856 uint8 = t855 + 48
                vec_push__Vec_5uint8(reversed__209, t856)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t857 uint64 = compound_old353 / compound_value354
                remaining__210 = t857
                continue
            } else {
                break Loop_loop852
            }
        }
        var t841 int
        var inline900 int = vec_len__Vec_5uint8(reversed__209)
        t841 = inline900
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t841)
        var offset__212 int = 0
        Loop_loop843:
        for {
            var t844 int
            var inline898 int = vec_len__Vec_5uint8(reversed__209)
            t844 = inline898
            var t845 bool = offset__212 < t844
            if t845 {
                var t846 int
                var inline896 int = vec_len__Vec_5uint8(reversed__209)
                t846 = inline896
                var t847 int = t846 - offset__212
                var t848 int = t847 - 1
                var t849 uint8 = vec_get__Vec_5uint8(reversed__209, t848)
                vec_push__Vec_5uint8(bytes__211, t849)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t850 int = compound_old358 + compound_value359
                offset__212 = t850
                continue
            } else {
                break Loop_loop843
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(env798 closure_env_cons_0, ctor_arg_0 int32, ctor_arg_1 List__i32) List__i32 {
    var t867 List__i32 = Cons{
        _0: ctor_arg_0,
        _1: ctor_arg_1,
    }
    return t867
}

func main() {
    main0()
}
