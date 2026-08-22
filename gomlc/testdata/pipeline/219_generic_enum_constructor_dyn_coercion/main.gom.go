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

type Ordering int32

type Boxed__i32 struct {
    _tag int32
    _v0_0 int32
}

type dyn__Show_vtable struct {
    show func(any) string
}

type dyn__Show struct {
    data any
    vtable *dyn__Show_vtable
}

func dyn__Show__wrap__Boxed__i32__show(self any) string {
    switch v := self.(type) {
    case Boxed__i32:
        return _goml_m_trait__impl_i_Show_i_Boxed____i32_i_show(v)
    default:
        panic("unexpected type")
    }
}

func dyn__Show__vtable__Boxed__i32() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__Boxed__i32__show,
    }
}

func _goml_m_trait__impl_i_Show_i_Boxed____i32_i_show(self__0 Boxed__i32) string {
    switch self__0._tag {
    case 0:
        var x796 int32 = self__0._v0_0
        var inline862 string = __goml_builtin_int32_to_string(x796)
        return inline862
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var value__3 int32 = 42
    var t806 Boxed__i32 = Boxed__i32{
        _tag: 0,
        _v0_0: value__3,
    }
    var t807 dyn__Show = dyn__Show{
        data: t806,
        vtable: dyn__Show__vtable__Boxed__i32(),
    }
    var t808 string
    var inline867 string = t807.vtable.show(t807.data)
    t808 = inline867
    var inline864 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline864)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t818 int64 = int64(int32(value__225))
    var inline873 bool = t818 < 0
    if inline873 {
        var inline874 uint64 = uint64(int64(t818))
        var inline875 uint64 = 0 - inline874
        var inline876 string = decimal_string(inline875)
        var inline877 string = "-" + inline876
        return inline877
    } else {
        var inline878 uint64 = uint64(int64(t818))
        var inline879 string = decimal_string(inline878)
        return inline879
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t855 bool = value__208 == 0
    if t855 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop848:
        for {
            var t849 bool = remaining__210 > 0
            if t849 {
                var t850_rhs uint64 = 10
                var t850 uint64 = remaining__210 % t850_rhs
                var t851 uint8 = uint8(uint64(t850))
                var t852 uint8 = t851 + 48
                vec_push__Vec_5uint8(reversed__209, t852)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t853 uint64 = compound_old353 / compound_value354
                remaining__210 = t853
                continue
            } else {
                break Loop_loop848
            }
        }
        var t837 int
        var inline889 int = vec_len__Vec_5uint8(reversed__209)
        t837 = inline889
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t837)
        var offset__212 int = 0
        Loop_loop839:
        for {
            var t840 int
            var inline887 int = vec_len__Vec_5uint8(reversed__209)
            t840 = inline887
            var t841 bool = offset__212 < t840
            if t841 {
                var t842 int
                var inline885 int = vec_len__Vec_5uint8(reversed__209)
                t842 = inline885
                var t843 int = t842 - offset__212
                var t844 int = t843 - 1
                var t845 uint8 = vec_get__Vec_5uint8(reversed__209, t844)
                vec_push__Vec_5uint8(bytes__211, t845)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t846 int = compound_old358 + compound_value359
                offset__212 = t846
                continue
            } else {
                break Loop_loop839
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
