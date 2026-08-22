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
        var inline865 string = __goml_builtin_int32_to_string(x796)
        return inline865
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var value__4 int32 = 42
    var t806 Boxed__i32
    var inline872 Boxed__i32 = Boxed__i32{
        _tag: 0,
        _v0_0: value__4,
    }
    t806 = inline872
    var t807 dyn__Show = dyn__Show{
        data: t806,
        vtable: dyn__Show__vtable__Boxed__i32(),
    }
    var t808 string
    var inline870 string = t807.vtable.show(t807.data)
    t808 = inline870
    var inline867 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline867)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t821 int64 = int64(int32(value__225))
    var inline878 bool = t821 < 0
    if inline878 {
        var inline879 uint64 = uint64(int64(t821))
        var inline880 uint64 = 0 - inline879
        var inline881 string = decimal_string(inline880)
        var inline882 string = "-" + inline881
        return inline882
    } else {
        var inline883 uint64 = uint64(int64(t821))
        var inline884 string = decimal_string(inline883)
        return inline884
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t858 bool = value__208 == 0
    if t858 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop851:
        for {
            var t852 bool = remaining__210 > 0
            if t852 {
                var t853_rhs uint64 = 10
                var t853 uint64 = remaining__210 % t853_rhs
                var t854 uint8 = uint8(uint64(t853))
                var t855 uint8 = t854 + 48
                vec_push__Vec_5uint8(reversed__209, t855)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t856 uint64 = compound_old353 / compound_value354
                remaining__210 = t856
                continue
            } else {
                break Loop_loop851
            }
        }
        var t840 int
        var inline894 int = vec_len__Vec_5uint8(reversed__209)
        t840 = inline894
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t840)
        var offset__212 int = 0
        Loop_loop842:
        for {
            var t843 int
            var inline892 int = vec_len__Vec_5uint8(reversed__209)
            t843 = inline892
            var t844 bool = offset__212 < t843
            if t844 {
                var t845 int
                var inline890 int = vec_len__Vec_5uint8(reversed__209)
                t845 = inline890
                var t846 int = t845 - offset__212
                var t847 int = t846 - 1
                var t848 uint8 = vec_get__Vec_5uint8(reversed__209, t847)
                vec_push__Vec_5uint8(bytes__211, t848)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t849 int = compound_old358 + compound_value359
                offset__212 = t849
                continue
            } else {
                break Loop_loop842
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
