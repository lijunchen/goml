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

type Point struct {
    x int32
    y int32
}

type Node struct {
    value int32
    next List
}

type Wrapper__i32 struct {
    value int32
}

type closure_env_add_0 struct {
    offset_0 int32
}

type closure_env_id_1 struct {}

type Ordering int32

type List interface {
    isList()
}

type Cons struct {
    _0 Node
}

func (_ Cons) isList() {}

type Nil struct {}

func (_ Nil) isList() {}

type Shape__i32 struct {
    _tag int32
    _v0_0 Point
    _v1_0 Wrapper__i32
}

func list_value(value__10 List) int32 {
    switch value__10.(type) {
    case Cons:
        var x798 Node = value__10.(Cons)._0
        var t828 int32 = x798.value
        var t829 List = x798.next
        var t830 int32 = list_value(t829)
        var t831 int32 = t828 + t830
        return t831
    case Nil:
        return 0
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var offset__12 int32 = 1
    var t833 closure_env_add_0 = closure_env_add_0{
        offset_0: offset__12,
    }
    var add__14 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(t833, p0)
    }
    var t834 int32 = add__14(1)
    var point__15 Point
    var inline938 int32 = 3
    var inline939 Point = Point{
        x: t834,
        y: inline938,
    }
    point__15 = inline939
    var t835 Point
    var inline936 Point = Point{
        x: 0,
        y: 0,
    }
    t835 = inline936
    var combined__16 Point
    var inline928 int32 = point__15.x
    var inline929 int32 = t835.x
    var inline930 int32 = inline928 + inline929
    var inline931 int32 = point__15.y
    var inline932 int32 = t835.y
    var inline933 int32 = inline931 + inline932
    var inline934 Point = Point{
        x: inline930,
        y: inline933,
    }
    combined__16 = inline934
    var t836 int32
    var inline923 int32 = 4
    var inline924 closure_env_id_1 = closure_env_id_1{}
    var inline925 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(inline924, p0)
    }
    var inline926 int32 = inline925(inline923)
    t836 = inline926
    var t838 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t838,
    }
    var t839 int32 = combined__16.x
    var t840 int32 = combined__16.y
    var t841 int32 = t839 + t840
    var t842 int32
    t842 = t836
    var t843 int32 = t841 + t842
    var t844 int32 = list_value(list__18)
    var t845 int32 = t843 + t844
    var t846 string
    var inline912 string = __goml_builtin_int32_to_string(t845)
    t846 = inline912
    var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t846)
    _goml_runtime_core_string_println(inline909)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t862 int64 = int64(int32(value__225))
    var inline945 bool = t862 < 0
    if inline945 {
        var inline946 uint64 = uint64(int64(t862))
        var inline947 uint64 = 0 - inline946
        var inline948 string = decimal_string(inline947)
        var inline949 string = "-" + inline948
        return inline949
    } else {
        var inline950 uint64 = uint64(int64(t862))
        var inline951 string = decimal_string(inline950)
        return inline951
    }
}

func decimal_string(value__208 uint64) string {
    var t897 bool = value__208 == 0
    if t897 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop890:
        for {
            var t891 bool = remaining__210 > 0
            if t891 {
                var t892_rhs uint64 = 10
                var t892 uint64 = remaining__210 % t892_rhs
                var t893 uint8 = uint8(uint64(t892))
                var t894 uint8 = t893 + 48
                vec_push__Vec_5uint8(reversed__209, t894)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t895 uint64 = compound_old353 / compound_value354
                remaining__210 = t895
                continue
            } else {
                break Loop_loop890
            }
        }
        var t879 int
        var inline961 int = vec_len__Vec_5uint8(reversed__209)
        t879 = inline961
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t879)
        var offset__212 int = 0
        Loop_loop881:
        for {
            var t882 int
            var inline959 int = vec_len__Vec_5uint8(reversed__209)
            t882 = inline959
            var t883 bool = offset__212 < t882
            if t883 {
                var t884 int
                var inline957 int = vec_len__Vec_5uint8(reversed__209)
                t884 = inline957
                var t885 int = t884 - offset__212
                var t886 int = t885 - 1
                var t887 uint8 = vec_get__Vec_5uint8(reversed__209, t886)
                vec_push__Vec_5uint8(bytes__211, t887)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t888 int = compound_old358 + compound_value359
                offset__212 = t888
                continue
            } else {
                break Loop_loop881
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(env799 closure_env_add_0, value__13 int32) int32 {
    var offset__12 int32 = env799.offset_0
    var t905 int32 = value__13 + offset__12
    return t905
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env800 closure_env_id_1, item__5 int32) int32 {
    return item__5
}

func main() {
    main0()
}
