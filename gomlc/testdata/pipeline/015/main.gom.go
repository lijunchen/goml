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

type IntList interface {
    isIntList()
}

type Nil struct {}

func (_ Nil) isIntList() {}

type Cons struct {
    _0 int32
    _1 IntList
}

func (_ Cons) isIntList() {}

func print_int_list(xs__0 IntList) struct{} {
    switch xs__0.(type) {
    case Nil:
        var inline898 string = "Nil"
        var inline899 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline898)
        _goml_runtime_core_string_println(inline899)
        return struct{}{}
    case Cons:
        var x796 int32 = xs__0.(Cons)._0
        var x797 IntList = xs__0.(Cons)._1
        var inline919 string = "Cons"
        var inline920 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline919)
        _goml_runtime_core_string_println(inline920)
        var inline915 string = "("
        var inline916 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline915)
        _goml_runtime_core_string_println(inline916)
        var t824 string
        var inline913 string = __goml_builtin_int32_to_string(x796)
        t824 = inline913
        var inline910 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t824)
        _goml_runtime_core_string_println(inline910)
        var inline906 string = ", "
        var inline907 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline906)
        _goml_runtime_core_string_println(inline907)
        print_int_list(x797)
        var inline902 string = ")"
        var inline903 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline902)
        _goml_runtime_core_string_println(inline903)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    switch xs__3.(type) {
    case Nil:
        return acc__4
    case Cons:
        var x804 int32 = xs__3.(Cons)._0
        var x805 IntList = xs__3.(Cons)._1
        var t829 IntList = Cons{
            _0: x804,
            _1: acc__4,
        }
        var t830 IntList = int_list_rev_aux(x805, t829)
        return t830
    default:
        panic("non-exhaustive match")
    }
}

func int_list_length(xs__8 IntList) int32 {
    switch xs__8.(type) {
    case Nil:
        return 0
    case Cons:
        var x807 IntList = xs__8.(Cons)._1
        var t838 int32 = int_list_length(x807)
        var t839 int32 = 1 + t838
        return t839
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    var inline961 string = ""
    var inline962 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline961)
    _goml_runtime_core_string_println(inline962)
    println__T_string("Length: ")
    var inline957 int32 = int_list_length(x__11)
    var inline958 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline957)
    println__T_string(inline958)
    var x__12 IntList = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    var inline952 string = ""
    var inline953 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline952)
    _goml_runtime_core_string_println(inline953)
    println__T_string("Length: ")
    var inline948 int32 = int_list_length(x__12)
    var inline949 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline948)
    println__T_string(inline949)
    var t844 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t845 IntList = Cons{
        _0: 2,
        _1: t844,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t845,
    }
    print_int_list(x__13)
    var inline943 string = ""
    var inline944 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline943)
    _goml_runtime_core_string_println(inline944)
    println__T_string("Length: ")
    var inline939 int32 = int_list_length(x__13)
    var inline940 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline939)
    println__T_string(inline940)
    var y__14 IntList
    var inline936 IntList = int_list_rev_aux(x__13, Nil{})
    y__14 = inline936
    print_int_list(y__14)
    var inline932 string = ""
    var inline933 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline932)
    _goml_runtime_core_string_println(inline933)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t847 string
    t847 = value__1
    _goml_runtime_core_string_println(t847)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline966 int64 = int64(int32(self__286))
    var inline967 string = signed_decimal_string(inline966)
    return inline967
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t856 int64 = int64(int32(value__225))
    var inline969 bool = t856 < 0
    if inline969 {
        var inline970 uint64 = uint64(int64(t856))
        var inline971 uint64 = 0 - inline970
        var inline972 string = decimal_string(inline971)
        var inline973 string = "-" + inline972
        return inline973
    } else {
        var inline974 uint64 = uint64(int64(t856))
        var inline975 string = decimal_string(inline974)
        return inline975
    }
}

func signed_decimal_string(value__214 int64) string {
    var t862 bool = value__214 < 0
    if t862 {
        var t863 uint64 = uint64(int64(value__214))
        var t864 uint64 = 0 - t863
        var t865 string = decimal_string(t864)
        var t866 string = "-" + t865
        return t866
    } else {
        var t867 uint64 = uint64(int64(value__214))
        var t868 string = decimal_string(t867)
        return t868
    }
}

func decimal_string(value__208 uint64) string {
    var t891 bool = value__208 == 0
    if t891 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop884:
        for {
            var t885 bool = remaining__210 > 0
            if t885 {
                var t886_rhs uint64 = 10
                var t886 uint64 = remaining__210 % t886_rhs
                var t887 uint8 = uint8(uint64(t886))
                var t888 uint8 = t887 + 48
                vec_push__Vec_5uint8(reversed__209, t888)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t889 uint64 = compound_old353 / compound_value354
                remaining__210 = t889
                continue
            } else {
                break Loop_loop884
            }
        }
        var t873 int
        var inline985 int = vec_len__Vec_5uint8(reversed__209)
        t873 = inline985
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t873)
        var offset__212 int = 0
        Loop_loop875:
        for {
            var t876 int
            var inline983 int = vec_len__Vec_5uint8(reversed__209)
            t876 = inline983
            var t877 bool = offset__212 < t876
            if t877 {
                var t878 int
                var inline981 int = vec_len__Vec_5uint8(reversed__209)
                t878 = inline981
                var t879 int = t878 - offset__212
                var t880 int = t879 - 1
                var t881 uint8 = vec_get__Vec_5uint8(reversed__209, t880)
                vec_push__Vec_5uint8(bytes__211, t881)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t882 int = compound_old358 + compound_value359
                offset__212 = t882
                continue
            } else {
                break Loop_loop875
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
