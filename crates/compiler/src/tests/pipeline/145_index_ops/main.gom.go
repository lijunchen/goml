package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_hash(s string) uint64 {
    var h uint64 = 14695981039346656037
    var i int32 = 0
    for {
        if i >= int32(len(s)) {
            break
        }
        h = h * 1099511628211 + uint64(s[i])
        i = i + 1
    }
    return h
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func _goml_trait_impl_Eq_string_eq(self string, other string) bool {
    return self == other
}

func _goml_trait_impl_Hash_string_hash(self string) uint64 {
    return string_hash(self)
}

func array_get__Array_2_int32(arr [2]int32, index int32) int32 {
    return arr[index]
}

func array_set__Array_2_int32(arr [2]int32, index int32, value int32) [2]int32 {
    arr[index] = value
    return arr
}

func array_get__Array_3_int32(arr [3]int32, index int32) int32 {
    return arr[index]
}

func array_set__Array_3_int32(arr [3]int32, index int32, value int32) [3]int32 {
    arr[index] = value
    return arr
}

func array_get__Array_2_Array_2_int32(arr [2][2]int32, index int32) [2]int32 {
    return arr[index]
}

func array_set__Array_2_Array_2_int32(arr [2][2]int32, index int32, value [2]int32) [2][2]int32 {
    arr[index] = value
    return arr
}

func vec_set__Vec_int32(vec []int32, index int32, value int32) struct{} {
    vec[index] = value
    return struct{}{}
}

func vec_set__Vec_Array_2_int32(vec [][2]int32, index int32, value [2]int32) struct{} {
    vec[index] = value
    return struct{}{}
}

type ref_array_2_int32_x struct {
    value [2]int32
}

func ref__Ref_Array_2_int32(value [2]int32) *ref_array_2_int32_x {
    return &ref_array_2_int32_x{
        value: value,
    }
}

func ref_get__Ref_Array_2_int32(reference *ref_array_2_int32_x) [2]int32 {
    return reference.value
}

func ref_set__Ref_Array_2_int32(reference *ref_array_2_int32_x, value [2]int32) struct{} {
    reference.value = value
    return struct{}{}
}

type hashmap_string_int32_x_entry struct {
    active bool
    key string
    value int32
}

type hashmap_string_int32_x struct {
    buckets map[uint64][]hashmap_string_int32_x_entry
    len int32
}

func hashmap_new__HashMap_string_int32() *hashmap_string_int32_x {
    return &hashmap_string_int32_x{
        buckets: make(map[uint64][]hashmap_string_int32_x_entry),
        len: 0,
    }
}

func hashmap_get_native__HashMap_string_int32(m *hashmap_string_int32_x, key string) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_trait_impl_Hash_string_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_string_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_impl_Eq_string_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_string_int32(m *hashmap_string_int32_x, key string) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_get_native__HashMap_string_int32(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_string_int32(m *hashmap_string_int32_x, key string, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_trait_impl_Hash_string_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_string_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_impl_Eq_string_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        i = i + 1
    }
    bucket = append(bucket, hashmap_string_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_Array2_int32_int32 struct {
    _0 [2]int32
    _1 int32
}

type Holder struct {
    data [2]int32
    vecs [][2]int32
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

type GoError = error

func print_opt_int(x__0 Option__int32) struct{} {
    switch x__0.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x0 int32 = x__0.(Some)._0
        var v__1 int32 = x0
        println__T_int32(v__1)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t67 [2]int32 = [2]int32{31, 32}
    var t68 int32 = array_get__Array_2_int32(t67, 1)
    println__T_int32(t68)
    var arr__2 [3]int32 = [3]int32{1, 2, 3}
    var t69 int32 = array_get__Array_3_int32(arr__2, 0)
    println__T_int32(t69)
    var arr2__3 [3]int32 = [3]int32{4, 5, 6}
    var place_root3 [3]int32 = arr2__3
    var index4 int32 = 1
    array_get__Array_3_int32(place_root3, index4)
    var value6 int32 = 50
    var t70 [3]int32 = array_set__Array_3_int32(place_root3, index4, value6)
    arr2__3 = t70
    var t72 int32 = array_get__Array_3_int32(arr2__3, 1)
    println__T_int32(t72)
    var t73 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32()
    var t74 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(t73, 7)
    var t75 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(t74, 8)
    var vec__4 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(t75, 9)
    var t76 int32 = vec__4[2]
    println__T_int32(t76)
    var t77 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32()
    var t78 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(t77, 10)
    var t79 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(t78, 11)
    var vec2__5 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(t79, 12)
    var place_root10 []int32 = vec2__5
    var index11 int32 = 0
    var value13 int32 = 100
    vec_set__Vec_int32(place_root10, index11, value13)
    var t81 int32 = vec2__5[0]
    println__T_int32(t81)
    var s__6 []int32 = vec2__5[0:2]
    var t82 int32 = s__6[1]
    println__T_int32(t82)
    var map__7 *hashmap_string_int32_x = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__new__K_string__V_int32()
    var place_root17 *hashmap_string_int32_x = map__7
    var index18 string = "a"
    hashmap_get__HashMap_string_int32(place_root17, index18)
    var value20 int32 = 13
    hashmap_set__HashMap_string_int32(place_root17, index18, value20)
    var t84 Option__int32 = hashmap_get__HashMap_string_int32(map__7, "a")
    print_opt_int(t84)
    var t85 Option__int32 = hashmap_get__HashMap_string_int32(map__7, "missing")
    print_opt_int(t85)
    var t86 [2]int32 = [2]int32{1, 2}
    var t87 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t86, t87}
    var place_root24 [2][2]int32 = matrix__8
    var index25 int32 = 1
    var place26 [2]int32 = array_get__Array_2_Array_2_int32(place_root24, index25)
    var index27 int32 = 0
    array_get__Array_2_int32(place26, index27)
    var value29 int32 = 30
    var t88 [2]int32 = array_set__Array_2_int32(place26, index27, value29)
    var t89 [2][2]int32 = array_set__Array_2_Array_2_int32(place_root24, index25, t88)
    matrix__8 = t89
    var t91 [2]int32 = array_get__Array_2_Array_2_int32(matrix__8, 1)
    var t92 int32 = array_get__Array_2_int32(t91, 0)
    println__T_int32(t92)
    var t93 [2]int32 = [2]int32{14, 15}
    var pair__9 Tuple2_Array2_int32_int32 = Tuple2_Array2_int32_int32{
        _0: t93,
        _1: 0,
    }
    var place_root32 Tuple2_Array2_int32_int32 = pair__9
    var place33 [2]int32 = place_root32._0
    var index34 int32 = 1
    array_get__Array_2_int32(place33, index34)
    var value36 int32 = 150
    var t94 [2]int32 = array_set__Array_2_int32(place33, index34, value36)
    var t95 int32 = place_root32._1
    var t96 Tuple2_Array2_int32_int32 = Tuple2_Array2_int32_int32{
        _0: t94,
        _1: t95,
    }
    pair__9 = t96
    var t98 [2]int32 = pair__9._0
    var t99 int32 = array_get__Array_2_int32(t98, 1)
    println__T_int32(t99)
    var t100 [2]int32 = [2]int32{16, 17}
    var t101 [][2]int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__new__T__x5b_int32_x3b_2_x5d_()
    var t102 [2]int32 = [2]int32{18, 19}
    var t103 [][2]int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T__x5b_int32_x3b_2_x5d_(t101, t102)
    var t104 [2]int32 = [2]int32{20, 21}
    var t105 [][2]int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T__x5b_int32_x3b_2_x5d_(t103, t104)
    var holder__10 Holder = Holder{
        data: t100,
        vecs: t105,
    }
    var place_root39 Holder = holder__10
    var place40 [2]int32 = place_root39.data
    var index41 int32 = 0
    array_get__Array_2_int32(place40, index41)
    var value43 int32 = 160
    var t106 [2]int32 = array_set__Array_2_int32(place40, index41, value43)
    var t107 [][2]int32 = place_root39.vecs
    var t108 Holder = Holder{
        data: t106,
        vecs: t107,
    }
    holder__10 = t108
    var t110 [2]int32 = holder__10.data
    var t111 int32 = array_get__Array_2_int32(t110, 0)
    println__T_int32(t111)
    var place_root46 Holder = holder__10
    var place47 [][2]int32 = place_root46.vecs
    var index48 int32 = 1
    var place49 [2]int32 = place47[index48]
    var index50 int32 = 0
    array_get__Array_2_int32(place49, index50)
    var value52 int32 = 200
    var t112 [2]int32 = array_set__Array_2_int32(place49, index50, value52)
    vec_set__Vec_Array_2_int32(place47, index48, t112)
    var t114 [][2]int32 = holder__10.vecs
    var t115 [2]int32 = t114[1]
    var t116 int32 = array_get__Array_2_int32(t115, 0)
    println__T_int32(t116)
    var t117 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_array_2_int32_x = ref__Ref_Array_2_int32(t117)
    var place_ref55 *ref_array_2_int32_x = r__11
    var place_root56 [2]int32 = ref_get__Ref_Array_2_int32(place_ref55)
    var index57 int32 = 1
    array_get__Array_2_int32(place_root56, index57)
    var value59 int32 = 230
    var t118 [2]int32 = array_set__Array_2_int32(place_root56, index57, value59)
    ref_set__Ref_Array_2_int32(place_ref55, t118)
    var t120 [2]int32 = ref_get__Ref_Array_2_int32(r__11)
    var t121 int32 = array_get__Array_2_int32(t120, 1)
    println__T_int32(t121)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t125 string = int32_to_string(value__1)
    string_println(t125)
    return struct{}{}
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32() []int32 {
    var retv128 []int32
    var t129 []int32 = nil
    retv128 = t129
    return retv128
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(self__67 []int32, elem__68 int32) []int32 {
    var retv131 []int32
    var t132 []int32 = append(self__67, elem__68)
    retv131 = t132
    return retv131
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__new__K_string__V_int32() *hashmap_string_int32_x {
    var retv134 *hashmap_string_int32_x
    var t135 *hashmap_string_int32_x = hashmap_new__HashMap_string_int32()
    retv134 = t135
    return retv134
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__new__T__x5b_int32_x3b_2_x5d_() [][2]int32 {
    var retv137 [][2]int32
    var t138 [][2]int32 = nil
    retv137 = t138
    return retv137
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__push__T__x5b_int32_x3b_2_x5d_(self__67 [][2]int32, elem__68 [2]int32) [][2]int32 {
    var retv140 [][2]int32
    var t141 [][2]int32 = append(self__67, elem__68)
    retv140 = t141
    return retv140
}

func main() {
    main0()
}
