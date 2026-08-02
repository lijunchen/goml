package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_4int8_5int16 struct {
    _0 int8
    _1 int16
}

type PairData struct {
    head int32
    tail int64
}

func is_special8(value__0 int8) bool {
    switch value__0 {
    case 5:
        return true
    case 7:
        return true
    default:
        return false
    }
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var x155 int8 = values__4._0
    var x156 int16 = values__4._1
    switch x156 {
    case 2:
        switch x155 {
        case 1:
            return true
        default:
            return false
        }
    default:
        return false
    }
}

func match_struct(pair__5 PairData) bool {
    var x157 int32 = pair__5.head
    var x158 int64 = pair__5.tail
    switch x158 {
    case 200:
        switch x157 {
        case 100:
            return true
        default:
            return false
        }
    case 300:
        return true
    default:
        return false
    }
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t193 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t193)
    var t194 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t194)
    var t195 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t195)
    var t196 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t196)
    var t197 bool = is_special8(5)
    var part1__14 string
    var inline263 string = "int8="
    var inline264 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t197)
    var inline265 string = inline263 + inline264
    part1__14 = inline265
    var t198 bool
    var inline261 int16 = 1024
    switch inline261 {
    case 1024:
        t198 = true
    case 2048:
        t198 = true
    default:
        t198 = false
    }
    var part2__15 string
    var inline257 string = ",int16="
    var inline258 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t198)
    var inline259 string = inline257 + inline258
    part2__15 = inline259
    var t199 bool
    var inline255 int32 = 8192
    switch inline255 {
    case 4096:
        t199 = true
    case 8192:
        t199 = true
    default:
        t199 = false
    }
    var part3__16 string
    var inline251 string = ",int32="
    var inline252 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t199)
    var inline253 string = inline251 + inline252
    part3__16 = inline253
    var t200 bool
    var inline249 int64 = 16384
    switch inline249 {
    case 16384:
        t200 = true
    case 32768:
        t200 = true
    default:
        t200 = false
    }
    var part4__17 string
    var inline245 string = ",int64_a="
    var inline246 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t200)
    var inline247 string = inline245 + inline246
    part4__17 = inline247
    var t201 bool
    var inline243 int64 = 32768
    switch inline243 {
    case 16384:
        t201 = true
    case 32768:
        t201 = true
    default:
        t201 = false
    }
    var part5__18 string
    var inline239 string = ",int64_b="
    var inline240 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t201)
    var inline241 string = inline239 + inline240
    part5__18 = inline241
    var part6__19 string
    var inline235 string = ",tuple_hit="
    var inline236 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_hit__10)
    var inline237 string = inline235 + inline236
    part6__19 = inline237
    var part7__20 string
    var inline231 string = ",tuple_miss="
    var inline232 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_miss__11)
    var inline233 string = inline231 + inline232
    part7__20 = inline233
    var part8__21 string
    var inline227 string = ",struct_first="
    var inline228 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_first__12)
    var inline229 string = inline227 + inline228
    part8__21 = inline229
    var part9__22 string
    var inline223 string = ",struct_second="
    var inline224 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_second__13)
    var inline225 string = inline223 + inline224
    part9__22 = inline225
    var t202 string = part1__14 + part2__15
    var t203 string = t202 + part3__16
    var t204 string = t203 + part4__17
    var t205 string = t204 + part5__18
    var t206 string = t205 + part6__19
    var t207 string = t206 + part7__20
    var t208 string = t207 + part8__21
    var message__23 string = t208 + part9__22
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__23)
    _goml_runtime_core_string_println(inline220)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t211 string = _goml_runtime_core_bool_to_string(self__37)
    return t211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
