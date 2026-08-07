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
    var x172 int8 = values__4._0
    var x173 int16 = values__4._1
    switch x173 {
    case 2:
        switch x172 {
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
    var x174 int32 = pair__5.head
    var x175 int64 = pair__5.tail
    switch x175 {
    case 200:
        switch x174 {
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
    var t210 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t210)
    var t211 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t211)
    var t212 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t212)
    var t213 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t213)
    var t214 bool = is_special8(5)
    var part1__14 string
    var inline280 string = "int8="
    var inline281 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t214)
    var inline282 string = inline280 + inline281
    part1__14 = inline282
    var t215 bool
    var inline278 int16 = 1024
    switch inline278 {
    case 1024:
        t215 = true
    case 2048:
        t215 = true
    default:
        t215 = false
    }
    var part2__15 string
    var inline274 string = ",int16="
    var inline275 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t215)
    var inline276 string = inline274 + inline275
    part2__15 = inline276
    var t216 bool
    var inline272 int32 = 8192
    switch inline272 {
    case 4096:
        t216 = true
    case 8192:
        t216 = true
    default:
        t216 = false
    }
    var part3__16 string
    var inline268 string = ",int32="
    var inline269 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t216)
    var inline270 string = inline268 + inline269
    part3__16 = inline270
    var t217 bool
    var inline266 int64 = 16384
    switch inline266 {
    case 16384:
        t217 = true
    case 32768:
        t217 = true
    default:
        t217 = false
    }
    var part4__17 string
    var inline262 string = ",int64_a="
    var inline263 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t217)
    var inline264 string = inline262 + inline263
    part4__17 = inline264
    var t218 bool
    var inline260 int64 = 32768
    switch inline260 {
    case 16384:
        t218 = true
    case 32768:
        t218 = true
    default:
        t218 = false
    }
    var part5__18 string
    var inline256 string = ",int64_b="
    var inline257 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t218)
    var inline258 string = inline256 + inline257
    part5__18 = inline258
    var part6__19 string
    var inline252 string = ",tuple_hit="
    var inline253 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_hit__10)
    var inline254 string = inline252 + inline253
    part6__19 = inline254
    var part7__20 string
    var inline248 string = ",tuple_miss="
    var inline249 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_miss__11)
    var inline250 string = inline248 + inline249
    part7__20 = inline250
    var part8__21 string
    var inline244 string = ",struct_first="
    var inline245 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_first__12)
    var inline246 string = inline244 + inline245
    part8__21 = inline246
    var part9__22 string
    var inline240 string = ",struct_second="
    var inline241 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_second__13)
    var inline242 string = inline240 + inline241
    part9__22 = inline242
    var t219 string = part1__14 + part2__15
    var t220 string = t219 + part3__16
    var t221 string = t220 + part4__17
    var t222 string = t221 + part5__18
    var t223 string = t222 + part6__19
    var t224 string = t223 + part7__20
    var t225 string = t224 + part8__21
    var message__23 string = t225 + part9__22
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__23)
    _goml_runtime_core_string_println(inline237)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t228 string = _goml_runtime_core_bool_to_string(self__66)
    return t228
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
