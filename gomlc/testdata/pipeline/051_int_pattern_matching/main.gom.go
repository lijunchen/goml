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
    var x187 int8 = values__4._0
    var x188 int16 = values__4._1
    switch x188 {
    case 2:
        switch x187 {
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
    var x189 int32 = pair__5.head
    var x190 int64 = pair__5.tail
    switch x190 {
    case 200:
        switch x189 {
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
    var t225 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t225)
    var t226 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t226)
    var t227 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t227)
    var t228 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t228)
    var t229 bool = is_special8(5)
    var part1__14 string
    var inline295 string = "int8="
    var inline296 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t229)
    var inline297 string = inline295 + inline296
    part1__14 = inline297
    var t230 bool
    var inline293 int16 = 1024
    switch inline293 {
    case 1024:
        t230 = true
    case 2048:
        t230 = true
    default:
        t230 = false
    }
    var part2__15 string
    var inline289 string = ",int16="
    var inline290 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t230)
    var inline291 string = inline289 + inline290
    part2__15 = inline291
    var t231 bool
    var inline287 int32 = 8192
    switch inline287 {
    case 4096:
        t231 = true
    case 8192:
        t231 = true
    default:
        t231 = false
    }
    var part3__16 string
    var inline283 string = ",int32="
    var inline284 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t231)
    var inline285 string = inline283 + inline284
    part3__16 = inline285
    var t232 bool
    var inline281 int64 = 16384
    switch inline281 {
    case 16384:
        t232 = true
    case 32768:
        t232 = true
    default:
        t232 = false
    }
    var part4__17 string
    var inline277 string = ",int64_a="
    var inline278 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t232)
    var inline279 string = inline277 + inline278
    part4__17 = inline279
    var t233 bool
    var inline275 int64 = 32768
    switch inline275 {
    case 16384:
        t233 = true
    case 32768:
        t233 = true
    default:
        t233 = false
    }
    var part5__18 string
    var inline271 string = ",int64_b="
    var inline272 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t233)
    var inline273 string = inline271 + inline272
    part5__18 = inline273
    var part6__19 string
    var inline267 string = ",tuple_hit="
    var inline268 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_hit__10)
    var inline269 string = inline267 + inline268
    part6__19 = inline269
    var part7__20 string
    var inline263 string = ",tuple_miss="
    var inline264 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_miss__11)
    var inline265 string = inline263 + inline264
    part7__20 = inline265
    var part8__21 string
    var inline259 string = ",struct_first="
    var inline260 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_first__12)
    var inline261 string = inline259 + inline260
    part8__21 = inline261
    var part9__22 string
    var inline255 string = ",struct_second="
    var inline256 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_second__13)
    var inline257 string = inline255 + inline256
    part9__22 = inline257
    var t234 string = part1__14 + part2__15
    var t235 string = t234 + part3__16
    var t236 string = t235 + part4__17
    var t237 string = t236 + part5__18
    var t238 string = t237 + part6__19
    var t239 string = t238 + part7__20
    var t240 string = t239 + part8__21
    var message__23 string = t240 + part9__22
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__23)
    _goml_runtime_core_string_println(inline252)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t243 string = _goml_runtime_core_bool_to_string(self__64)
    return t243
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
