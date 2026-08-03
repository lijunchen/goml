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
    var x177 int8 = values__4._0
    var x178 int16 = values__4._1
    switch x178 {
    case 2:
        switch x177 {
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
    var x179 int32 = pair__5.head
    var x180 int64 = pair__5.tail
    switch x180 {
    case 200:
        switch x179 {
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
    var t215 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t215)
    var t216 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t216)
    var t217 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t217)
    var t218 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t218)
    var t219 bool = is_special8(5)
    var part1__14 string
    var inline285 string = "int8="
    var inline286 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t219)
    var inline287 string = inline285 + inline286
    part1__14 = inline287
    var t220 bool
    var inline283 int16 = 1024
    switch inline283 {
    case 1024:
        t220 = true
    case 2048:
        t220 = true
    default:
        t220 = false
    }
    var part2__15 string
    var inline279 string = ",int16="
    var inline280 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t220)
    var inline281 string = inline279 + inline280
    part2__15 = inline281
    var t221 bool
    var inline277 int32 = 8192
    switch inline277 {
    case 4096:
        t221 = true
    case 8192:
        t221 = true
    default:
        t221 = false
    }
    var part3__16 string
    var inline273 string = ",int32="
    var inline274 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t221)
    var inline275 string = inline273 + inline274
    part3__16 = inline275
    var t222 bool
    var inline271 int64 = 16384
    switch inline271 {
    case 16384:
        t222 = true
    case 32768:
        t222 = true
    default:
        t222 = false
    }
    var part4__17 string
    var inline267 string = ",int64_a="
    var inline268 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t222)
    var inline269 string = inline267 + inline268
    part4__17 = inline269
    var t223 bool
    var inline265 int64 = 32768
    switch inline265 {
    case 16384:
        t223 = true
    case 32768:
        t223 = true
    default:
        t223 = false
    }
    var part5__18 string
    var inline261 string = ",int64_b="
    var inline262 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t223)
    var inline263 string = inline261 + inline262
    part5__18 = inline263
    var part6__19 string
    var inline257 string = ",tuple_hit="
    var inline258 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_hit__10)
    var inline259 string = inline257 + inline258
    part6__19 = inline259
    var part7__20 string
    var inline253 string = ",tuple_miss="
    var inline254 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_miss__11)
    var inline255 string = inline253 + inline254
    part7__20 = inline255
    var part8__21 string
    var inline249 string = ",struct_first="
    var inline250 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_first__12)
    var inline251 string = inline249 + inline250
    part8__21 = inline251
    var part9__22 string
    var inline245 string = ",struct_second="
    var inline246 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_second__13)
    var inline247 string = inline245 + inline246
    part9__22 = inline247
    var t224 string = part1__14 + part2__15
    var t225 string = t224 + part3__16
    var t226 string = t225 + part4__17
    var t227 string = t226 + part5__18
    var t228 string = t227 + part6__19
    var t229 string = t228 + part7__20
    var t230 string = t229 + part8__21
    var message__23 string = t230 + part9__22
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__23)
    _goml_runtime_core_string_println(inline242)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t233 string = _goml_runtime_core_bool_to_string(self__66)
    return t233
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
