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
    var x182 int8 = values__4._0
    var x183 int16 = values__4._1
    switch x183 {
    case 2:
        switch x182 {
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
    var x184 int32 = pair__5.head
    var x185 int64 = pair__5.tail
    switch x185 {
    case 200:
        switch x184 {
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
    var t220 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t220)
    var t221 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t221)
    var t222 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t222)
    var t223 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t223)
    var t224 bool = is_special8(5)
    var part1__14 string
    var inline290 string = "int8="
    var inline291 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t224)
    var inline292 string = inline290 + inline291
    part1__14 = inline292
    var t225 bool
    var inline288 int16 = 1024
    switch inline288 {
    case 1024:
        t225 = true
    case 2048:
        t225 = true
    default:
        t225 = false
    }
    var part2__15 string
    var inline284 string = ",int16="
    var inline285 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t225)
    var inline286 string = inline284 + inline285
    part2__15 = inline286
    var t226 bool
    var inline282 int32 = 8192
    switch inline282 {
    case 4096:
        t226 = true
    case 8192:
        t226 = true
    default:
        t226 = false
    }
    var part3__16 string
    var inline278 string = ",int32="
    var inline279 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t226)
    var inline280 string = inline278 + inline279
    part3__16 = inline280
    var t227 bool
    var inline276 int64 = 16384
    switch inline276 {
    case 16384:
        t227 = true
    case 32768:
        t227 = true
    default:
        t227 = false
    }
    var part4__17 string
    var inline272 string = ",int64_a="
    var inline273 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t227)
    var inline274 string = inline272 + inline273
    part4__17 = inline274
    var t228 bool
    var inline270 int64 = 32768
    switch inline270 {
    case 16384:
        t228 = true
    case 32768:
        t228 = true
    default:
        t228 = false
    }
    var part5__18 string
    var inline266 string = ",int64_b="
    var inline267 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t228)
    var inline268 string = inline266 + inline267
    part5__18 = inline268
    var part6__19 string
    var inline262 string = ",tuple_hit="
    var inline263 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_hit__10)
    var inline264 string = inline262 + inline263
    part6__19 = inline264
    var part7__20 string
    var inline258 string = ",tuple_miss="
    var inline259 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_miss__11)
    var inline260 string = inline258 + inline259
    part7__20 = inline260
    var part8__21 string
    var inline254 string = ",struct_first="
    var inline255 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_first__12)
    var inline256 string = inline254 + inline255
    part8__21 = inline256
    var part9__22 string
    var inline250 string = ",struct_second="
    var inline251 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_second__13)
    var inline252 string = inline250 + inline251
    part9__22 = inline252
    var t229 string = part1__14 + part2__15
    var t230 string = t229 + part3__16
    var t231 string = t230 + part4__17
    var t232 string = t231 + part5__18
    var t233 string = t232 + part6__19
    var t234 string = t233 + part7__20
    var t235 string = t234 + part8__21
    var message__23 string = t235 + part9__22
    var inline247 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__23)
    _goml_runtime_core_string_println(inline247)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t238 string = _goml_runtime_core_bool_to_string(self__64)
    return t238
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
