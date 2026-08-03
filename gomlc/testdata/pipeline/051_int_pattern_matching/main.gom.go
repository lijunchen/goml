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
    var x136 int8 = values__4._0
    var x137 int16 = values__4._1
    switch x137 {
    case 2:
        switch x136 {
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
    var x138 int32 = pair__5.head
    var x139 int64 = pair__5.tail
    switch x139 {
    case 200:
        switch x138 {
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
    var t174 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t174)
    var t175 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t175)
    var t176 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t176)
    var t177 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t177)
    var t178 bool = is_special8(5)
    var part1__14 string
    var inline244 string = "int8="
    var inline245 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t178)
    var inline246 string = inline244 + inline245
    part1__14 = inline246
    var t179 bool
    var inline242 int16 = 1024
    switch inline242 {
    case 1024:
        t179 = true
    case 2048:
        t179 = true
    default:
        t179 = false
    }
    var part2__15 string
    var inline238 string = ",int16="
    var inline239 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t179)
    var inline240 string = inline238 + inline239
    part2__15 = inline240
    var t180 bool
    var inline236 int32 = 8192
    switch inline236 {
    case 4096:
        t180 = true
    case 8192:
        t180 = true
    default:
        t180 = false
    }
    var part3__16 string
    var inline232 string = ",int32="
    var inline233 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t180)
    var inline234 string = inline232 + inline233
    part3__16 = inline234
    var t181 bool
    var inline230 int64 = 16384
    switch inline230 {
    case 16384:
        t181 = true
    case 32768:
        t181 = true
    default:
        t181 = false
    }
    var part4__17 string
    var inline226 string = ",int64_a="
    var inline227 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t181)
    var inline228 string = inline226 + inline227
    part4__17 = inline228
    var t182 bool
    var inline224 int64 = 32768
    switch inline224 {
    case 16384:
        t182 = true
    case 32768:
        t182 = true
    default:
        t182 = false
    }
    var part5__18 string
    var inline220 string = ",int64_b="
    var inline221 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t182)
    var inline222 string = inline220 + inline221
    part5__18 = inline222
    var part6__19 string
    var inline216 string = ",tuple_hit="
    var inline217 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_hit__10)
    var inline218 string = inline216 + inline217
    part6__19 = inline218
    var part7__20 string
    var inline212 string = ",tuple_miss="
    var inline213 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_miss__11)
    var inline214 string = inline212 + inline213
    part7__20 = inline214
    var part8__21 string
    var inline208 string = ",struct_first="
    var inline209 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_first__12)
    var inline210 string = inline208 + inline209
    part8__21 = inline210
    var part9__22 string
    var inline204 string = ",struct_second="
    var inline205 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_second__13)
    var inline206 string = inline204 + inline205
    part9__22 = inline206
    var t183 string = part1__14 + part2__15
    var t184 string = t183 + part3__16
    var t185 string = t184 + part4__17
    var t186 string = t185 + part5__18
    var t187 string = t186 + part6__19
    var t188 string = t187 + part7__20
    var t189 string = t188 + part8__21
    var message__23 string = t189 + part9__22
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__23)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t192 string = _goml_runtime_core_bool_to_string(self__66)
    return t192
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
