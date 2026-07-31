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
    var retv158 bool
    var jp160 bool
    switch value__0 {
    case 5:
        jp160 = true
    case 7:
        jp160 = true
    default:
        jp160 = false
    }
    retv158 = jp160
    return retv158
}

func is_special16(value__1 int16) bool {
    var retv162 bool
    var jp164 bool
    switch value__1 {
    case 1024:
        jp164 = true
    case 2048:
        jp164 = true
    default:
        jp164 = false
    }
    retv162 = jp164
    return retv162
}

func is_special32(value__2 int32) bool {
    var retv166 bool
    var jp168 bool
    switch value__2 {
    case 4096:
        jp168 = true
    case 8192:
        jp168 = true
    default:
        jp168 = false
    }
    retv166 = jp168
    return retv166
}

func is_special64(value__3 int64) bool {
    var retv170 bool
    var jp172 bool
    switch value__3 {
    case 16384:
        jp172 = true
    case 32768:
        jp172 = true
    default:
        jp172 = false
    }
    retv170 = jp172
    return retv170
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var retv174 bool
    var x152 int8 = values__4._0
    var x153 int16 = values__4._1
    var jp176 bool
    switch x153 {
    case 2:
        var jp178 bool
        switch x152 {
        case 1:
            jp178 = true
        default:
            jp178 = false
        }
        jp176 = jp178
    default:
        jp176 = false
    }
    retv174 = jp176
    return retv174
}

func match_struct(pair__5 PairData) bool {
    var retv180 bool
    var x154 int32 = pair__5.head
    var x155 int64 = pair__5.tail
    var jp182 bool
    switch x155 {
    case 200:
        var jp184 bool
        switch x154 {
        case 100:
            jp184 = true
        default:
            jp184 = false
        }
        jp182 = jp184
    case 300:
        jp182 = true
    default:
        jp182 = false
    }
    retv180 = jp182
    return retv180
}

func report(label__6 string, value__7 bool) string {
    var retv186 string
    var t187 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__7)
    var t188 string = label__6 + t187
    retv186 = t188
    return retv186
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t190 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t190)
    var t191 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t191)
    var t192 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t192)
    var t193 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t193)
    var t194 bool = is_special8(5)
    var part1__14 string = report("int8=", t194)
    var t195 bool = is_special16(1024)
    var part2__15 string = report(",int16=", t195)
    var t196 bool = is_special32(8192)
    var part3__16 string = report(",int32=", t196)
    var t197 bool = is_special64(16384)
    var part4__17 string = report(",int64_a=", t197)
    var t198 bool = is_special64(32768)
    var part5__18 string = report(",int64_b=", t198)
    var part6__19 string = report(",tuple_hit=", tuple_result_hit__10)
    var part7__20 string = report(",tuple_miss=", tuple_result_miss__11)
    var part8__21 string = report(",struct_first=", pair_first__12)
    var part9__22 string = report(",struct_second=", pair_second__13)
    var t199 string = part1__14 + part2__15
    var t200 string = t199 + part3__16
    var t201 string = t200 + part4__17
    var t202 string = t201 + part5__18
    var t203 string = t202 + part6__19
    var t204 string = t203 + part7__20
    var t205 string = t204 + part8__21
    var message__23 string = t205 + part9__22
    println__T_string(message__23)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv207 string
    var t208 string = _goml_runtime_core_bool_to_string(self__37)
    retv207 = t208
    return retv207
}

func println__T_string(value__1 string) struct{} {
    var t210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv213 string
    retv213 = self__38
    return retv213
}

func main() {
    main0()
}
