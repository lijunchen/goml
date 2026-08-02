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
    var retv161 bool
    var jp163 bool
    switch value__0 {
    case 5:
        jp163 = true
    case 7:
        jp163 = true
    default:
        jp163 = false
    }
    retv161 = jp163
    return retv161
}

func is_special16(value__1 int16) bool {
    var retv165 bool
    var jp167 bool
    switch value__1 {
    case 1024:
        jp167 = true
    case 2048:
        jp167 = true
    default:
        jp167 = false
    }
    retv165 = jp167
    return retv165
}

func is_special32(value__2 int32) bool {
    var retv169 bool
    var jp171 bool
    switch value__2 {
    case 4096:
        jp171 = true
    case 8192:
        jp171 = true
    default:
        jp171 = false
    }
    retv169 = jp171
    return retv169
}

func is_special64(value__3 int64) bool {
    var retv173 bool
    var jp175 bool
    switch value__3 {
    case 16384:
        jp175 = true
    case 32768:
        jp175 = true
    default:
        jp175 = false
    }
    retv173 = jp175
    return retv173
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var retv177 bool
    var x155 int8 = values__4._0
    var x156 int16 = values__4._1
    var jp179 bool
    switch x156 {
    case 2:
        var jp181 bool
        switch x155 {
        case 1:
            jp181 = true
        default:
            jp181 = false
        }
        jp179 = jp181
    default:
        jp179 = false
    }
    retv177 = jp179
    return retv177
}

func match_struct(pair__5 PairData) bool {
    var retv183 bool
    var x157 int32 = pair__5.head
    var x158 int64 = pair__5.tail
    var jp185 bool
    switch x158 {
    case 200:
        var jp187 bool
        switch x157 {
        case 100:
            jp187 = true
        default:
            jp187 = false
        }
        jp185 = jp187
    case 300:
        jp185 = true
    default:
        jp185 = false
    }
    retv183 = jp185
    return retv183
}

func report(label__6 string, value__7 bool) string {
    var retv189 string
    var t190 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__7)
    var t191 string = label__6 + t190
    retv189 = t191
    return retv189
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
    var part1__14 string = report("int8=", t197)
    var t198 bool = is_special16(1024)
    var part2__15 string = report(",int16=", t198)
    var t199 bool = is_special32(8192)
    var part3__16 string = report(",int32=", t199)
    var t200 bool = is_special64(16384)
    var part4__17 string = report(",int64_a=", t200)
    var t201 bool = is_special64(32768)
    var part5__18 string = report(",int64_b=", t201)
    var part6__19 string = report(",tuple_hit=", tuple_result_hit__10)
    var part7__20 string = report(",tuple_miss=", tuple_result_miss__11)
    var part8__21 string = report(",struct_first=", pair_first__12)
    var part9__22 string = report(",struct_second=", pair_second__13)
    var t202 string = part1__14 + part2__15
    var t203 string = t202 + part3__16
    var t204 string = t203 + part4__17
    var t205 string = t204 + part5__18
    var t206 string = t205 + part6__19
    var t207 string = t206 + part7__20
    var t208 string = t207 + part8__21
    var message__23 string = t208 + part9__22
    println__T_string(message__23)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv210 string
    var t211 string = _goml_runtime_core_bool_to_string(self__37)
    retv210 = t211
    return retv210
}

func println__T_string(value__1 string) struct{} {
    var t213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t213)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv216 string
    retv216 = self__38
    return retv216
}

func main() {
    main0()
}
