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
    var retv114 bool
    var jp116 bool
    switch value__0 {
    case 5:
        jp116 = true
    case 7:
        jp116 = true
    default:
        jp116 = false
    }
    retv114 = jp116
    return retv114
}

func is_special16(value__1 int16) bool {
    var retv118 bool
    var jp120 bool
    switch value__1 {
    case 1024:
        jp120 = true
    case 2048:
        jp120 = true
    default:
        jp120 = false
    }
    retv118 = jp120
    return retv118
}

func is_special32(value__2 int32) bool {
    var retv122 bool
    var jp124 bool
    switch value__2 {
    case 4096:
        jp124 = true
    case 8192:
        jp124 = true
    default:
        jp124 = false
    }
    retv122 = jp124
    return retv122
}

func is_special64(value__3 int64) bool {
    var retv126 bool
    var jp128 bool
    switch value__3 {
    case 16384:
        jp128 = true
    case 32768:
        jp128 = true
    default:
        jp128 = false
    }
    retv126 = jp128
    return retv126
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var retv130 bool
    var x108 int8 = values__4._0
    var x109 int16 = values__4._1
    var jp132 bool
    switch x109 {
    case 2:
        var jp134 bool
        switch x108 {
        case 1:
            jp134 = true
        default:
            jp134 = false
        }
        jp132 = jp134
    default:
        jp132 = false
    }
    retv130 = jp132
    return retv130
}

func match_struct(pair__5 PairData) bool {
    var retv136 bool
    var x110 int32 = pair__5.head
    var x111 int64 = pair__5.tail
    var jp138 bool
    switch x111 {
    case 200:
        var jp140 bool
        switch x110 {
        case 100:
            jp140 = true
        default:
            jp140 = false
        }
        jp138 = jp140
    case 300:
        jp138 = true
    default:
        jp138 = false
    }
    retv136 = jp138
    return retv136
}

func report(label__6 string, value__7 bool) string {
    var retv142 string
    var t143 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__7)
    var t144 string = label__6 + t143
    retv142 = t144
    return retv142
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t146 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t146)
    var t147 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t147)
    var t148 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t148)
    var t149 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t149)
    var t150 bool = is_special8(5)
    var part1__14 string = report("int8=", t150)
    var t151 bool = is_special16(1024)
    var part2__15 string = report(",int16=", t151)
    var t152 bool = is_special32(8192)
    var part3__16 string = report(",int32=", t152)
    var t153 bool = is_special64(16384)
    var part4__17 string = report(",int64_a=", t153)
    var t154 bool = is_special64(32768)
    var part5__18 string = report(",int64_b=", t154)
    var part6__19 string = report(",tuple_hit=", tuple_result_hit__10)
    var part7__20 string = report(",tuple_miss=", tuple_result_miss__11)
    var part8__21 string = report(",struct_first=", pair_first__12)
    var part9__22 string = report(",struct_second=", pair_second__13)
    var t155 string = part1__14 + part2__15
    var t156 string = t155 + part3__16
    var t157 string = t156 + part4__17
    var t158 string = t157 + part5__18
    var t159 string = t158 + part6__19
    var t160 string = t159 + part7__20
    var t161 string = t160 + part8__21
    var message__23 string = t161 + part9__22
    println__T_string(message__23)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv163 string
    var t164 string = _goml_runtime_core_bool_to_string(self__37)
    retv163 = t164
    return retv163
}

func println__T_string(value__1 string) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv169 string
    retv169 = self__38
    return retv169
}

func main() {
    main0()
}
