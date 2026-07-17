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
    var retv67 bool
    var jp69 bool
    switch value__0 {
    case 5:
        jp69 = true
    case 7:
        jp69 = true
    default:
        jp69 = false
    }
    retv67 = jp69
    return retv67
}

func is_special16(value__1 int16) bool {
    var retv71 bool
    var jp73 bool
    switch value__1 {
    case 1024:
        jp73 = true
    case 2048:
        jp73 = true
    default:
        jp73 = false
    }
    retv71 = jp73
    return retv71
}

func is_special32(value__2 int32) bool {
    var retv75 bool
    var jp77 bool
    switch value__2 {
    case 4096:
        jp77 = true
    case 8192:
        jp77 = true
    default:
        jp77 = false
    }
    retv75 = jp77
    return retv75
}

func is_special64(value__3 int64) bool {
    var retv79 bool
    var jp81 bool
    switch value__3 {
    case 16384:
        jp81 = true
    case 32768:
        jp81 = true
    default:
        jp81 = false
    }
    retv79 = jp81
    return retv79
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var retv83 bool
    var x61 int8 = values__4._0
    var x62 int16 = values__4._1
    var jp85 bool
    switch x62 {
    case 2:
        var jp87 bool
        switch x61 {
        case 1:
            jp87 = true
        default:
            jp87 = false
        }
        jp85 = jp87
    default:
        jp85 = false
    }
    retv83 = jp85
    return retv83
}

func match_struct(pair__5 PairData) bool {
    var retv89 bool
    var x63 int32 = pair__5.head
    var x64 int64 = pair__5.tail
    var jp91 bool
    switch x64 {
    case 200:
        var jp93 bool
        switch x63 {
        case 100:
            jp93 = true
        default:
            jp93 = false
        }
        jp91 = jp93
    case 300:
        jp91 = true
    default:
        jp91 = false
    }
    retv89 = jp91
    return retv89
}

func report(label__6 string, value__7 bool) string {
    var retv95 string
    var t96 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__7)
    var t97 string = label__6 + t96
    retv95 = t97
    return retv95
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t99 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t99)
    var t100 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t100)
    var t101 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t101)
    var t102 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t102)
    var t103 bool = is_special8(5)
    var part1__14 string = report("int8=", t103)
    var t104 bool = is_special16(1024)
    var part2__15 string = report(",int16=", t104)
    var t105 bool = is_special32(8192)
    var part3__16 string = report(",int32=", t105)
    var t106 bool = is_special64(16384)
    var part4__17 string = report(",int64_a=", t106)
    var t107 bool = is_special64(32768)
    var part5__18 string = report(",int64_b=", t107)
    var part6__19 string = report(",tuple_hit=", tuple_result_hit__10)
    var part7__20 string = report(",tuple_miss=", tuple_result_miss__11)
    var part8__21 string = report(",struct_first=", pair_first__12)
    var part9__22 string = report(",struct_second=", pair_second__13)
    var t108 string = part1__14 + part2__15
    var t109 string = t108 + part3__16
    var t110 string = t109 + part4__17
    var t111 string = t110 + part5__18
    var t112 string = t111 + part6__19
    var t113 string = t112 + part7__20
    var t114 string = t113 + part8__21
    var message__23 string = t114 + part9__22
    println__T_string(message__23)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv116 string
    var t117 string = _goml_runtime_core_bool_to_string(self__36)
    retv116 = t117
    return retv116
}

func println__T_string(value__1 string) struct{} {
    var t119 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t119)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv122 string
    retv122 = self__37
    return retv122
}

func main() {
    main0()
}
