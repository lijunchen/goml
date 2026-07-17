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
    var retv64 bool
    var jp66 bool
    switch value__0 {
    case 5:
        jp66 = true
    case 7:
        jp66 = true
    default:
        jp66 = false
    }
    retv64 = jp66
    return retv64
}

func is_special16(value__1 int16) bool {
    var retv68 bool
    var jp70 bool
    switch value__1 {
    case 1024:
        jp70 = true
    case 2048:
        jp70 = true
    default:
        jp70 = false
    }
    retv68 = jp70
    return retv68
}

func is_special32(value__2 int32) bool {
    var retv72 bool
    var jp74 bool
    switch value__2 {
    case 4096:
        jp74 = true
    case 8192:
        jp74 = true
    default:
        jp74 = false
    }
    retv72 = jp74
    return retv72
}

func is_special64(value__3 int64) bool {
    var retv76 bool
    var jp78 bool
    switch value__3 {
    case 16384:
        jp78 = true
    case 32768:
        jp78 = true
    default:
        jp78 = false
    }
    retv76 = jp78
    return retv76
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var retv80 bool
    var x58 int8 = values__4._0
    var x59 int16 = values__4._1
    var jp82 bool
    switch x59 {
    case 2:
        var jp84 bool
        switch x58 {
        case 1:
            jp84 = true
        default:
            jp84 = false
        }
        jp82 = jp84
    default:
        jp82 = false
    }
    retv80 = jp82
    return retv80
}

func match_struct(pair__5 PairData) bool {
    var retv86 bool
    var x60 int32 = pair__5.head
    var x61 int64 = pair__5.tail
    var jp88 bool
    switch x61 {
    case 200:
        var jp90 bool
        switch x60 {
        case 100:
            jp90 = true
        default:
            jp90 = false
        }
        jp88 = jp90
    case 300:
        jp88 = true
    default:
        jp88 = false
    }
    retv86 = jp88
    return retv86
}

func report(label__6 string, value__7 bool) string {
    var retv92 string
    var t93 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__7)
    var t94 string = label__6 + t93
    retv92 = t94
    return retv92
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t96 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t96)
    var t97 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t97)
    var t98 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t98)
    var t99 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t99)
    var t100 bool = is_special8(5)
    var part1__14 string = report("int8=", t100)
    var t101 bool = is_special16(1024)
    var part2__15 string = report(",int16=", t101)
    var t102 bool = is_special32(8192)
    var part3__16 string = report(",int32=", t102)
    var t103 bool = is_special64(16384)
    var part4__17 string = report(",int64_a=", t103)
    var t104 bool = is_special64(32768)
    var part5__18 string = report(",int64_b=", t104)
    var part6__19 string = report(",tuple_hit=", tuple_result_hit__10)
    var part7__20 string = report(",tuple_miss=", tuple_result_miss__11)
    var part8__21 string = report(",struct_first=", pair_first__12)
    var part9__22 string = report(",struct_second=", pair_second__13)
    var t105 string = part1__14 + part2__15
    var t106 string = t105 + part3__16
    var t107 string = t106 + part4__17
    var t108 string = t107 + part5__18
    var t109 string = t108 + part6__19
    var t110 string = t109 + part7__20
    var t111 string = t110 + part8__21
    var message__23 string = t111 + part9__22
    println__T_string(message__23)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv113 string
    var t114 string = _goml_runtime_core_bool_to_string(self__33)
    retv113 = t114
    return retv113
}

func println__T_string(value__1 string) struct{} {
    var t116 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t116)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv119 string
    retv119 = self__34
    return retv119
}

func main() {
    main0()
}
