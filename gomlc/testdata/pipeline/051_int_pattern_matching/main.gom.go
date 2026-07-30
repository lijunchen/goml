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
    var retv74 bool
    var jp76 bool
    switch value__0 {
    case 5:
        jp76 = true
    case 7:
        jp76 = true
    default:
        jp76 = false
    }
    retv74 = jp76
    return retv74
}

func is_special16(value__1 int16) bool {
    var retv78 bool
    var jp80 bool
    switch value__1 {
    case 1024:
        jp80 = true
    case 2048:
        jp80 = true
    default:
        jp80 = false
    }
    retv78 = jp80
    return retv78
}

func is_special32(value__2 int32) bool {
    var retv82 bool
    var jp84 bool
    switch value__2 {
    case 4096:
        jp84 = true
    case 8192:
        jp84 = true
    default:
        jp84 = false
    }
    retv82 = jp84
    return retv82
}

func is_special64(value__3 int64) bool {
    var retv86 bool
    var jp88 bool
    switch value__3 {
    case 16384:
        jp88 = true
    case 32768:
        jp88 = true
    default:
        jp88 = false
    }
    retv86 = jp88
    return retv86
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var retv90 bool
    var x68 int8 = values__4._0
    var x69 int16 = values__4._1
    var jp92 bool
    switch x69 {
    case 2:
        var jp94 bool
        switch x68 {
        case 1:
            jp94 = true
        default:
            jp94 = false
        }
        jp92 = jp94
    default:
        jp92 = false
    }
    retv90 = jp92
    return retv90
}

func match_struct(pair__5 PairData) bool {
    var retv96 bool
    var x70 int32 = pair__5.head
    var x71 int64 = pair__5.tail
    var jp98 bool
    switch x71 {
    case 200:
        var jp100 bool
        switch x70 {
        case 100:
            jp100 = true
        default:
            jp100 = false
        }
        jp98 = jp100
    case 300:
        jp98 = true
    default:
        jp98 = false
    }
    retv96 = jp98
    return retv96
}

func report(label__6 string, value__7 bool) string {
    var retv102 string
    var t103 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__7)
    var t104 string = label__6 + t103
    retv102 = t104
    return retv102
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t106 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t106)
    var t107 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t107)
    var t108 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t108)
    var t109 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t109)
    var t110 bool = is_special8(5)
    var part1__14 string = report("int8=", t110)
    var t111 bool = is_special16(1024)
    var part2__15 string = report(",int16=", t111)
    var t112 bool = is_special32(8192)
    var part3__16 string = report(",int32=", t112)
    var t113 bool = is_special64(16384)
    var part4__17 string = report(",int64_a=", t113)
    var t114 bool = is_special64(32768)
    var part5__18 string = report(",int64_b=", t114)
    var part6__19 string = report(",tuple_hit=", tuple_result_hit__10)
    var part7__20 string = report(",tuple_miss=", tuple_result_miss__11)
    var part8__21 string = report(",struct_first=", pair_first__12)
    var part9__22 string = report(",struct_second=", pair_second__13)
    var t115 string = part1__14 + part2__15
    var t116 string = t115 + part3__16
    var t117 string = t116 + part4__17
    var t118 string = t117 + part5__18
    var t119 string = t118 + part6__19
    var t120 string = t119 + part7__20
    var t121 string = t120 + part8__21
    var message__23 string = t121 + part9__22
    println__T_string(message__23)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv123 string
    var t124 string = _goml_runtime_core_bool_to_string(self__37)
    retv123 = t124
    return retv123
}

func println__T_string(value__1 string) struct{} {
    var t126 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv129 string
    retv129 = self__38
    return retv129
}

func main() {
    main0()
}
