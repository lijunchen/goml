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
    var retv28 bool
    var jp30 bool
    switch value__0 {
    case 5:
        jp30 = true
    case 7:
        jp30 = true
    default:
        jp30 = false
    }
    retv28 = jp30
    return retv28
}

func is_special16(value__1 int16) bool {
    var retv32 bool
    var jp34 bool
    switch value__1 {
    case 1024:
        jp34 = true
    case 2048:
        jp34 = true
    default:
        jp34 = false
    }
    retv32 = jp34
    return retv32
}

func is_special32(value__2 int32) bool {
    var retv36 bool
    var jp38 bool
    switch value__2 {
    case 4096:
        jp38 = true
    case 8192:
        jp38 = true
    default:
        jp38 = false
    }
    retv36 = jp38
    return retv36
}

func is_special64(value__3 int64) bool {
    var retv40 bool
    var jp42 bool
    switch value__3 {
    case 16384:
        jp42 = true
    case 32768:
        jp42 = true
    default:
        jp42 = false
    }
    retv40 = jp42
    return retv40
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var retv44 bool
    var x22 int8 = values__4._0
    var x23 int16 = values__4._1
    var jp46 bool
    switch x23 {
    case 2:
        var jp48 bool
        switch x22 {
        case 1:
            jp48 = true
        default:
            jp48 = false
        }
        jp46 = jp48
    default:
        jp46 = false
    }
    retv44 = jp46
    return retv44
}

func match_struct(pair__5 PairData) bool {
    var retv50 bool
    var x24 int32 = pair__5.head
    var x25 int64 = pair__5.tail
    var jp52 bool
    switch x25 {
    case 200:
        var jp54 bool
        switch x24 {
        case 100:
            jp54 = true
        default:
            jp54 = false
        }
        jp52 = jp54
    case 300:
        jp52 = true
    default:
        jp52 = false
    }
    retv50 = jp52
    return retv50
}

func report(label__6 string, value__7 bool) string {
    var retv56 string
    var t57 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__7)
    var t58 string = label__6 + t57
    retv56 = t58
    return retv56
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t60 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t60)
    var t61 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t61)
    var t62 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t62)
    var t63 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t63)
    var t64 bool = is_special8(5)
    var part1__14 string = report("int8=", t64)
    var t65 bool = is_special16(1024)
    var part2__15 string = report(",int16=", t65)
    var t66 bool = is_special32(8192)
    var part3__16 string = report(",int32=", t66)
    var t67 bool = is_special64(16384)
    var part4__17 string = report(",int64_a=", t67)
    var t68 bool = is_special64(32768)
    var part5__18 string = report(",int64_b=", t68)
    var part6__19 string = report(",tuple_hit=", tuple_result_hit__10)
    var part7__20 string = report(",tuple_miss=", tuple_result_miss__11)
    var part8__21 string = report(",struct_first=", pair_first__12)
    var part9__22 string = report(",struct_second=", pair_second__13)
    var t69 string = part1__14 + part2__15
    var t70 string = t69 + part3__16
    var t71 string = t70 + part4__17
    var t72 string = t71 + part5__18
    var t73 string = t72 + part6__19
    var t74 string = t73 + part7__20
    var t75 string = t74 + part8__21
    var message__23 string = t75 + part9__22
    println__T_string(message__23)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv77 string
    var t78 string = _goml_runtime_core_bool_to_string(self__8)
    retv77 = t78
    return retv77
}

func println__T_string(value__1 string) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t80)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv83 string
    retv83 = self__9
    return retv83
}

func main() {
    main0()
}
