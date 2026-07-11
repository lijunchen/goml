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
    var retv10 bool
    var jp12 bool
    switch value__0 {
    case 5:
        jp12 = true
    case 7:
        jp12 = true
    default:
        jp12 = false
    }
    retv10 = jp12
    return retv10
}

func is_special16(value__1 int16) bool {
    var retv14 bool
    var jp16 bool
    switch value__1 {
    case 1024:
        jp16 = true
    case 2048:
        jp16 = true
    default:
        jp16 = false
    }
    retv14 = jp16
    return retv14
}

func is_special32(value__2 int32) bool {
    var retv18 bool
    var jp20 bool
    switch value__2 {
    case 4096:
        jp20 = true
    case 8192:
        jp20 = true
    default:
        jp20 = false
    }
    retv18 = jp20
    return retv18
}

func is_special64(value__3 int64) bool {
    var retv22 bool
    var jp24 bool
    switch value__3 {
    case 16384:
        jp24 = true
    case 32768:
        jp24 = true
    default:
        jp24 = false
    }
    retv22 = jp24
    return retv22
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var retv26 bool
    var x4 int8 = values__4._0
    var x5 int16 = values__4._1
    var jp28 bool
    switch x5 {
    case 2:
        var jp30 bool
        switch x4 {
        case 1:
            jp30 = true
        default:
            jp30 = false
        }
        jp28 = jp30
    default:
        jp28 = false
    }
    retv26 = jp28
    return retv26
}

func match_struct(pair__5 PairData) bool {
    var retv32 bool
    var x6 int32 = pair__5.head
    var x7 int64 = pair__5.tail
    var jp34 bool
    switch x7 {
    case 200:
        var jp36 bool
        switch x6 {
        case 100:
            jp36 = true
        default:
            jp36 = false
        }
        jp34 = jp36
    case 300:
        jp34 = true
    default:
        jp34 = false
    }
    retv32 = jp34
    return retv32
}

func report(label__6 string, value__7 bool) string {
    var retv38 string
    var t39 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__7)
    var t40 string = label__6 + t39
    retv38 = t40
    return retv38
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t42 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t42)
    var t43 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t43)
    var t44 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t44)
    var t45 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t45)
    var t46 bool = is_special8(5)
    var part1__14 string = report("int8=", t46)
    var t47 bool = is_special16(1024)
    var part2__15 string = report(",int16=", t47)
    var t48 bool = is_special32(8192)
    var part3__16 string = report(",int32=", t48)
    var t49 bool = is_special64(16384)
    var part4__17 string = report(",int64_a=", t49)
    var t50 bool = is_special64(32768)
    var part5__18 string = report(",int64_b=", t50)
    var part6__19 string = report(",tuple_hit=", tuple_result_hit__10)
    var part7__20 string = report(",tuple_miss=", tuple_result_miss__11)
    var part8__21 string = report(",struct_first=", pair_first__12)
    var part9__22 string = report(",struct_second=", pair_second__13)
    var t51 string = part1__14 + part2__15
    var t52 string = t51 + part3__16
    var t53 string = t52 + part4__17
    var t54 string = t53 + part5__18
    var t55 string = t54 + part6__19
    var t56 string = t55 + part7__20
    var t57 string = t56 + part8__21
    var message__23 string = t57 + part9__22
    println__T_string(message__23)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv59 string
    var t60 string = _goml_runtime_core_bool_to_string(self__8)
    retv59 = t60
    return retv59
}

func println__T_string(value__1 string) struct{} {
    var t62 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t62)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv65 string
    retv65 = self__9
    return retv65
}

func main() {
    main0()
}
