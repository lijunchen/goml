package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Tuple2_int8_int16 struct {
    _0 int8
    _1 int16
}

type PairData struct {
    head int32
    tail int64
}

func is_special8(value__0 int8) bool {
    var jp6 bool
    switch value__0 {
    case 5:
        jp6 = true
    case 7:
        jp6 = true
    default:
        jp6 = false
    }
    return jp6
}

func is_special16(value__1 int16) bool {
    var jp8 bool
    switch value__1 {
    case 1024:
        jp8 = true
    case 2048:
        jp8 = true
    default:
        jp8 = false
    }
    return jp8
}

func is_special32(value__2 int32) bool {
    var jp10 bool
    switch value__2 {
    case 4096:
        jp10 = true
    case 8192:
        jp10 = true
    default:
        jp10 = false
    }
    return jp10
}

func is_special64(value__3 int64) bool {
    var jp12 bool
    switch value__3 {
    case 16384:
        jp12 = true
    case 32768:
        jp12 = true
    default:
        jp12 = false
    }
    return jp12
}

func match_tuple(values__4 Tuple2_int8_int16) bool {
    var x0 int8 = values__4._0
    var x1 int16 = values__4._1
    var jp14 bool
    switch x1 {
    case 2:
        var jp16 bool
        switch x0 {
        case 1:
            jp16 = true
        default:
            jp16 = false
        }
        jp14 = jp16
        return jp14
    default:
        jp14 = false
        return jp14
    }
}

func match_struct(pair__5 PairData) bool {
    var x2 int32 = pair__5.head
    var x3 int64 = pair__5.tail
    var jp18 bool
    switch x3 {
    case 200:
        var jp20 bool
        switch x2 {
        case 100:
            jp20 = true
        default:
            jp20 = false
        }
        jp18 = jp20
        return jp18
    case 300:
        jp18 = true
        return jp18
    default:
        jp18 = false
        return jp18
    }
}

func report(label__6 string, value__7 bool) string {
    var t21 string = bool_to_string(value__7)
    var t22 string = label__6 + t21
    return t22
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t23 Tuple2_int8_int16 = Tuple2_int8_int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t23)
    var t24 Tuple2_int8_int16 = Tuple2_int8_int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t24)
    var t25 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t25)
    var t26 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t26)
    var t27 bool = is_special8(5)
    var part1__14 string = report("int8=", t27)
    var t28 bool = is_special16(1024)
    var part2__15 string = report(",int16=", t28)
    var t29 bool = is_special32(8192)
    var part3__16 string = report(",int32=", t29)
    var t30 bool = is_special64(16384)
    var part4__17 string = report(",int64_a=", t30)
    var t31 bool = is_special64(32768)
    var part5__18 string = report(",int64_b=", t31)
    var part6__19 string = report(",tuple_hit=", tuple_result_hit__10)
    var part7__20 string = report(",tuple_miss=", tuple_result_miss__11)
    var part8__21 string = report(",struct_first=", pair_first__12)
    var part9__22 string = report(",struct_second=", pair_second__13)
    var t32 string = part1__14 + part2__15
    var t33 string = t32 + part3__16
    var t34 string = t33 + part4__17
    var t35 string = t34 + part5__18
    var t36 string = t35 + part6__19
    var t37 string = t36 + part7__20
    var t38 string = t37 + part8__21
    var message__23 string = t38 + part9__22
    string_println(message__23)
    return struct{}{}
}

func main() {
    main0()
}
