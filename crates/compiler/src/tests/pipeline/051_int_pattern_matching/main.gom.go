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
    var retv13 bool
    var jp15 bool
    switch value__0 {
    case 5:
        jp15 = true
    case 7:
        jp15 = true
    default:
        jp15 = false
    }
    retv13 = jp15
    return retv13
}

func is_special16(value__1 int16) bool {
    var retv17 bool
    var jp19 bool
    switch value__1 {
    case 1024:
        jp19 = true
    case 2048:
        jp19 = true
    default:
        jp19 = false
    }
    retv17 = jp19
    return retv17
}

func is_special32(value__2 int32) bool {
    var retv21 bool
    var jp23 bool
    switch value__2 {
    case 4096:
        jp23 = true
    case 8192:
        jp23 = true
    default:
        jp23 = false
    }
    retv21 = jp23
    return retv21
}

func is_special64(value__3 int64) bool {
    var retv25 bool
    var jp27 bool
    switch value__3 {
    case 16384:
        jp27 = true
    case 32768:
        jp27 = true
    default:
        jp27 = false
    }
    retv25 = jp27
    return retv25
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var retv29 bool
    var x7 int8 = values__4._0
    var x8 int16 = values__4._1
    var jp31 bool
    switch x8 {
    case 2:
        var jp33 bool
        switch x7 {
        case 1:
            jp33 = true
        default:
            jp33 = false
        }
        jp31 = jp33
    default:
        jp31 = false
    }
    retv29 = jp31
    return retv29
}

func match_struct(pair__5 PairData) bool {
    var retv35 bool
    var x9 int32 = pair__5.head
    var x10 int64 = pair__5.tail
    var jp37 bool
    switch x10 {
    case 200:
        var jp39 bool
        switch x9 {
        case 100:
            jp39 = true
        default:
            jp39 = false
        }
        jp37 = jp39
    case 300:
        jp37 = true
    default:
        jp37 = false
    }
    retv35 = jp37
    return retv35
}

func report(label__6 string, value__7 bool) string {
    var retv41 string
    var t42 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__7)
    var t43 string = label__6 + t42
    retv41 = t43
    return retv41
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t45 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t45)
    var t46 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t46)
    var t47 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t47)
    var t48 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t48)
    var t49 bool = is_special8(5)
    var part1__14 string = report("int8=", t49)
    var t50 bool = is_special16(1024)
    var part2__15 string = report(",int16=", t50)
    var t51 bool = is_special32(8192)
    var part3__16 string = report(",int32=", t51)
    var t52 bool = is_special64(16384)
    var part4__17 string = report(",int64_a=", t52)
    var t53 bool = is_special64(32768)
    var part5__18 string = report(",int64_b=", t53)
    var part6__19 string = report(",tuple_hit=", tuple_result_hit__10)
    var part7__20 string = report(",tuple_miss=", tuple_result_miss__11)
    var part8__21 string = report(",struct_first=", pair_first__12)
    var part9__22 string = report(",struct_second=", pair_second__13)
    var t54 string = part1__14 + part2__15
    var t55 string = t54 + part3__16
    var t56 string = t55 + part4__17
    var t57 string = t56 + part5__18
    var t58 string = t57 + part6__19
    var t59 string = t58 + part7__20
    var t60 string = t59 + part8__21
    var message__23 string = t60 + part9__22
    println__T_string(message__23)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv62 string
    var t63 string = _goml_runtime_core_bool_to_string(self__8)
    retv62 = t63
    return retv62
}

func println__T_string(value__1 string) struct{} {
    var t65 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t65)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv68 string
    retv68 = self__9
    return retv68
}

func main() {
    main0()
}
