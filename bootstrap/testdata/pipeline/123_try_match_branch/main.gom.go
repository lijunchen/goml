package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Choice interface {
    isChoice()
}

type Left struct {
    _0 bool
}

func (_ Left) isChoice() {}

type Right struct {
    _0 bool
}

func (_ Right) isChoice() {}

type Keep struct {
    _0 int32
}

func (_ Keep) isChoice() {}

type Result__int32__string interface {
    isResult__int32__string()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

func read_left(ok__0 bool) Result__int32__string {
    var retv81 Result__int32__string
    var jp83 Result__int32__string
    if ok__0 {
        var t84 Result__int32__string = Ok{
            _0: 10,
        }
        jp83 = t84
    } else {
        var t85 Result__int32__string = Err{
            _0: "left failed",
        }
        jp83 = t85
    }
    retv81 = jp83
    return retv81
}

func read_right(ok__1 bool) Result__int32__string {
    var retv87 Result__int32__string
    var jp89 Result__int32__string
    if ok__1 {
        var t90 Result__int32__string = Ok{
            _0: 20,
        }
        jp89 = t90
    } else {
        var t91 Result__int32__string = Err{
            _0: "right failed",
        }
        jp89 = t91
    }
    retv87 = jp89
    return retv87
}

func choose(choice__2 Choice) Result__int32__string {
    var retv93 Result__int32__string
    var jp95 int32
    switch choice__2.(type) {
    case Left:
        var x64 bool = choice__2.(Left)._0
        var ok__3 bool = x64
        var mtmp67 Result__int32__string = read_left(ok__3)
        var jp98 int32
        switch mtmp67.(type) {
        case Ok:
            var x68 int32 = mtmp67.(Ok)._0
            var try_value__21 int32 = x68
            jp98 = try_value__21
            jp95 = jp98
            var value__6 int32 = jp95
            var t96 Result__int32__string = Ok{
                _0: value__6,
            }
            retv93 = t96
            return retv93
        case Err:
            var x69 string = mtmp67.(Err)._0
            var try_residual__21 string = x69
            var t99 Result__int32__string = Err{
                _0: try_residual__21,
            }
            retv93 = t99
            return retv93
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x65 bool = choice__2.(Right)._0
        var ok__4 bool = x65
        var mtmp70 Result__int32__string = read_right(ok__4)
        var jp101 int32
        switch mtmp70.(type) {
        case Ok:
            var x71 int32 = mtmp70.(Ok)._0
            var try_value__25 int32 = x71
            jp101 = try_value__25
            var t102 int32 = jp101 + 1
            jp95 = t102
            var value__6 int32 = jp95
            var t96 Result__int32__string = Ok{
                _0: value__6,
            }
            retv93 = t96
            return retv93
        case Err:
            var x72 string = mtmp70.(Err)._0
            var try_residual__25 string = x72
            var t103 Result__int32__string = Err{
                _0: try_residual__25,
            }
            retv93 = t103
            return retv93
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x66 int32 = choice__2.(Keep)._0
        var value__5 int32 = x66
        jp95 = value__5
        var value__6 int32 = jp95
        var t96 Result__int32__string = Ok{
            _0: value__6,
        }
        retv93 = t96
        return retv93
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    var retv105 string
    var jp107 string
    switch res__7.(type) {
    case Ok:
        var x73 int32 = res__7.(Ok)._0
        var value__8 int32 = x73
        var t108 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t109 string = "ok " + t108
        jp107 = t109
    case Err:
        var x74 string = res__7.(Err)._0
        var err__9 string = x74
        var t110 string = "err " + err__9
        jp107 = t110
    default:
        panic("non-exhaustive match")
    }
    retv105 = jp107
    return retv105
}

func main0() struct{} {
    var t112 Choice = Left{
        _0: true,
    }
    var t113 Result__int32__string = choose(t112)
    var t114 string = show(t113)
    println__T_string(t114)
    var t115 Choice = Right{
        _0: true,
    }
    var t116 Result__int32__string = choose(t115)
    var t117 string = show(t116)
    println__T_string(t117)
    var t118 Choice = Keep{
        _0: 5,
    }
    var t119 Result__int32__string = choose(t118)
    var t120 string = show(t119)
    println__T_string(t120)
    var t121 Choice = Left{
        _0: false,
    }
    var t122 Result__int32__string = choose(t121)
    var t123 string = show(t122)
    println__T_string(t123)
    var t124 Choice = Right{
        _0: false,
    }
    var t125 Result__int32__string = choose(t124)
    var t126 string = show(t125)
    println__T_string(t126)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv128 string
    var t129 string = _goml_runtime_core_int32_to_string(self__6)
    retv128 = t129
    return retv128
}

func println__T_string(value__1 string) struct{} {
    var t131 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t131)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv134 string
    retv134 = self__38
    return retv134
}

func main() {
    main0()
}
