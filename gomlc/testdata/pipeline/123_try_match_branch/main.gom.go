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
    var retv85 Result__int32__string
    var jp87 Result__int32__string
    if ok__0 {
        var t88 Result__int32__string = Ok{
            _0: 10,
        }
        jp87 = t88
    } else {
        var t89 Result__int32__string = Err{
            _0: "left failed",
        }
        jp87 = t89
    }
    retv85 = jp87
    return retv85
}

func read_right(ok__1 bool) Result__int32__string {
    var retv91 Result__int32__string
    var jp93 Result__int32__string
    if ok__1 {
        var t94 Result__int32__string = Ok{
            _0: 20,
        }
        jp93 = t94
    } else {
        var t95 Result__int32__string = Err{
            _0: "right failed",
        }
        jp93 = t95
    }
    retv91 = jp93
    return retv91
}

func choose(choice__2 Choice) Result__int32__string {
    var retv97 Result__int32__string
    var jp99 int32
    switch choice__2.(type) {
    case Left:
        var x68 bool = choice__2.(Left)._0
        var ok__3 bool = x68
        var mtmp71 Result__int32__string = read_left(ok__3)
        var jp102 int32
        switch mtmp71.(type) {
        case Ok:
            var x72 int32 = mtmp71.(Ok)._0
            var try_value__21 int32 = x72
            jp102 = try_value__21
            jp99 = jp102
            var value__6 int32 = jp99
            var t100 Result__int32__string = Ok{
                _0: value__6,
            }
            retv97 = t100
            return retv97
        case Err:
            var x73 string = mtmp71.(Err)._0
            var try_residual__21 string = x73
            var t103 Result__int32__string = Err{
                _0: try_residual__21,
            }
            retv97 = t103
            return retv97
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x69 bool = choice__2.(Right)._0
        var ok__4 bool = x69
        var mtmp74 Result__int32__string = read_right(ok__4)
        var jp105 int32
        switch mtmp74.(type) {
        case Ok:
            var x75 int32 = mtmp74.(Ok)._0
            var try_value__25 int32 = x75
            jp105 = try_value__25
            var t106 int32 = jp105 + 1
            jp99 = t106
            var value__6 int32 = jp99
            var t100 Result__int32__string = Ok{
                _0: value__6,
            }
            retv97 = t100
            return retv97
        case Err:
            var x76 string = mtmp74.(Err)._0
            var try_residual__25 string = x76
            var t107 Result__int32__string = Err{
                _0: try_residual__25,
            }
            retv97 = t107
            return retv97
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x70 int32 = choice__2.(Keep)._0
        var value__5 int32 = x70
        jp99 = value__5
        var value__6 int32 = jp99
        var t100 Result__int32__string = Ok{
            _0: value__6,
        }
        retv97 = t100
        return retv97
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    var retv109 string
    var jp111 string
    switch res__7.(type) {
    case Ok:
        var x77 int32 = res__7.(Ok)._0
        var value__8 int32 = x77
        var t112 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t113 string = "ok " + t112
        jp111 = t113
    case Err:
        var x78 string = res__7.(Err)._0
        var err__9 string = x78
        var t114 string = "err " + err__9
        jp111 = t114
    default:
        panic("non-exhaustive match")
    }
    retv109 = jp111
    return retv109
}

func main0() struct{} {
    var t116 Choice = Left{
        _0: true,
    }
    var t117 Result__int32__string = choose(t116)
    var t118 string = show(t117)
    println__T_string(t118)
    var t119 Choice = Right{
        _0: true,
    }
    var t120 Result__int32__string = choose(t119)
    var t121 string = show(t120)
    println__T_string(t121)
    var t122 Choice = Keep{
        _0: 5,
    }
    var t123 Result__int32__string = choose(t122)
    var t124 string = show(t123)
    println__T_string(t124)
    var t125 Choice = Left{
        _0: false,
    }
    var t126 Result__int32__string = choose(t125)
    var t127 string = show(t126)
    println__T_string(t127)
    var t128 Choice = Right{
        _0: false,
    }
    var t129 Result__int32__string = choose(t128)
    var t130 string = show(t129)
    println__T_string(t130)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv132 string
    var t133 string = _goml_runtime_core_int32_to_string(self__6)
    retv132 = t133
    return retv132
}

func println__T_string(value__1 string) struct{} {
    var t135 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t135)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv138 string
    retv138 = self__38
    return retv138
}

func main() {
    main0()
}
