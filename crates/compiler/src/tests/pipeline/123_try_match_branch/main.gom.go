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
    var retv39 Result__int32__string
    var jp41 Result__int32__string
    if ok__0 {
        var t42 Result__int32__string = Ok{
            _0: 10,
        }
        jp41 = t42
    } else {
        var t43 Result__int32__string = Err{
            _0: "left failed",
        }
        jp41 = t43
    }
    retv39 = jp41
    return retv39
}

func read_right(ok__1 bool) Result__int32__string {
    var retv45 Result__int32__string
    var jp47 Result__int32__string
    if ok__1 {
        var t48 Result__int32__string = Ok{
            _0: 20,
        }
        jp47 = t48
    } else {
        var t49 Result__int32__string = Err{
            _0: "right failed",
        }
        jp47 = t49
    }
    retv45 = jp47
    return retv45
}

func choose(choice__2 Choice) Result__int32__string {
    var retv51 Result__int32__string
    var jp53 int32
    switch choice__2.(type) {
    case Left:
        var x22 bool = choice__2.(Left)._0
        var ok__3 bool = x22
        var mtmp25 Result__int32__string = read_left(ok__3)
        var jp56 int32
        switch mtmp25.(type) {
        case Ok:
            var x26 int32 = mtmp25.(Ok)._0
            var try_value__21 int32 = x26
            jp56 = try_value__21
            jp53 = jp56
            var value__6 int32 = jp53
            var t54 Result__int32__string = Ok{
                _0: value__6,
            }
            retv51 = t54
            return retv51
        case Err:
            var x27 string = mtmp25.(Err)._0
            var try_residual__21 string = x27
            var t57 Result__int32__string = Err{
                _0: try_residual__21,
            }
            retv51 = t57
            return retv51
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x23 bool = choice__2.(Right)._0
        var ok__4 bool = x23
        var mtmp28 Result__int32__string = read_right(ok__4)
        var jp59 int32
        switch mtmp28.(type) {
        case Ok:
            var x29 int32 = mtmp28.(Ok)._0
            var try_value__25 int32 = x29
            jp59 = try_value__25
            var t60 int32 = jp59 + 1
            jp53 = t60
            var value__6 int32 = jp53
            var t54 Result__int32__string = Ok{
                _0: value__6,
            }
            retv51 = t54
            return retv51
        case Err:
            var x30 string = mtmp28.(Err)._0
            var try_residual__25 string = x30
            var t61 Result__int32__string = Err{
                _0: try_residual__25,
            }
            retv51 = t61
            return retv51
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x24 int32 = choice__2.(Keep)._0
        var value__5 int32 = x24
        jp53 = value__5
        var value__6 int32 = jp53
        var t54 Result__int32__string = Ok{
            _0: value__6,
        }
        retv51 = t54
        return retv51
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    var retv63 string
    var jp65 string
    switch res__7.(type) {
    case Ok:
        var x31 int32 = res__7.(Ok)._0
        var value__8 int32 = x31
        var t66 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t67 string = "ok " + t66
        jp65 = t67
    case Err:
        var x32 string = res__7.(Err)._0
        var err__9 string = x32
        var t68 string = "err " + err__9
        jp65 = t68
    default:
        panic("non-exhaustive match")
    }
    retv63 = jp65
    return retv63
}

func main0() struct{} {
    var t70 Choice = Left{
        _0: true,
    }
    var t71 Result__int32__string = choose(t70)
    var t72 string = show(t71)
    println__T_string(t72)
    var t73 Choice = Right{
        _0: true,
    }
    var t74 Result__int32__string = choose(t73)
    var t75 string = show(t74)
    println__T_string(t75)
    var t76 Choice = Keep{
        _0: 5,
    }
    var t77 Result__int32__string = choose(t76)
    var t78 string = show(t77)
    println__T_string(t78)
    var t79 Choice = Left{
        _0: false,
    }
    var t80 Result__int32__string = choose(t79)
    var t81 string = show(t80)
    println__T_string(t81)
    var t82 Choice = Right{
        _0: false,
    }
    var t83 Result__int32__string = choose(t82)
    var t84 string = show(t83)
    println__T_string(t84)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv86 string
    var t87 string = _goml_runtime_core_int32_to_string(self__2)
    retv86 = t87
    return retv86
}

func println__T_string(value__1 string) struct{} {
    var t89 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t89)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv92 string
    retv92 = self__9
    return retv92
}

func main() {
    main0()
}
