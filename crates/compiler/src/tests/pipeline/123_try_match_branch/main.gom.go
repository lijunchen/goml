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
    var retv21 Result__int32__string
    var jp23 Result__int32__string
    if ok__0 {
        var t24 Result__int32__string = Ok{
            _0: 10,
        }
        jp23 = t24
    } else {
        var t25 Result__int32__string = Err{
            _0: "left failed",
        }
        jp23 = t25
    }
    retv21 = jp23
    return retv21
}

func read_right(ok__1 bool) Result__int32__string {
    var retv27 Result__int32__string
    var jp29 Result__int32__string
    if ok__1 {
        var t30 Result__int32__string = Ok{
            _0: 20,
        }
        jp29 = t30
    } else {
        var t31 Result__int32__string = Err{
            _0: "right failed",
        }
        jp29 = t31
    }
    retv27 = jp29
    return retv27
}

func choose(choice__2 Choice) Result__int32__string {
    var retv33 Result__int32__string
    var jp35 int32
    switch choice__2.(type) {
    case Left:
        var x4 bool = choice__2.(Left)._0
        var ok__3 bool = x4
        var mtmp7 Result__int32__string = read_left(ok__3)
        var jp38 int32
        switch mtmp7.(type) {
        case Ok:
            var x8 int32 = mtmp7.(Ok)._0
            var try_value__21 int32 = x8
            jp38 = try_value__21
            jp35 = jp38
            var value__6 int32 = jp35
            var t36 Result__int32__string = Ok{
                _0: value__6,
            }
            retv33 = t36
            return retv33
        case Err:
            var x9 string = mtmp7.(Err)._0
            var try_residual__21 string = x9
            var t39 Result__int32__string = Err{
                _0: try_residual__21,
            }
            retv33 = t39
            return retv33
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x5 bool = choice__2.(Right)._0
        var ok__4 bool = x5
        var mtmp10 Result__int32__string = read_right(ok__4)
        var jp41 int32
        switch mtmp10.(type) {
        case Ok:
            var x11 int32 = mtmp10.(Ok)._0
            var try_value__25 int32 = x11
            jp41 = try_value__25
            var t42 int32 = jp41 + 1
            jp35 = t42
            var value__6 int32 = jp35
            var t36 Result__int32__string = Ok{
                _0: value__6,
            }
            retv33 = t36
            return retv33
        case Err:
            var x12 string = mtmp10.(Err)._0
            var try_residual__25 string = x12
            var t43 Result__int32__string = Err{
                _0: try_residual__25,
            }
            retv33 = t43
            return retv33
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x6 int32 = choice__2.(Keep)._0
        var value__5 int32 = x6
        jp35 = value__5
        var value__6 int32 = jp35
        var t36 Result__int32__string = Ok{
            _0: value__6,
        }
        retv33 = t36
        return retv33
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    var retv45 string
    var jp47 string
    switch res__7.(type) {
    case Ok:
        var x13 int32 = res__7.(Ok)._0
        var value__8 int32 = x13
        var t48 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t49 string = "ok " + t48
        jp47 = t49
    case Err:
        var x14 string = res__7.(Err)._0
        var err__9 string = x14
        var t50 string = "err " + err__9
        jp47 = t50
    default:
        panic("non-exhaustive match")
    }
    retv45 = jp47
    return retv45
}

func main0() struct{} {
    var t52 Choice = Left{
        _0: true,
    }
    var t53 Result__int32__string = choose(t52)
    var t54 string = show(t53)
    println__T_string(t54)
    var t55 Choice = Right{
        _0: true,
    }
    var t56 Result__int32__string = choose(t55)
    var t57 string = show(t56)
    println__T_string(t57)
    var t58 Choice = Keep{
        _0: 5,
    }
    var t59 Result__int32__string = choose(t58)
    var t60 string = show(t59)
    println__T_string(t60)
    var t61 Choice = Left{
        _0: false,
    }
    var t62 Result__int32__string = choose(t61)
    var t63 string = show(t62)
    println__T_string(t63)
    var t64 Choice = Right{
        _0: false,
    }
    var t65 Result__int32__string = choose(t64)
    var t66 string = show(t65)
    println__T_string(t66)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv68 string
    var t69 string = _goml_runtime_core_int32_to_string(self__2)
    retv68 = t69
    return retv68
}

func println__T_string(value__1 string) struct{} {
    var t71 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t71)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv74 string
    retv74 = self__9
    return retv74
}

func main() {
    main0()
}
