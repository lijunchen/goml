package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
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
    var retv17 Result__int32__string
    var jp19 Result__int32__string
    if ok__0 {
        var t20 Result__int32__string = Ok{
            _0: 10,
        }
        jp19 = t20
    } else {
        var t21 Result__int32__string = Err{
            _0: "left failed",
        }
        jp19 = t21
    }
    retv17 = jp19
    return retv17
}

func read_right(ok__1 bool) Result__int32__string {
    var retv23 Result__int32__string
    var jp25 Result__int32__string
    if ok__1 {
        var t26 Result__int32__string = Ok{
            _0: 20,
        }
        jp25 = t26
    } else {
        var t27 Result__int32__string = Err{
            _0: "right failed",
        }
        jp25 = t27
    }
    retv23 = jp25
    return retv23
}

func choose(choice__2 Choice) Result__int32__string {
    var retv29 Result__int32__string
    var jp31 int32
    switch choice__2.(type) {
    case Left:
        var x0 bool = choice__2.(Left)._0
        var ok__3 bool = x0
        var mtmp3 Result__int32__string = read_left(ok__3)
        var jp34 int32
        switch mtmp3.(type) {
        case Ok:
            var x4 int32 = mtmp3.(Ok)._0
            var try_value__21 int32 = x4
            jp34 = try_value__21
            jp31 = jp34
            var value__6 int32 = jp31
            var t32 Result__int32__string = Ok{
                _0: value__6,
            }
            retv29 = t32
            return retv29
        case Err:
            var x5 string = mtmp3.(Err)._0
            var try_residual__21 string = x5
            var t35 Result__int32__string = Err{
                _0: try_residual__21,
            }
            retv29 = t35
            return retv29
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x1 bool = choice__2.(Right)._0
        var ok__4 bool = x1
        var mtmp6 Result__int32__string = read_right(ok__4)
        var jp37 int32
        switch mtmp6.(type) {
        case Ok:
            var x7 int32 = mtmp6.(Ok)._0
            var try_value__25 int32 = x7
            jp37 = try_value__25
            var t38 int32 = jp37 + 1
            jp31 = t38
            var value__6 int32 = jp31
            var t32 Result__int32__string = Ok{
                _0: value__6,
            }
            retv29 = t32
            return retv29
        case Err:
            var x8 string = mtmp6.(Err)._0
            var try_residual__25 string = x8
            var t39 Result__int32__string = Err{
                _0: try_residual__25,
            }
            retv29 = t39
            return retv29
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x2 int32 = choice__2.(Keep)._0
        var value__5 int32 = x2
        jp31 = value__5
        var value__6 int32 = jp31
        var t32 Result__int32__string = Ok{
            _0: value__6,
        }
        retv29 = t32
        return retv29
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    var retv41 string
    var jp43 string
    switch res__7.(type) {
    case Ok:
        var x9 int32 = res__7.(Ok)._0
        var value__8 int32 = x9
        var t44 string = int32_to_string(value__8)
        var t45 string = "ok " + t44
        jp43 = t45
    case Err:
        var x10 string = res__7.(Err)._0
        var err__9 string = x10
        var t46 string = "err " + err__9
        jp43 = t46
    default:
        panic("non-exhaustive match")
    }
    retv41 = jp43
    return retv41
}

func main0() struct{} {
    var t48 Choice = Left{
        _0: true,
    }
    var t49 Result__int32__string = choose(t48)
    var t50 string = show(t49)
    println__T_string(t50)
    var t51 Choice = Right{
        _0: true,
    }
    var t52 Result__int32__string = choose(t51)
    var t53 string = show(t52)
    println__T_string(t53)
    var t54 Choice = Keep{
        _0: 5,
    }
    var t55 Result__int32__string = choose(t54)
    var t56 string = show(t55)
    println__T_string(t56)
    var t57 Choice = Left{
        _0: false,
    }
    var t58 Result__int32__string = choose(t57)
    var t59 string = show(t58)
    println__T_string(t59)
    var t60 Choice = Right{
        _0: false,
    }
    var t61 Result__int32__string = choose(t60)
    var t62 string = show(t61)
    println__T_string(t62)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
