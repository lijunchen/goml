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
    var retv24 Result__int32__string
    var jp26 Result__int32__string
    if ok__0 {
        var t27 Result__int32__string = Ok{
            _0: 10,
        }
        jp26 = t27
    } else {
        var t28 Result__int32__string = Err{
            _0: "left failed",
        }
        jp26 = t28
    }
    retv24 = jp26
    return retv24
}

func read_right(ok__1 bool) Result__int32__string {
    var retv30 Result__int32__string
    var jp32 Result__int32__string
    if ok__1 {
        var t33 Result__int32__string = Ok{
            _0: 20,
        }
        jp32 = t33
    } else {
        var t34 Result__int32__string = Err{
            _0: "right failed",
        }
        jp32 = t34
    }
    retv30 = jp32
    return retv30
}

func choose(choice__2 Choice) Result__int32__string {
    var retv36 Result__int32__string
    var jp38 int32
    switch choice__2.(type) {
    case Left:
        var x7 bool = choice__2.(Left)._0
        var ok__3 bool = x7
        var mtmp10 Result__int32__string = read_left(ok__3)
        var jp41 int32
        switch mtmp10.(type) {
        case Ok:
            var x11 int32 = mtmp10.(Ok)._0
            var try_value__21 int32 = x11
            jp41 = try_value__21
            jp38 = jp41
            var value__6 int32 = jp38
            var t39 Result__int32__string = Ok{
                _0: value__6,
            }
            retv36 = t39
            return retv36
        case Err:
            var x12 string = mtmp10.(Err)._0
            var try_residual__21 string = x12
            var t42 Result__int32__string = Err{
                _0: try_residual__21,
            }
            retv36 = t42
            return retv36
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x8 bool = choice__2.(Right)._0
        var ok__4 bool = x8
        var mtmp13 Result__int32__string = read_right(ok__4)
        var jp44 int32
        switch mtmp13.(type) {
        case Ok:
            var x14 int32 = mtmp13.(Ok)._0
            var try_value__25 int32 = x14
            jp44 = try_value__25
            var t45 int32 = jp44 + 1
            jp38 = t45
            var value__6 int32 = jp38
            var t39 Result__int32__string = Ok{
                _0: value__6,
            }
            retv36 = t39
            return retv36
        case Err:
            var x15 string = mtmp13.(Err)._0
            var try_residual__25 string = x15
            var t46 Result__int32__string = Err{
                _0: try_residual__25,
            }
            retv36 = t46
            return retv36
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x9 int32 = choice__2.(Keep)._0
        var value__5 int32 = x9
        jp38 = value__5
        var value__6 int32 = jp38
        var t39 Result__int32__string = Ok{
            _0: value__6,
        }
        retv36 = t39
        return retv36
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    var retv48 string
    var jp50 string
    switch res__7.(type) {
    case Ok:
        var x16 int32 = res__7.(Ok)._0
        var value__8 int32 = x16
        var t51 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t52 string = "ok " + t51
        jp50 = t52
    case Err:
        var x17 string = res__7.(Err)._0
        var err__9 string = x17
        var t53 string = "err " + err__9
        jp50 = t53
    default:
        panic("non-exhaustive match")
    }
    retv48 = jp50
    return retv48
}

func main0() struct{} {
    var t55 Choice = Left{
        _0: true,
    }
    var t56 Result__int32__string = choose(t55)
    var t57 string = show(t56)
    println__T_string(t57)
    var t58 Choice = Right{
        _0: true,
    }
    var t59 Result__int32__string = choose(t58)
    var t60 string = show(t59)
    println__T_string(t60)
    var t61 Choice = Keep{
        _0: 5,
    }
    var t62 Result__int32__string = choose(t61)
    var t63 string = show(t62)
    println__T_string(t63)
    var t64 Choice = Left{
        _0: false,
    }
    var t65 Result__int32__string = choose(t64)
    var t66 string = show(t65)
    println__T_string(t66)
    var t67 Choice = Right{
        _0: false,
    }
    var t68 Result__int32__string = choose(t67)
    var t69 string = show(t68)
    println__T_string(t69)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv71 string
    var t72 string = _goml_runtime_core_int32_to_string(self__2)
    retv71 = t72
    return retv71
}

func println__T_string(value__1 string) struct{} {
    var t74 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv77 string
    retv77 = self__9
    return retv77
}

func main() {
    main0()
}
