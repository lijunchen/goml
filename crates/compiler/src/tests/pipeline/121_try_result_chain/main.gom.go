package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Result__string__string interface {
    isResult__string__string()
}

type Ok struct {
    _0 string
}

func (_ Ok) isResult__string__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__string__string() {}

func parse_text(ok__0 bool) Result__string__string {
    var retv11 Result__string__string
    var jp13 Result__string__string
    if ok__0 {
        var t14 Result__string__string = Ok{
            _0: "goml",
        }
        jp13 = t14
    } else {
        var t15 Result__string__string = Err{
            _0: "parse failed",
        }
        jp13 = t15
    }
    retv11 = jp13
    return retv11
}

func normalize_text(ok__1 bool) Result__string__string {
    var retv17 Result__string__string
    var mtmp0 Result__string__string = parse_text(ok__1)
    var jp19 string
    switch mtmp0.(type) {
    case Ok:
        var x1 string = mtmp0.(Ok)._0
        var try_value__12 string = x1
        jp19 = try_value__12
        var text__2 string = jp19
        var t20 string = text__2 + "!"
        var t21 Result__string__string = Ok{
            _0: t20,
        }
        retv17 = t21
        return retv17
    case Err:
        var x2 string = mtmp0.(Err)._0
        var try_residual__12 string = x2
        var t22 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv17 = t22
        return retv17
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var retv24 Result__string__string
    var mtmp3 Result__string__string = normalize_text(ok__3)
    var jp26 string
    switch mtmp3.(type) {
    case Ok:
        var x4 string = mtmp3.(Ok)._0
        var try_value__20 string = x4
        jp26 = try_value__20
        var text__4 string = jp26
        var t27 string = "[" + text__4
        var t28 string = t27 + "]"
        var t29 Result__string__string = Ok{
            _0: t28,
        }
        retv24 = t29
        return retv24
    case Err:
        var x5 string = mtmp3.(Err)._0
        var try_residual__20 string = x5
        var t30 Result__string__string = Err{
            _0: try_residual__20,
        }
        retv24 = t30
        return retv24
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv32 string
    var jp34 string
    switch res__5.(type) {
    case Ok:
        var x6 string = res__5.(Ok)._0
        var value__6 string = x6
        var t35 string = "ok " + value__6
        jp34 = t35
    case Err:
        var x7 string = res__5.(Err)._0
        var err__7 string = x7
        var t36 string = "err " + err__7
        jp34 = t36
    default:
        panic("non-exhaustive match")
    }
    retv32 = jp34
    return retv32
}

func main0() struct{} {
    var t38 Result__string__string = decorate_text(true)
    var t39 string = show(t38)
    println__T_string(t39)
    var t40 Result__string__string = decorate_text(false)
    var t41 string = show(t40)
    println__T_string(t41)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
