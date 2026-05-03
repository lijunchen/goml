package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Result__unit__string interface {
    isResult__unit__string()
}

type Result__unit__string_Ok struct {
    _0 struct{}
}

func (_ Result__unit__string_Ok) isResult__unit__string() {}

type Result__unit__string_Err struct {
    _0 string
}

func (_ Result__unit__string_Err) isResult__unit__string() {}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

func configure(ok__0 bool) Result__unit__string {
    var retv13 Result__unit__string
    var jp15 Result__unit__string
    if ok__0 {
        var t16 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp15 = t16
    } else {
        var t17 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        jp15 = t17
    }
    retv13 = jp15
    return retv13
}

func read_duration(ok__1 bool) Result__string__string {
    var retv19 Result__string__string
    var jp21 Result__string__string
    if ok__1 {
        var t22 Result__string__string = Result__string__string_Ok{
            _0: "2s",
        }
        jp21 = t22
    } else {
        var t23 Result__string__string = Result__string__string_Err{
            _0: "duration failed",
        }
        jp21 = t23
    }
    retv19 = jp21
    return retv19
}

func format_duration(value__2 string) string {
    var retv25 string
    var t26 string = "duration=" + value__2
    retv25 = t26
    return retv25
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var retv28 Result__string__string
    var mtmp0 Result__unit__string = configure(config_ok__3)
    switch mtmp0.(type) {
    case Result__unit__string_Ok:
        var mtmp4 Result__string__string = read_duration(read_ok__4)
        var jp31 string
        switch mtmp4.(type) {
        case Result__string__string_Ok:
            var x5 string = mtmp4.(Result__string__string_Ok)._0
            var try_value__27 string = x5
            jp31 = try_value__27
            var value__5 string = jp31
            var t32 string = format_duration(value__5)
            var t33 Result__string__string = Result__string__string_Ok{
                _0: t32,
            }
            retv28 = t33
            return retv28
        case Result__string__string_Err:
            var x6 string = mtmp4.(Result__string__string_Err)._0
            var try_residual__27 string = x6
            var t34 Result__string__string = Result__string__string_Err{
                _0: try_residual__27,
            }
            retv28 = t34
            return retv28
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x2 string = mtmp0.(Result__unit__string_Err)._0
        var try_residual__23 string = x2
        var t35 Result__string__string = Result__string__string_Err{
            _0: try_residual__23,
        }
        retv28 = t35
        return retv28
    default:
        panic("non-exhaustive match")
    }
}

func show(res__6 Result__string__string) string {
    var retv37 string
    var jp39 string
    switch res__6.(type) {
    case Result__string__string_Ok:
        var x7 string = res__6.(Result__string__string_Ok)._0
        var value__7 string = x7
        var t40 string = "ok " + value__7
        jp39 = t40
    case Result__string__string_Err:
        var x8 string = res__6.(Result__string__string_Err)._0
        var err__8 string = x8
        var t41 string = "err " + err__8
        jp39 = t41
    default:
        panic("non-exhaustive match")
    }
    retv37 = jp39
    return retv37
}

func main0() struct{} {
    var t43 Result__string__string = configure_and_format(true, true)
    var t44 string = show(t43)
    println__T_string(t44)
    var t45 Result__string__string = configure_and_format(true, false)
    var t46 string = show(t45)
    println__T_string(t46)
    var t47 Result__string__string = configure_and_format(false, true)
    var t48 string = show(t47)
    println__T_string(t48)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
