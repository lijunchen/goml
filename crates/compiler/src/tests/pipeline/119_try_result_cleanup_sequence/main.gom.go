package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Handle struct {
    name string
}

type Result__Handle__string interface {
    isResult__Handle__string()
}

type Result__Handle__string_Ok struct {
    _0 Handle
}

func (_ Result__Handle__string_Ok) isResult__Handle__string() {}

type Result__Handle__string_Err struct {
    _0 string
}

func (_ Result__Handle__string_Err) isResult__Handle__string() {}

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

func open_handle(ok__0 bool) Result__Handle__string {
    var retv13 Result__Handle__string
    var jp15 Result__Handle__string
    if ok__0 {
        var t16 Handle = Handle{
            name: "config",
        }
        var t17 Result__Handle__string = Result__Handle__string_Ok{
            _0: t16,
        }
        jp15 = t17
    } else {
        var t18 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        jp15 = t18
    }
    retv13 = jp15
    return retv13
}

func close_handle(handle__1 Handle, ok__2 bool) Result__unit__string {
    var retv20 Result__unit__string
    var jp22 Result__unit__string
    if ok__2 {
        var t23 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp22 = t23
    } else {
        var t24 string = handle__1.name
        var t25 string = "close failed for " + t24
        var t26 Result__unit__string = Result__unit__string_Err{
            _0: t25,
        }
        jp22 = t26
    }
    retv20 = jp22
    return retv20
}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var retv28 Result__string__string
    var mtmp0 Result__Handle__string = open_handle(open_ok__3)
    var jp30 Handle
    switch mtmp0.(type) {
    case Result__Handle__string_Ok:
        var x1 Handle = mtmp0.(Result__Handle__string_Ok)._0
        var try_value__24 Handle = x1
        jp30 = try_value__24
        var handle__5 Handle = jp30
        var name__6 string = handle__5.name
        var mtmp3 Result__unit__string = close_handle(handle__5, close_ok__4)
        switch mtmp3.(type) {
        case Result__unit__string_Ok:
            var t32 string = "closed " + name__6
            var t33 Result__string__string = Result__string__string_Ok{
                _0: t32,
            }
            retv28 = t33
            return retv28
        case Result__unit__string_Err:
            var x5 string = mtmp3.(Result__unit__string_Err)._0
            var try_residual__31 string = x5
            var t34 Result__string__string = Result__string__string_Err{
                _0: try_residual__31,
            }
            retv28 = t34
            return retv28
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x2 string = mtmp0.(Result__Handle__string_Err)._0
        var try_residual__24 string = x2
        var t35 Result__string__string = Result__string__string_Err{
            _0: try_residual__24,
        }
        retv28 = t35
        return retv28
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__string__string) string {
    var retv37 string
    var jp39 string
    switch res__7.(type) {
    case Result__string__string_Ok:
        var x7 string = res__7.(Result__string__string_Ok)._0
        var value__8 string = x7
        var t40 string = "ok " + value__8
        jp39 = t40
    case Result__string__string_Err:
        var x8 string = res__7.(Result__string__string_Err)._0
        var err__9 string = x8
        var t41 string = "err " + err__9
        jp39 = t41
    default:
        panic("non-exhaustive match")
    }
    retv37 = jp39
    return retv37
}

func main0() struct{} {
    var t43 Result__string__string = use_handle(true, true)
    var t44 string = show(t43)
    println__T_string(t44)
    var t45 Result__string__string = use_handle(false, true)
    var t46 string = show(t45)
    println__T_string(t46)
    var t47 Result__string__string = use_handle(true, false)
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
