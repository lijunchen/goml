package main

import (
    _goml_fmt "fmt"
    "os"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func go_error_to_string(value GoError) string {
    return value.Error()
}

type Result__string__GoError interface {
    isResult__string__GoError()
}

type Result__string__GoError_Ok struct {
    _0 string
}

func (_ Result__string__GoError_Ok) isResult__string__GoError() {}

type Result__string__GoError_Err struct {
    _0 GoError
}

func (_ Result__string__GoError_Err) isResult__string__GoError() {}

type Result__File__GoError interface {
    isResult__File__GoError()
}

type Result__File__GoError_Ok struct {
    _0 File
}

func (_ Result__File__GoError_Ok) isResult__File__GoError() {}

type Result__File__GoError_Err struct {
    _0 GoError
}

func (_ Result__File__GoError_Err) isResult__File__GoError() {}

type Result__unit__GoError interface {
    isResult__unit__GoError()
}

type Result__unit__GoError_Ok struct {
    _0 struct{}
}

func (_ Result__unit__GoError_Ok) isResult__unit__GoError() {}

type Result__unit__GoError_Err struct {
    _0 GoError
}

func (_ Result__unit__GoError_Err) isResult__unit__GoError() {}

type GoError = error

type File = *os.File

func describe__native(path__0 string) (string, GoError) {
    var jp14 File
    var mtmp0_value_0 File
    var mtmp0_err GoError
    mtmp0_value_0, mtmp0_err = os.Open(path__0)
    if mtmp0_err != nil {
        var ret_zero string
        return ret_zero, mtmp0_err
    }
    jp14 = mtmp0_value_0
    var file__1 File = jp14
    var name__2 string = file__1.Name()
    var mtmp3_err GoError = file__1.Close()
    if mtmp3_err != nil {
        var ret_zero string
        return ret_zero, mtmp3_err
    }
    var t16 string = "ok=" + name__2
    return t16, nil
}

func describe(path__0 string) Result__string__GoError {
    var native_value_0 string
    var native_err GoError
    native_value_0, native_err = describe__native(path__0)
    if native_err != nil {
        return Result__string__GoError_Err{
            _0: native_err,
        }
    }
    return Result__string__GoError_Ok{
        _0: native_value_0,
    }
}

func show(res__3 Result__string__GoError) string {
    var retv21 string
    var jp23 string
    switch res__3.(type) {
    case Result__string__GoError_Ok:
        var x7 string = res__3.(Result__string__GoError_Ok)._0
        var value__4 string = x7
        jp23 = value__4
    case Result__string__GoError_Err:
        var x8 GoError = res__3.(Result__string__GoError_Err)._0
        var err__5 GoError = x8
        var t24 string = go_error_to_string(err__5)
        var t25 string = "err=" + t24
        jp23 = t25
    default:
        panic("non-exhaustive match")
    }
    retv21 = jp23
    return retv21
}

func main0() struct{} {
    var t27 Result__string__GoError = describe("/etc/hosts")
    var t28 string = show(t27)
    println__T_string(t28)
    var t29 Result__string__GoError = describe("/definitely/missing")
    var t30 string = show(t29)
    println__T_string(t30)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
