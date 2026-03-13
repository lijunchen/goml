package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func go_error_to_string(value GoError) string {
    return value.Error()
}

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

type Result__GoInt__GoError interface {
    isResult__GoInt__GoError()
}

type Result__GoInt__GoError_Ok struct {
    _0 GoInt
}

func (_ Result__GoInt__GoError_Ok) isResult__GoInt__GoError() {}

type Result__GoInt__GoError_Err struct {
    _0 GoError
}

func (_ Result__GoInt__GoError_Err) isResult__GoInt__GoError() {}

type GoError = error

type GoInt = int

func touch__native(text__0 string) (struct{}, GoError) {
    var mtmp0_err GoError
    _, mtmp0_err = fmt.Print(text__0)
    if mtmp0_err != nil {
        var ret_zero struct{}
        return ret_zero, mtmp0_err
    }
    return struct{}{}, nil
}

func touch(text__0 string) Result__unit__GoError {
    var native_value_0 struct{}
    var native_err GoError
    native_value_0, native_err = touch__native(text__0)
    if native_err != nil {
        return Result__unit__GoError_Err{
            _0: native_err,
        }
    }
    return Result__unit__GoError_Ok{
        _0: native_value_0,
    }
}

func show(res__1 Result__unit__GoError) string {
    var retv14 string
    var jp16 string
    switch res__1.(type) {
    case Result__unit__GoError_Ok:
        var jp18 string
        jp18 = "ok"
        jp16 = jp18
        retv14 = jp16
        return retv14
    case Result__unit__GoError_Err:
        var x5 GoError = res__1.(Result__unit__GoError_Err)._0
        var err__2 GoError = x5
        var t19 string = go_error_to_string(err__2)
        var t20 string = "err=" + t19
        jp16 = t20
        retv14 = jp16
        return retv14
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t22 Result__unit__GoError = touch("hello")
    var t23 string = show(t22)
    println__T_string(t23)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
