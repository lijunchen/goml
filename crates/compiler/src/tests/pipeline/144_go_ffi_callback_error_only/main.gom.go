package main

import (
    _goml_fmt "fmt"
    "os"
    "io"
    "io/fs"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func go_error_to_string(value GoError) string {
    return value.Error()
}

type Result__unit__GoError interface {
    isResult__unit__GoError()
}

type Ok struct {
    _0 struct{}
}

func (_ Ok) isResult__unit__GoError() {}

type Err struct {
    _0 GoError
}

func (_ Err) isResult__unit__GoError() {}

type GoError = error

type FS = fs.FS

type DirEntry = fs.DirEntry

func eof_ffi_wrap() GoError {
    return io.EOF
}

func walk_dir_ffi_wrap(p0 FS, p1 string, p2 func(string, DirEntry, GoError) Result__unit__GoError) Result__unit__GoError {
    var ffi_err GoError = fs.WalkDir(p0, p1, func(callback_arg_0 string, callback_arg_1 DirEntry, callback_arg_2 GoError) GoError {
        var ret_value Result__unit__GoError = p2(callback_arg_0, callback_arg_1, callback_arg_2)
        switch ret_variant := ret_value.(type) {
        case Ok:
            return nil
        case Err:
            return ret_variant._0
        default:
            panic("non-exhaustive match")
        }
    })
    if ffi_err != nil {
        return Err{
            _0: ffi_err,
        }
    }
    return Ok{
        _0: struct{}{},
    }
}

func stop__native(path__0 string, entry__1 DirEntry, err__2 GoError) (struct{}, GoError) {
    var t8 GoError = eof_ffi_wrap()
    var ret_zero struct{}
    return ret_zero, t8
}

func stop(path__0 string, entry__1 DirEntry, err__2 GoError) Result__unit__GoError {
    var native_value_0 struct{}
    var native_err GoError
    native_value_0, native_err = stop__native(path__0, entry__1, err__2)
    if native_err != nil {
        return Err{
            _0: native_err,
        }
    }
    return Ok{
        _0: native_value_0,
    }
}

func show(res__3 Result__unit__GoError) string {
    var retv11 string
    var jp13 string
    switch res__3.(type) {
    case Ok:
        jp13 = "ok"
    case Err:
        var x4 GoError = res__3.(Err)._0
        var err__4 GoError = x4
        var t14 string = go_error_to_string(err__4)
        jp13 = t14
    default:
        panic("non-exhaustive match")
    }
    retv11 = jp13
    return retv11
}

func main0() struct{} {
    var t16 FS = os.DirFS(".")
    var t17 Result__unit__GoError = walk_dir_ffi_wrap(t16, ".", stop)
    var t18 string = show(t17)
    println__T_string(t18)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
