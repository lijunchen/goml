package main

import (
    "fmt"
    "net/url"
)

func string_println(s string) struct{} {
    fmt.Println(s)
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

type Result__URL__GoError interface {
    isResult__URL__GoError()
}

type Result__URL__GoError_Ok struct {
    _0 URL
}

func (_ Result__URL__GoError_Ok) isResult__URL__GoError() {}

type Result__URL__GoError_Err struct {
    _0 GoError
}

func (_ Result__URL__GoError_Err) isResult__URL__GoError() {}

type GoError = error

type URL = *url.URL

func url_host_ffi_wrap(p0 URL) string {
    return p0.Host
}

func url_set_path_ffi_wrap(p0 URL, p1 string) struct{} {
    p0.Path = p1
    return struct{}{}
}

func update__native(text__0 string, path__1 string) (string, GoError) {
    var jp10 URL
    var mtmp0_value_0 URL
    var mtmp0_err GoError
    mtmp0_value_0, mtmp0_err = url.Parse(text__0)
    if mtmp0_err != nil {
        var ret_zero string
        return ret_zero, mtmp0_err
    }
    jp10 = mtmp0_value_0
    var url__2 URL = jp10
    var host__3 string = url_host_ffi_wrap(url__2)
    url_set_path_ffi_wrap(url__2, path__1)
    var t11 string = host__3 + "|"
    var t12 string = url__2.String()
    var t13 string = t11 + t12
    return t13, nil
}

func update(text__0 string, path__1 string) Result__string__GoError {
    var native_value_0 string
    var native_err GoError
    native_value_0, native_err = update__native(text__0, path__1)
    if native_err != nil {
        return Result__string__GoError_Err{
            _0: native_err,
        }
    }
    return Result__string__GoError_Ok{
        _0: native_value_0,
    }
}

func show(res__4 Result__string__GoError) string {
    var retv17 string
    var jp19 string
    switch res__4.(type) {
    case Result__string__GoError_Ok:
        var x4 string = res__4.(Result__string__GoError_Ok)._0
        var text__5 string = x4
        jp19 = text__5
    case Result__string__GoError_Err:
        var x5 GoError = res__4.(Result__string__GoError_Err)._0
        var err__6 GoError = x5
        var t20 string = go_error_to_string(err__6)
        jp19 = t20
    default:
        panic("non-exhaustive match")
    }
    retv17 = jp19
    return retv17
}

func main0() struct{} {
    var t22 Result__string__GoError = update("https://example.com/old", "/new")
    var t23 string = show(t22)
    string_println(t23)
    return struct{}{}
}

func main() {
    main0()
}
