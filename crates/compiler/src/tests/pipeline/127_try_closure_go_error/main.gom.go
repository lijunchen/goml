package main

import (
    _goml_fmt "fmt"
    "time"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func go_error_to_string(value GoError) string {
    return value.Error()
}

type closure_env_run_0 struct {
    text_0 string
    prefix_1 string
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

type Result__Duration__GoError interface {
    isResult__Duration__GoError()
}

type Result__Duration__GoError_Ok struct {
    _0 Duration
}

func (_ Result__Duration__GoError_Ok) isResult__Duration__GoError() {}

type Result__Duration__GoError_Err struct {
    _0 GoError
}

func (_ Result__Duration__GoError_Err) isResult__Duration__GoError() {}

type GoError = error

type Duration = time.Duration

func render_with_prefix__native(prefix__0 string, text__1 string) (string, GoError) {
    var run__3 closure_env_run_0 = closure_env_run_0{
        text_0: text__1,
        prefix_1: prefix__0,
    }
    var t10_value_0 string
    var t10_err GoError
    t10_value_0, t10_err = _goml_inherent_x23_closure_x5f_env_x5f_run_x5f_0_x23_closure_x5f_env_x5f_run_x5f_0_x23_apply_x5f__x5f_native(run__3)
    if t10_err != nil {
        var ret_zero string
        return ret_zero, t10_err
    }
    return t10_value_0, nil
}

func render_with_prefix(prefix__0 string, text__1 string) Result__string__GoError {
    var native_value_0 string
    var native_err GoError
    native_value_0, native_err = render_with_prefix__native(prefix__0, text__1)
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
    var retv12 string
    var jp14 string
    switch res__4.(type) {
    case Result__string__GoError_Ok:
        var x3 string = res__4.(Result__string__GoError_Ok)._0
        var value__5 string = x3
        jp14 = value__5
    case Result__string__GoError_Err:
        var x4 GoError = res__4.(Result__string__GoError_Err)._0
        var err__6 GoError = x4
        var t15 string = go_error_to_string(err__6)
        var t16 string = "err=" + t15
        jp14 = t16
    default:
        panic("non-exhaustive match")
    }
    retv12 = jp14
    return retv12
}

func main0() struct{} {
    var t18 Result__string__GoError = render_with_prefix("seen=", "5s")
    var t19 string = show(t18)
    println__T_string(t19)
    var t20 Result__string__GoError = render_with_prefix("seen=", "bad")
    var t21 string = show(t20)
    println__T_string(t21)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_run_x5f_0_x23_closure_x5f_env_x5f_run_x5f_0_x23_apply_x5f__x5f_native(env7 closure_env_run_0) (string, GoError) {
    var text__1 string = env7.text_0
    var prefix__0 string = env7.prefix_1
    var jp27 Duration
    var mtmp0_value_0 Duration
    var mtmp0_err GoError
    mtmp0_value_0, mtmp0_err = time.ParseDuration(text__1)
    if mtmp0_err != nil {
        var ret_zero string
        return ret_zero, mtmp0_err
    }
    jp27 = mtmp0_value_0
    var value__2 Duration = jp27
    var t28 string = _goml_fmt.Sprintf("%v", value__2)
    var t29 string = prefix__0 + t28
    return t29, nil
}

func main() {
    main0()
}
