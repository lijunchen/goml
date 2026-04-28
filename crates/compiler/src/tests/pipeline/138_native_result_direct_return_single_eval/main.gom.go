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

func go_error_to_string(value GoError) string {
    return value.Error()
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type Result__int32__GoError interface {
    isResult__int32__GoError()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__GoError() {}

type Err struct {
    _0 GoError
}

func (_ Err) isResult__int32__GoError() {}

type GoError = error

func inner__native(counter__0 *ref_int32_x) (int32, GoError) {
    var t7 int32 = ref_get__Ref_5int32(counter__0)
    var t8 int32 = t7 + 1
    ref_set__Ref_5int32(counter__0, t8)
    var t9 int32 = ref_get__Ref_5int32(counter__0)
    return t9, nil
}

func outer__native(counter__1 *ref_int32_x) (int32, GoError) {
    var t13_value_0 int32
    var t13_err GoError
    t13_value_0, t13_err = inner__native(counter__1)
    if t13_err != nil {
        var ret_zero int32
        return ret_zero, t13_err
    }
    return t13_value_0, nil
}

func outer(counter__1 *ref_int32_x) Result__int32__GoError {
    var native_value_0 int32
    var native_err GoError
    native_value_0, native_err = outer__native(counter__1)
    if native_err != nil {
        return Err{
            _0: native_err,
        }
    }
    return Ok{
        _0: native_value_0,
    }
}

func show(res__2 Result__int32__GoError) string {
    var retv15 string
    var jp17 string
    switch res__2.(type) {
    case Ok:
        var x1 int32 = res__2.(Ok)._0
        var value__3 int32 = x1
        var t18 string = int32_to_string(value__3)
        jp17 = t18
    case Err:
        var x2 GoError = res__2.(Err)._0
        var err__4 GoError = x2
        var t19 string = go_error_to_string(err__4)
        var t20 string = "err=" + t19
        jp17 = t20
    default:
        panic("non-exhaustive match")
    }
    retv15 = jp17
    return retv15
}

func main0() struct{} {
    var counter__5 *ref_int32_x = ref__Ref_5int32(0)
    var t22 Result__int32__GoError = outer(counter__5)
    var t23 string = show(t22)
    println__T_string(t23)
    var t24 int32 = ref_get__Ref_5int32(counter__5)
    var t25 string = int32_to_string(t24)
    println__T_string(t25)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
