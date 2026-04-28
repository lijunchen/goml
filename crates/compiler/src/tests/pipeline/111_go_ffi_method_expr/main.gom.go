package main

import (
    _goml_fmt "fmt"
    _goml_pkg_time "time"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

type Moment = _goml_pkg_time.Time

type Span = _goml_pkg_time.Duration

func describe() string {
    var retv1 string
    var base__0 Moment = _goml_pkg_time.Unix(1740823200, 0)
    var t2 int32 = 90 * 1000000000
    var t3 Span = _goml_pkg_time.Duration(t2)
    var shifted__1 Moment = base__0.Add(t3)
    var t4 string = shifted__1.Format("15:04:05")
    retv1 = t4
    return retv1
}

func main0() struct{} {
    var t6 string = describe()
    println__T_string(t6)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
