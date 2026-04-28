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

type Span = _goml_pkg_time.Duration

func describe() string {
    var retv1 string
    var value__0 Span = _goml_pkg_time.Duration(1500000000)
    var t2 string = _goml_fmt.Sprintf("span => %v", value__0)
    retv1 = t2
    return retv1
}

func main0() struct{} {
    var t4 string = describe()
    string_println(t4)
    return struct{}{}
}

func main() {
    main0()
}
