package main

import (
    _goml_fmt "fmt"
    "time"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type GoError = error

type Time = time.Time

type Duration = time.Duration

func describe_epoch() string {
    var retv4 string
    var epoch__0 Time = time.Unix(946684800, 500000000)
    var t5 string = _goml_fmt.Sprintf("epoch snapshot => %v", epoch__0)
    retv4 = t5
    return retv4
}

func describe_planned_launch() string {
    var retv7 string
    var launch__1 Time = time.Unix(1709294730, 250000000)
    var t8 string = _goml_fmt.Sprintf("planned launch => %v", launch__1)
    retv7 = t8
    return retv7
}

func describe_duration() string {
    var retv10 string
    var parsed__2 Duration = time.Duration(1500000000)
    var t11 string = _goml_fmt.Sprintf("parsed duration => %v", parsed__2)
    retv10 = t11
    return retv10
}

func main0() struct{} {
    var t13 string = describe_epoch()
    string_println(t13)
    var t14 string = describe_planned_launch()
    string_println(t14)
    var t15 string = describe_duration()
    string_println(t15)
    return struct{}{}
}

func main() {
    main0()
}
