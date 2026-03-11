package main

import (
    "fmt"
    "time"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Span = time.Duration

func describe() string {
    var retv1 string
    var value__0 Span = time.Duration(1500000000)
    var t2 string = fmt.Sprintf("span => %v", value__0)
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
