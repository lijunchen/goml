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

type Duration = time.Duration

func child_a() struct{} {
    println__T_string("child-a")
    return struct{}{}
}

func child_b() struct{} {
    println__T_string("child-b")
    return struct{}{}
}

func spawn(job__0 func() struct{}) struct{} {
    go job__0()
    return struct{}{}
}

func pick(flag__1 bool) func() struct{} {
    var retv13 func() struct{}
    var jp15 func() struct{}
    if flag__1 {
        jp15 = child_a
    } else {
        jp15 = child_b
    }
    retv13 = jp15
    return retv13
}

func wait() struct{} {
    var t17 Duration = time.Duration(5000000)
    time.Sleep(t17)
    return struct{}{}
}

func main0() struct{} {
    go child_a()
    wait()
    spawn(child_b)
    wait()
    var job__2 func() struct{} = pick(true)
    go job__2()
    wait()
    println__T_string("main")
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
