package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func match_int(n__0 int32) int32 {
    var ret143 int32
    switch n__0 {
    case 0:
        ret143 = 10
    case 1:
        ret143 = 20
    default:
        ret143 = 30
    }
    return ret143
}

func wildcard_first(n__1 int32) int32 {
    var ret144 int32
    ret144 = 40
    return ret144
}

func wildcard_middle(n__2 int32) int32 {
    var ret145 int32
    switch n__2 {
    case 2:
        ret145 = 90
    case 3:
        ret145 = 100
    default:
        ret145 = 100
    }
    return ret145
}

func repeated(n__3 int32) int32 {
    var ret146 int32
    switch n__3 {
    case 1:
        ret146 = 60
    default:
        ret146 = 80
    }
    return ret146
}

func main0() struct{} {
    var ret147 struct{}
    var t128 int32 = match_int(0)
    var t127 string = int32_to_string(t128)
    string_println(t127)
    var t130 int32 = match_int(5)
    var t129 string = int32_to_string(t130)
    string_println(t129)
    var t132 int32 = wildcard_first(0)
    var t131 string = int32_to_string(t132)
    string_println(t131)
    var t134 int32 = wildcard_first(2)
    var t133 string = int32_to_string(t134)
    string_println(t133)
    var t136 int32 = wildcard_middle(2)
    var t135 string = int32_to_string(t136)
    string_println(t135)
    var t138 int32 = wildcard_middle(3)
    var t137 string = int32_to_string(t138)
    string_println(t137)
    var t140 int32 = repeated(1)
    var t139 string = int32_to_string(t140)
    string_println(t139)
    var t142 int32 = repeated(3)
    var t141 string = int32_to_string(t142)
    ret147 = string_println(t141)
    return ret147
}

func main() {
    main0()
}
