package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func show_int(label__0 string, value__1 int32) struct{} {
    var ret304 struct{}
    var t290 string = int32_to_string(value__1)
    var t289 string = label__0 + t290
    string_println(t289)
    ret304 = struct{}{}
    return ret304
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var ret305 struct{}
    var t292 string = bool_to_string(value__3)
    var t291 string = label__2 + t292
    string_println(t291)
    ret305 = struct{}{}
    return ret305
}

func main0() struct{} {
    var ret306 struct{}
    var base__4 int32 = 10
    var sum__5 int32 = base__4 + 5
    var diff__6 int32 = sum__5 - 3
    var prod__7 int32 = diff__6 * 2
    var quot__8 int32 = prod__7 / 4
    show_int("sum=", sum__5)
    show_int("diff=", diff__6)
    show_int("prod=", prod__7)
    show_int("quot=", quot__8)
    var and_result__9 bool = true && false
    var or_result__10 bool = true || false
    var not_result__11 bool = !false
    var t294 bool = !and_result__9
    var t297 int32 = prod__7 * base__4
    var t296 int32 = sum__5 + t297
    var t298 int32 = prod__7 / 2
    var mtmp287 int32 = t296 - t298
    var t295 bool
    switch mtmp287 {
    case 0:
        t295 = false
    default:
        t295 = true
    }
    var t293 bool = t294 && t295
    var t302 int32 = diff__6 - quot__8
    var t301 int32 = t302 + base__4
    var t303 int32 = sum__5 / 2
    var mtmp288 int32 = t301 - t303
    var t300 bool
    switch mtmp288 {
    case 0:
        t300 = false
    default:
        t300 = true
    }
    var t299 bool = !t300
    var mixed__12 bool = t293 || t299
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    ret306 = struct{}{}
    return ret306
}

func main() {
    main0()
}
