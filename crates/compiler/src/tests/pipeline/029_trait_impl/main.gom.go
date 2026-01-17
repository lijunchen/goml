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

type Point struct {
    x int32
    y int32
}

type Maybe__int32 interface {
    isMaybe__int32()
}

type Just struct {
    _0 int32
}

func (_ Just) isMaybe__int32() {}

type Nothing struct {}

func (_ Nothing) isMaybe__int32() {}

func goml__trait__impl_x23_Display_x23_Point_x23_show(self__0 Point) string {
    var ret9 string
    ret9 = "Point"
    return ret9
}

func goml__trait__impl_x23_Display_x23_Maybe_x5b_int32_x5d__x23_show(self__1 Maybe__int32) string {
    var ret10 string
    switch self__1 := self__1.(type) {
    case Just:
        var x0 int32 = self__1._0
        var value__2 int32 = x0
        var t5 string = int32_to_string(value__2)
        var t4 string = "Just(" + t5
        ret10 = t4 + ")"
    case Nothing:
        ret10 = "Nothing"
    }
    return ret10
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var ret11 Maybe__int32
    if flag__3 {
        ret11 = Just{
            _0: 42,
        }
    } else {
        ret11 = Nothing{}
    }
    return ret11
}

func main0() struct{} {
    var ret12 struct{}
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t6 string = goml__trait__impl_x23_Display_x23_Point_x23_show(point__4)
    string_println(t6)
    var t7 string = goml__trait__impl_x23_Display_x23_Maybe_x5b_int32_x5d__x23_show(some_number__5)
    string_println(t7)
    var t8 string = goml__trait__impl_x23_Display_x23_Maybe_x5b_int32_x5d__x23_show(none_number__6)
    string_println(t8)
    ret12 = struct{}{}
    return ret12
}

func main() {
    main0()
}
