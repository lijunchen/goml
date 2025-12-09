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

type Point__int32__string struct {
    x int32
    y string
}

type Point__string__string struct {
    x string
    y string
}

type Point__string__int32 struct {
    x string
    y int32
}

func main0() struct{} {
    var ret4 struct{}
    var p1__4 Point__int32__string = impl_inherent_Point_TParam_U_TParam_V_new__U_int32__V_string(10, "hello")
    var p2__5 Point__string__string = impl_inherent_Point_TParam_U_TParam_V_new__U_string__V_string("goml", "lang")
    var p3__6 Point__string__int32 = impl_inherent_Point_TParam_U_TParam_V_swap__U_int32__V_string(p1__4)
    var x__7 int32 = p3__6.y
    var t1 string = int32_to_string(x__7)
    string_println(t1)
    var x2__8 string = impl_inherent_Point_TParam_U_TParam_V_get_x__U_string__V_string(p2__5)
    ret4 = string_println(x2__8)
    return ret4
}

func impl_inherent_Point_TParam_U_TParam_V_new__U_int32__V_string(x__0 int32, y__1 string) Point__int32__string {
    var ret5 Point__int32__string
    ret5 = Point__int32__string{
        x: x__0,
        y: y__1,
    }
    return ret5
}

func impl_inherent_Point_TParam_U_TParam_V_new__U_string__V_string(x__0 string, y__1 string) Point__string__string {
    var ret6 Point__string__string
    ret6 = Point__string__string{
        x: x__0,
        y: y__1,
    }
    return ret6
}

func impl_inherent_Point_TParam_U_TParam_V_swap__U_int32__V_string(self__2 Point__int32__string) Point__string__int32 {
    var ret7 Point__string__int32
    var t2 string = self__2.y
    var t3 int32 = self__2.x
    ret7 = Point__string__int32{
        x: t2,
        y: t3,
    }
    return ret7
}

func impl_inherent_Point_TParam_U_TParam_V_get_x__U_string__V_string(self__3 Point__string__string) string {
    var ret8 string
    ret8 = self__3.x
    return ret8
}

func main() {
    main0()
}
