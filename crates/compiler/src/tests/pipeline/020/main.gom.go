package main

import (
    "fmt"
)

func unit_to_string(x struct{}) string {
    return "()"
}

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

type Wrapper__int32 struct {
    value int32
}

type Wrapper__unit struct {
    value struct{}
}

type Shape__int32 interface {
    isShape__int32()
}

type Shape__int32_Dot struct {
    _0 Point
}

func (_ Shape__int32_Dot) isShape__int32() {}

type Shape__int32_Wrapped struct {
    _0 Wrapper__int32
}

func (_ Shape__int32_Wrapped) isShape__int32() {}

type Shape__int32_Origin struct {}

func (_ Shape__int32_Origin) isShape__int32() {}

type Shape__unit interface {
    isShape__unit()
}

type Shape__unit_Dot struct {
    _0 Point
}

func (_ Shape__unit_Dot) isShape__unit() {}

type Shape__unit_Wrapped struct {
    _0 Wrapper__unit
}

func (_ Shape__unit_Wrapped) isShape__unit() {}

type Shape__unit_Origin struct {}

func (_ Shape__unit_Origin) isShape__unit() {}

func bounce_int(shape__0 Shape__int32) Shape__int32 {
    var ret1070 Shape__int32
    switch shape__0 := shape__0.(type) {
    case Shape__int32_Dot:
        var x0 Point = shape__0._0
        var point__1 Point = x0
        ret1070 = Shape__int32_Dot{
            _0: point__1,
        }
    case Shape__int32_Wrapped:
        var x1 Wrapper__int32 = shape__0._0
        var inner__2 Wrapper__int32 = x1
        ret1070 = Shape__int32_Wrapped{
            _0: inner__2,
        }
    case Shape__int32_Origin:
        ret1070 = Shape__int32_Origin{}
    }
    return ret1070
}

func point32_to_string(point__8 Point) string {
    var ret1073 string
    var mtmp4 Point = point__8
    var x5 int32 = mtmp4.x
    var x6 int32 = mtmp4.y
    var y__10 int32 = x6
    var x__9 int32 = x5
    var t1038 string = int32_to_string(x__9)
    var with_x__11 string = "Point { x: " + t1038
    var with_y_label__12 string = with_x__11 + ", y: "
    var t1039 string = int32_to_string(y__10)
    var with_y__13 string = with_y_label__12 + t1039
    ret1073 = with_y__13 + " }"
    return ret1073
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var ret1074 string
    var mtmp7 Wrapper__int32 = wrapper__14
    var x8 int32 = mtmp7.value
    var value__15 int32 = x8
    var t1040 string = int32_to_string(value__15)
    var prefix__16 string = "Wrapper[int32] { value: " + t1040
    ret1074 = prefix__16 + " }"
    return ret1074
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var ret1075 string
    var mtmp9 Wrapper__unit = wrapper__17
    var x10 struct{} = mtmp9.value
    var value__18 struct{} = x10
    var t1041 string = unit_to_string(value__18)
    var prefix__19 string = "Wrapper[unit] { value: " + t1041
    ret1075 = prefix__19 + " }"
    return ret1075
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    var ret1076 string
    switch shape__20 := shape__20.(type) {
    case Shape__int32_Dot:
        var x11 Point = shape__20._0
        var point__21 Point = x11
        var t1042 string = point32_to_string(point__21)
        var prefix__22 string = "Shape::Dot(" + t1042
        ret1076 = prefix__22 + ")"
    case Shape__int32_Wrapped:
        var x12 Wrapper__int32 = shape__20._0
        var wrapper__23 Wrapper__int32 = x12
        var t1043 string = wrapper_int32_to_string(wrapper__23)
        var prefix__24 string = "Shape::Wrapped(" + t1043
        ret1076 = prefix__24 + ")"
    case Shape__int32_Origin:
        ret1076 = "Shape::Origin"
    }
    return ret1076
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    var ret1077 string
    switch shape__25 := shape__25.(type) {
    case Shape__unit_Dot:
        var x13 Point = shape__25._0
        var point__26 Point = x13
        var t1044 string = point32_to_string(point__26)
        var prefix__27 string = "Shape::Dot(" + t1044
        ret1077 = prefix__27 + ")"
    case Shape__unit_Wrapped:
        var x14 Wrapper__unit = shape__25._0
        var wrapper__28 Wrapper__unit = x14
        var t1045 string = wrapper_unit_to_string(wrapper__28)
        var prefix__29 string = "Shape::Wrapped(" + t1045
        ret1077 = prefix__29 + ")"
    case Shape__unit_Origin:
        ret1077 = "Shape::Origin"
    }
    return ret1077
}

func main0() struct{} {
    var ret1078 struct{}
    var t1047 Point = Point{
        x: 3,
        y: 4,
    }
    var t1046 string = point32_to_string(t1047)
    string_println(t1046)
    var t1049 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t1048 string = wrapper_int32_to_string(t1049)
    string_println(t1048)
    var t1051 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t1050 string = wrapper_unit_to_string(t1051)
    string_println(t1050)
    var t1052 Shape__int32 = Shape__int32_Origin{}
    var bounced_origin__30 Shape__int32 = bounce_int(t1052)
    var t1055 Point = Point{
        x: 3,
        y: 4,
    }
    var t1054 Shape__int32 = Shape__int32_Dot{
        _0: t1055,
    }
    var t1053 string = shape_int32_to_string(t1054)
    string_println(t1053)
    var t1058 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t1057 Shape__int32 = Shape__int32_Wrapped{
        _0: t1058,
    }
    var t1056 string = shape_int32_to_string(t1057)
    string_println(t1056)
    var t1059 string = shape_int32_to_string(bounced_origin__30)
    string_println(t1059)
    var t1062 Point = Point{
        x: 3,
        y: 4,
    }
    var t1061 Shape__unit = Shape__unit_Dot{
        _0: t1062,
    }
    var t1060 string = shape_unit_to_string(t1061)
    string_println(t1060)
    var t1065 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t1064 Shape__unit = Shape__unit_Wrapped{
        _0: t1065,
    }
    var t1063 string = shape_unit_to_string(t1064)
    string_println(t1063)
    var t1067 Shape__unit = Shape__unit_Origin{}
    var t1066 string = shape_unit_to_string(t1067)
    string_println(t1066)
    var t1069 Shape__int32 = Shape__int32_Origin{}
    var t1068 Shape__int32 = bounce_int(t1069)
    describe__T_int32(t1068)
    ret1078 = string_println("struct enums!")
    return ret1078
}

func describe__T_int32(shape__7 Shape__int32) int32 {
    var ret1079 int32
    switch shape__7.(type) {
    case Shape__int32_Dot:
        ret1079 = 1
    case Shape__int32_Wrapped:
        ret1079 = 2
    case Shape__int32_Origin:
        ret1079 = 0
    }
    return ret1079
}

func main() {
    main0()
}
