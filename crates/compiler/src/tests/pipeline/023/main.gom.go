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

type Tuple2_int32_string struct {
    _0 int32
    _1 string
}

type Mixed interface {
    isMixed()
}

type OnlyInt struct {
    _0 int32
}

func (_ OnlyInt) isMixed() {}

type OnlyStr struct {
    _0 string
}

func (_ OnlyStr) isMixed() {}

type Both struct {
    _0 int32
    _1 string
}

func (_ Both) isMixed() {}

func match_mixed_pair(pair__0 Tuple2_int32_string) int32 {
    var ret4140 int32
    var x0 int32 = pair__0._0
    var x1 string = pair__0._1
    switch x1 {
    case "zero":
        switch x0 {
        case 0:
            ret4140 = 1
        default:
            ret4140 = 4
        }
    case "one":
        switch x0 {
        case 0:
            ret4140 = 2
        case 1:
            ret4140 = 3
        default:
            ret4140 = 5
        }
    default:
        switch x0 {
        case 0:
            ret4140 = 2
        default:
            ret4140 = 5
        }
    }
    return ret4140
}

func match_mixed_enum(value__1 Mixed) int32 {
    var ret4141 int32
    switch value__1 := value__1.(type) {
    case OnlyInt:
        var x2 int32 = value__1._0
        switch x2 {
        case 0:
            ret4141 = 6
        default:
            ret4141 = 7
        }
    case OnlyStr:
        var x3 string = value__1._0
        switch x3 {
        case "zero":
            ret4141 = 8
        default:
            ret4141 = 9
        }
    case Both:
        var x4 int32 = value__1._0
        var x5 string = value__1._1
        switch x5 {
        case "zero":
            switch x4 {
            case 0:
                ret4141 = 10
            default:
                ret4141 = 12
            }
        default:
            switch x4 {
            case 0:
                ret4141 = 11
            default:
                ret4141 = 13
            }
        }
    }
    return ret4141
}

func main0() struct{} {
    var ret4142 struct{}
    var t4103 Tuple2_int32_string = Tuple2_int32_string{
        _0: 0,
        _1: "zero",
    }
    var t4102 int32 = match_mixed_pair(t4103)
    var t4101 string = int32_to_string(t4102)
    string_println(t4101)
    var t4106 Tuple2_int32_string = Tuple2_int32_string{
        _0: 0,
        _1: "other",
    }
    var t4105 int32 = match_mixed_pair(t4106)
    var t4104 string = int32_to_string(t4105)
    string_println(t4104)
    var t4109 Tuple2_int32_string = Tuple2_int32_string{
        _0: 1,
        _1: "one",
    }
    var t4108 int32 = match_mixed_pair(t4109)
    var t4107 string = int32_to_string(t4108)
    string_println(t4107)
    var t4112 Tuple2_int32_string = Tuple2_int32_string{
        _0: 2,
        _1: "zero",
    }
    var t4111 int32 = match_mixed_pair(t4112)
    var t4110 string = int32_to_string(t4111)
    string_println(t4110)
    var t4115 Tuple2_int32_string = Tuple2_int32_string{
        _0: 2,
        _1: "two",
    }
    var t4114 int32 = match_mixed_pair(t4115)
    var t4113 string = int32_to_string(t4114)
    string_println(t4113)
    var t4118 Mixed = OnlyInt{
        _0: 0,
    }
    var t4117 int32 = match_mixed_enum(t4118)
    var t4116 string = int32_to_string(t4117)
    string_println(t4116)
    var t4121 Mixed = OnlyInt{
        _0: 5,
    }
    var t4120 int32 = match_mixed_enum(t4121)
    var t4119 string = int32_to_string(t4120)
    string_println(t4119)
    var t4124 Mixed = OnlyStr{
        _0: "zero",
    }
    var t4123 int32 = match_mixed_enum(t4124)
    var t4122 string = int32_to_string(t4123)
    string_println(t4122)
    var t4127 Mixed = OnlyStr{
        _0: "hello",
    }
    var t4126 int32 = match_mixed_enum(t4127)
    var t4125 string = int32_to_string(t4126)
    string_println(t4125)
    var t4130 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t4129 int32 = match_mixed_enum(t4130)
    var t4128 string = int32_to_string(t4129)
    string_println(t4128)
    var t4133 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t4132 int32 = match_mixed_enum(t4133)
    var t4131 string = int32_to_string(t4132)
    string_println(t4131)
    var t4136 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t4135 int32 = match_mixed_enum(t4136)
    var t4134 string = int32_to_string(t4135)
    string_println(t4134)
    var t4139 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t4138 int32 = match_mixed_enum(t4139)
    var t4137 string = int32_to_string(t4138)
    ret4142 = string_println(t4137)
    return ret4142
}

func main() {
    main0()
}
