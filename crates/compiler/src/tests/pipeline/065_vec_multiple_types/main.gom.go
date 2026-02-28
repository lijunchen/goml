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

func main0() struct{} {
    var vi__0 []int32
    var vi__1 []int32
    var val_i__2 int32
    var len_i__3 int32
    var vs__4 []string
    var vs__5 []string
    var vs__6 []string
    var val_s__7 string
    var len_s__8 int32
    var vb__9 []bool
    var vb__10 []bool
    var vb__11 []bool
    var val_b__12 bool
    var len_b__13 int32
    var t6 string
    var mtmp0 struct{}
    var t7 string
    var mtmp1 struct{}
    var mtmp2 struct{}
    var t8 string
    var mtmp3 struct{}
    var t9 string
    var mtmp4 struct{}
    var t10 string
    var mtmp5 struct{}
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    _ = mtmp3
    _ = mtmp4
    _ = mtmp5
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            vi__0 = nil
            vi__1 = append(vi__0, 42)
            val_i__2 = vi__1[0]
            len_i__3 = int32(len(vi__1))
            vs__4 = nil
            vs__5 = append(vs__4, "hello")
            vs__6 = append(vs__5, "world")
            val_s__7 = vs__6[1]
            len_s__8 = int32(len(vs__6))
            vb__9 = nil
            vb__10 = append(vb__9, true)
            vb__11 = append(vb__10, false)
            val_b__12 = vb__11[0]
            len_b__13 = int32(len(vb__11))
            t6 = int32_to_string(val_i__2)
            string_println(t6)
            t7 = int32_to_string(len_i__3)
            string_println(t7)
            string_println(val_s__7)
            t8 = int32_to_string(len_s__8)
            string_println(t8)
            t9 = bool_to_string(val_b__12)
            string_println(t9)
            t10 = int32_to_string(len_b__13)
            string_println(t10)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
