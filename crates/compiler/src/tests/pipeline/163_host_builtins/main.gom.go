package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func host_read_file(path string) string {
    var data []uint8
    var err error
    data, err = _goml_os.ReadFile(path)
    if err != nil {
        panic(err.Error())
    }
    return string(data)
}

func host_write_file(path string, content string) struct{} {
    var err error = _goml_os.WriteFile(path, []byte(content), 0644)
    if err != nil {
        panic(err.Error())
    }
    return struct{}{}
}

func host_file_exists(path string) bool {
    var err error
    _, err = _goml_os.Stat(path)
    return err == nil
}

func host_read_dir(path string) []string {
    var entries []_goml_os.DirEntry
    var err error
    entries, err = _goml_os.ReadDir(path)
    if err != nil {
        panic(err.Error())
    }
    var names []string
    var i int32 = 0
    for {
        if i >= int32(len(entries)) {
            break
        }
        var entry _goml_os.DirEntry = entries[i]
        names = append(names, entry.Name())
        i = i + 1
    }
    return names
}

func main0() struct{} {
    host_write_file("goml-host-test.txt", "host-ok")
    var t5 string = host_read_file("goml-host-test.txt")
    println__T_string(t5)
    var t6 bool = host_file_exists("goml-host-test.txt")
    var t7 string = bool_to_string(t6)
    println__T_string(t7)
    var names__0 []string = host_read_dir(".")
    var t8 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(names__0)
    var t9 bool = t8 > 0
    var t10 string = bool_to_string(t9)
    println__T_string(t10)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(self__73 []string) int32 {
    var retv14 int32
    var t15 int32 = int32(len(self__73))
    retv14 = t15
    return retv14
}

func main() {
    main0()
}
