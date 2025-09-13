use crate::go::lib;
use crate::go::lib::EmbededRawString;

// unit_to_string(x : struct{}) string
// bool_to_string(x : bool) string
// int_to_string(x : int) string
// int_add(x : int, y : int) int
// int_sub(x : int, y : int) int
// int_less(x : int, y : int) bool
// print(s : string) struct{}
// println(s : string) struct{}
// missing(s : string) struct{}
pub fn make_runtime() -> Vec<lib::Item> {
    let rt = vec![lib::Item::EmbededRawString(EmbededRawString {
        value: r#"package main

import (
    "fmt"
)

func unit_to_string(x struct{}) string {
    return "()"
}

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int_to_string(x int) string {
    return fmt.Sprintf("%d", x)
}

func int_add(x int, y int) int {
    return x + y
}

func int_sub(x int, y int) int {
    return x - y
}

func int_less(x int, y int) bool {
    return x < y
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func missing(s string) struct{} {
    panic("missing: " + s)
    return struct{}{}
}"#
        .to_string(),
    })];

    rt
}
