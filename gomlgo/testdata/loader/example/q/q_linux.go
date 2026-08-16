//go:build linux

package q

import "example/r"

type Number int

const Linux Number = 1

func private() int {
    return r.Value
}
