package main

import "os/exec"

func main() {
	command := exec.Cmd{
		Path: "/bin/true",
		Args: []string{"/bin/true"},
		Cancel: func() error {
			return nil
		},
	}
	println(command.Path, len(command.Args))
}
