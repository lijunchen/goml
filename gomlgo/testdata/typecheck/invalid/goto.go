package fixture

func invalid() {
	goto Done
	value := 1
Done:
	_ = value
}
