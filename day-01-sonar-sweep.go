package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func a() int {
	var (
		init  bool = false
		total int
		prev  int
		next  int
	)

	lines := bufio.NewScanner(os.Stdin)
	for lines.Scan() {
		next, _ = strconv.Atoi(lines.Text())
		if !init {
			init = true
			prev = next
			continue
		}

		if next > prev {
			total += 1
		}

		prev = next
	}

	return total
}

func b() int {
	var (
		t int
		l int
		v int
		s bool = false
		a int
		b int
		c int
		d int
	)

	lines := bufio.NewScanner(os.Stdin)
	for lines.Scan() {
		v, _ = strconv.Atoi(lines.Text())
		s = l > 2

		switch l % 4 {
		case 0:
			a = v
			c += v
			d += v

			if s && c > b {
				t += 1
			}
		case 1:
			a += v
			b = v
			d += v

			if s && d > c {
				t += 1
			}
		case 2:
			a += v
			b += v
			c = v

			if s && a > d {
				t += 1
			}
		case 3:
			b += v
			c += v
			d = v

			if s && b > a {
				t += 1
			}
		}

		l += 1
	}

	return t
}

func main() {
	fmt.Printf("Part A: %d\n", a())
	os.Stdin.Seek(0, 0)
	fmt.Printf("Part B: %d\n", b())
}
