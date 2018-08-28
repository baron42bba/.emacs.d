package main
 // `(user-full-name)` <`user-mail-address`>


import (
//	"errors"
	"github.com/jessevdk/go-flags"
	"os"
)

type Options struct {
//	 Example of verbosity with level
	Verbose []bool \`short:"v" long:"verbose" description:"Verbose output"\`

}

var options Options

var parser = flags.NewParser(&options, flags.Default)

func main() {
	if _, err := parser.Parse(); err != nil {
		if flagsErr, ok := err.(*flags.Error); ok && flagsErr.Type == flags.ErrHelp {
			os.Exit(0)
		} else {
			os.Exit(1)
		}
	}
}
