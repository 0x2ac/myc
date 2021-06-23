package main

import (
	"errors"
	"io/ioutil"
	"log"
	"os"
	"os/exec"

	"github.com/kartiknair/myc/analyzer"
	llvmgen "github.com/kartiknair/myc/gen/llvm"
	"github.com/kartiknair/myc/lexer"
	"github.com/kartiknair/myc/parser"
	"github.com/urfave/cli/v2"
)

var CLANG_EXECUTABLE_PATH = "clang"

func genIRFromFile(filename string) string {
	code, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatalf("Failed while attempting to read source file.\n%s", err.Error())
	}

	tokens := lexer.Lex(string(code))
	parsed := parser.Parse(tokens)
	analyzer.Analyze(parsed)
	ir := llvmgen.Gen(parsed)

	return ir
}

func run(filename string) {
	ir := genIRFromFile(filename)

	irFile, err := ioutil.TempFile("", "myc-ir--*.ll")
	if err != nil {
		log.Fatalf("Failed while opening temp file for IR.\n%s", err.Error())
	}
	executableFile, err := ioutil.TempFile("", "myc-exe--*.out")
	if err != nil {
		log.Fatalf("Failed while opening temp file for executable.\n%s", err.Error())
	}

	numberOfIRBytes, err := irFile.Write([]byte(ir))
	if err != nil {
		log.Fatalf("Failed while writing to temp IR file. Wrote %d bytes.\n%s", numberOfIRBytes, err.Error())
	}
	numberOfExeBytes, err := executableFile.Write([]byte(ir))
	if err != nil {
		log.Fatalf("Failed while writing to temp executable file. Wrote %d bytes.\n%s", numberOfExeBytes, err.Error())
	}

	irFile.Close()
	executableFile.Close()

	compileCommand := exec.Command(
		CLANG_EXECUTABLE_PATH,
		irFile.Name(),
		"-o",
		executableFile.Name(),
	)
	compileCommand.Stdout = os.Stdout
	compileCommand.Stderr = os.Stderr
	err = compileCommand.Run()
	if err != nil {
		log.Fatal(err.Error())
	}

	runCmd := exec.Command(executableFile.Name(), os.Args...)
	runCmd.Stdout = os.Stdout
	runCmd.Stderr = os.Stderr
	err = runCmd.Run()
	if err != nil {
		log.Fatalf("Failed to run compiled binary.\n%s", err.Error())
	}
}

func build(filename string, executableName string) {
	ir := genIRFromFile(filename)

	irFile, err := ioutil.TempFile("", "myc-ir--*.ll")
	if err != nil {
		log.Fatalf("Failed while opening temp file for IR.\n%s", err.Error())
	}
	numberOfIRBytes, err := irFile.Write([]byte(ir))
	if err != nil {
		log.Fatalf("Failed while writing to temp IR file. Wrote %d bytes.\n%s", numberOfIRBytes, err.Error())
	}
	irFile.Close()

	compileCommand := exec.Command(
		CLANG_EXECUTABLE_PATH,
		irFile.Name(),
		"-o",
		executableName,
	)
	compileCommand.Stdout = os.Stdout
	compileCommand.Stderr = os.Stderr
	err = compileCommand.Run()
	if err != nil {
		log.Fatal(err.Error())
	}
}

func main() {
	var executableOutputFile string

	app := &cli.App{
		Name:  "myc",
		Usage: "A simple programming language (WIP).",
		Commands: []*cli.Command{
			{
				Name:  "run",
				Usage: "Builds and immediately runs the provided source file.",
				Action: func(c *cli.Context) error {
					if c.Args().Len() > 1 {
						return errors.New("\n\nToo many arguments provided.\nRun takes only a single argument (and no flags).")
					}
					filename := c.Args().First()
					if filename == "" {
						return errors.New("Source file not provided.")
					}
					run(filename)
					return nil
				},
			},
			{
				Name:  "build",
				Usage: "Builds the provided source file to an executable.",
				Flags: []cli.Flag{
					&cli.StringFlag{
						Name:        "output",
						Aliases:     []string{"o"},
						Value:       "a.out",
						Usage:       "Name of the executable.",
						Destination: &executableOutputFile,
					},
				},
				Action: func(c *cli.Context) error {
					if c.Args().Len() > 1 {
						return errors.New(`

Too many arguments provided.

If you've provided flags make sure they go before the arguments.
    Wrong: $ myc build file.myc -o foo
    Right: $ myc build -o foo file.myc
`)
					}

					filename := c.Args().First()
					if filename == "" {
						return errors.New("Source file not provided.")
					}
					build(filename, executableOutputFile)
					return nil
				},
			},
		},
	}

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}
}
