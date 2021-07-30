package main

import (
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/kartiknair/myc/pkg/analyzer"
	"github.com/kartiknair/myc/pkg/ast"
	llvmgen "github.com/kartiknair/myc/pkg/gen/llvm"
	"github.com/kartiknair/myc/pkg/lexer"
	"github.com/kartiknair/myc/pkg/parser"
	"github.com/urfave/cli/v2"
)

var CLANG_EXECUTABLE_PATH = "clang"

const DEBUG_MESSAGES = true

func genIRFromFile(filename string) string {
	code, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatalf("Failed while attempting to read source file.\n%s", err.Error())
	}

	start := time.Now()

	m := ast.Module{
		Path:    filename,
		Source:  string(code),
		Exports: make(map[string]ast.Type),
	}

	lexer.Lex(&m)
	parser.Parse(&m)
	analyzer.Analyze(&m)

	lpaTime := time.Now()

	if DEBUG_MESSAGES {
		fmt.Printf("time: %dus for lexing and parsing and analysis\n", lpaTime.Sub(start).Microseconds())
	}

	ir := llvmgen.Gen(&m)

	if DEBUG_MESSAGES {
		irGenTime := time.Now()
		fmt.Printf("time: %dus to generate LLVM IR\n", irGenTime.Sub(lpaTime).Microseconds())
	}

	return ir
}

func compileIRToExecutable(ir string) string {
	if cc := os.Getenv("MYC_CC"); cc != "" {
		CLANG_EXECUTABLE_PATH = cc
	}

	tmpDir, err := ioutil.TempDir("", "myc-tmp--*")
	if err != nil {
		log.Fatalf("Failed while creating temp directory.\n%s", err.Error())
	}

	mycExeFilePath := filepath.Join(tmpDir, "myc-exe.out")

	compileCommand := exec.Command(
		CLANG_EXECUTABLE_PATH,
		"-x",
		"ir",
		"-o",
		mycExeFilePath,
		"-",
	)

	fmt.Println(compileCommand)
	if DEBUG_MESSAGES {
		compileCommand.Stdout = os.Stdout
		compileCommand.Stderr = os.Stderr
	}
	compileCommand.Stdin = strings.NewReader(ir)

	start := time.Now()

	err = compileCommand.Run()

	if err != nil {
		log.Fatalf("Failed while compiling LLVM IR to object. %s", err.Error())
	}

	if DEBUG_MESSAGES {
		fmt.Printf("time: %dms for clang to compile and link.\n", time.Since(start).Milliseconds())
	}

	return mycExeFilePath
}

func run(filename string) {
	ir := genIRFromFile(filename)

	exePath := compileIRToExecutable(ir)

	runCmd := exec.Command(exePath, os.Args...)
	runCmd.Stdout = os.Stdout
	runCmd.Stderr = os.Stderr

	err := runCmd.Run()
	if err != nil {
		log.Fatalf("Failed to run compiled binary.\n%s", err.Error())
	}
}

func copyFile(srcPath string, dstPath string) error {
	in, err := os.Open(srcPath)
	if err != nil {
		return err
	}
	defer in.Close()

	out, err := os.Create(dstPath)
	if err != nil {
		return err
	}
	defer out.Close()

	_, err = io.Copy(out, in)
	if err != nil {
		return err
	}
	return out.Close()
}

func build(filename string, executableName string) {
	ir := genIRFromFile(filename)
	exePath := compileIRToExecutable(ir)
	copyFile(exePath, executableName)
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
					fmt.Println("Compiling:")
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
