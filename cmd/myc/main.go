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

func CreatePreludeObject(dir string) string {
	prelude := `
		#include <stdio.h>
		#include <stdlib.h>

		int printf(const char*, ...);
		int putchar(int);
		void* malloc(size_t);
		void free(void*);
		void exit(int);
	`

	objPath := filepath.Join(dir, "prelude.o")
	compileCommand := exec.Command(
		CLANG_EXECUTABLE_PATH,
		"-c",
		"-o",
		objPath,
		"-x",
		"c",
		"-",
	)
	compileCommand.Stdin = strings.NewReader(prelude)

	if DEBUG_MESSAGES {
		fmt.Println("Compiling prelude object...")
		fmt.Println(compileCommand)
		compileCommand.Stdout = os.Stdout
		compileCommand.Stderr = os.Stderr
	}

	err := compileCommand.Run()
	if err != nil {
		log.Fatalf("Failed to compile prelude object.\n%s", err.Error())
	}

	return objPath
}

func CreateRuntimeObject(dir string) string {
	runtimeCode := llvmgen.GenRuntime()

	objPath := filepath.Join(dir, "runtime.o")
	compileCommand := exec.Command(
		CLANG_EXECUTABLE_PATH,
		"-c",
		"-o",
		objPath,
		"-x",
		"ir",
		"-",
	)
	compileCommand.Stdin = strings.NewReader(runtimeCode)

	if DEBUG_MESSAGES {
		fmt.Println("Compiling runtime object...")
		fmt.Println(compileCommand)
		compileCommand.Stdout = os.Stdout
		compileCommand.Stderr = os.Stderr
	}

	err := compileCommand.Run()
	if err != nil {
		log.Fatalf("Failed to compile runtime object.\n%s", err.Error())
	}

	return objPath

}

func CreateModuleFromPath(filename string) *ast.Module {
	start := time.Now()

	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatalf("Failed while reading source file.\n%s", err.Error())
	}

	readFileTime := time.Now()
	if DEBUG_MESSAGES {
		fmt.Printf("%s: time: %dus to read source file\n", filepath.Base(filename), readFileTime.Sub(start).Microseconds())
	}

	absPath, err := filepath.Abs(filename)
	if err != nil {
		log.Fatalf("Failed while getting source file's absolute path.\n%s", err.Error())
	}

	module := &ast.Module{
		Name:    "",
		Path:    absPath,
		Source:  string(bytes),
		Imports: make(map[string]*ast.Module),
	}

	lexer.Lex(module)
	parser.Parse(module)
	analyzer.Analyze(module)

	lpaTime := time.Now()
	if DEBUG_MESSAGES {
		fmt.Printf("%s: time: %dus to lex, parse, and analze\n", filepath.Base(module.Path), lpaTime.Sub(readFileTime).Microseconds())
	}

	return module
}

// Compiles the provided module to an object file and returns the path of the output object file
func CompileModule(dir string, module *ast.Module) string {
	start := time.Now()

	ir := llvmgen.Gen(module)
	irGenTime := time.Now()

	if DEBUG_MESSAGES {
		fmt.Printf("%s: time: %dus to generate LLVM IR\n", filepath.Base(module.Path), irGenTime.Sub(start).Microseconds())
	}

	objFilePath := filepath.Join(dir, fmt.Sprintf("myc-%s.o", module.Name))

	compileCommand := exec.Command(
		CLANG_EXECUTABLE_PATH,
		"-x",
		"ir",
		"-c",
		"-o",
		objFilePath,
		"-",
	)

	if DEBUG_MESSAGES {
		fmt.Println(compileCommand)
		compileCommand.Stdout = os.Stdout
		compileCommand.Stderr = os.Stderr
	}
	compileCommand.Stdin = strings.NewReader(ir)

	err := compileCommand.Run()
	clangTime := time.Now()

	if err != nil {
		log.Fatalf("Failed while compiling LLVM IR to object.\n%s", err.Error())
	}

	if DEBUG_MESSAGES {
		fmt.Printf("%s: time: %dms for clang to compile LLVM IR to object.\n", filepath.Base(module.Path), clangTime.Sub(irGenTime).Milliseconds())
	}

	return objFilePath
}

func addModuleToListRecursively(module *ast.Module, moduleList *[]*ast.Module) {
	inList := false
	for i, m := range *moduleList {
		if m.Path == module.Path {
			inList = true
			module.Name = fmt.Sprintf("_%d", i)
		}
	}

	if !inList {
		module.Name = fmt.Sprintf("_%d", len(*moduleList))
		*moduleList = append(*moduleList, module)
	}

	for _, imp := range module.Imports {
		addModuleToListRecursively(imp, moduleList)
	}
}

func flattenAndMangleModuleDependencies(module *ast.Module) []*ast.Module {
	var moduleList []*ast.Module

	for _, dep := range module.Imports {
		addModuleToListRecursively(dep, &moduleList)
	}

	return moduleList
}

func CompileAndLinkModule(module *ast.Module, linkerFlags string) string {
	tmpDir, err := ioutil.TempDir("", "myc-tmp--*")
	if err != nil {
		log.Fatalf("Failed while creating temp directory.\n%s", err.Error())
	}

	module.Name = "main"
	deps := flattenAndMangleModuleDependencies(module)

	exePath := filepath.Join(tmpDir, "myc-exe-.out")
	mainObjPath := CompileModule(tmpDir, module)
	preludeObjPath := CreatePreludeObject(tmpDir)
	runtimeObjPath := CreateRuntimeObject(tmpDir)

	objs := []string{mainObjPath}
	for _, dep := range deps {
		objs = append(objs, CompileModule(tmpDir, dep))
	}

	args := []string{"-o", exePath, preludeObjPath, runtimeObjPath}
	args = append(args, linkerFlags)
	args = append(args, objs...)

	linkCommand := exec.Command(
		CLANG_EXECUTABLE_PATH,
		args...,
	)

	if DEBUG_MESSAGES {
		fmt.Println(linkCommand)
		linkCommand.Stdout = os.Stdout
		linkCommand.Stderr = os.Stderr
	}

	err = linkCommand.Run()
	if err != nil {
		log.Fatalf("Failed while linking objects to executable.\n%s", err.Error())
	}

	return exePath
}

func run(filename string, linkerFlags string) {
	exePath := CompileAndLinkModule(CreateModuleFromPath(filename), linkerFlags)

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

func build(filename string, executableName string, linkerFlags string) {
	exePath := CompileAndLinkModule(CreateModuleFromPath(filename), linkerFlags)
	copyFile(exePath, executableName)
}

func main() {
	var executableOutputFile string
	var linkerFlags string

	if cc := os.Getenv("MYC_CC"); cc != "" {
		CLANG_EXECUTABLE_PATH = cc
	}

	app := &cli.App{
		Name:  "myc",
		Usage: "A simple programming language (WIP).",
		Flags: []cli.Flag{
			&cli.StringFlag{
				Name:        "linker-flags",
				Value:       "",
				Usage:       "Pass flags directly to the linker (helpful to link external dependencies)",
				Destination: &linkerFlags,
			},
		},
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
					run(filename, linkerFlags)
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
					build(filename, executableOutputFile, linkerFlags)
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
