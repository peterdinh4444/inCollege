# inCollege COBOL Program

## Prerequisites

- Windows
- GnuCOBOL installed, with the `cobc` command available in `PATH`

## Prepare the input file

The program reads input from the path configured in `inCollege.cob`:

`SELECT INPUT-FILE ASSIGN TO`

Before running the program:

1. Open that text file, or copy another test input file over it.
2. Enter one response per line, in the order the program prompts for responses.
3. Save the file as a plain text file at that exact path relative to the project folder.

To use a different input file permanently, update the `SELECT INPUT-FILE ASSIGN TO` path near the top of `inCollege.cob`.

## Compile and run

From the project folder, run:

```bat
cobc -x -o inCollege.exe inCollege.cob
inCollege.exe
```

Or run the included batch file, which compiles and then runs the program:

```bat
run-inCollege.bat
```

The batch file must be run from any location where the project folder is available; it changes to its own folder before compiling and running.

## Output

The program writes its results to:

`SELECT OUTPUT-FILE ASSIGN TO`

This output file is created in the project folder, alongside `inCollege.cob` and `inCollege.exe`. The file is overwritten each time the program starts.
