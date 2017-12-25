Roi's bashsheet
===============

- `#!/bin/bash` - lets the execution know with what to execute

## Special Variables
* `$0` - The name of the Bash script.
* `$1 - $9` - The first 9 arguments to the Bash script. (As mentioned above.)
* `$#` - How many arguments were passed to the Bash script.
* `$@` - All the arguments supplied to the Bash script.
* `$?` - The exit status of the most recently run process.
* `$$` - The process ID of the current script.
* `$USER` - The username of the user running the script.
* `$HOSTNAME` - The hostname of the machine the script is running on.
* `$SECONDS` - The number of seconds since the script was started.
* `$RANDOM` - Returns a different random number each time is it referred to.
* `$LINENO` - Returns the current line number in the Bash script.

## Variables
* `varaible=value` - To set a value for a variable. Remember, no spaces on either side of =
* `Quotes " '` - Double will do variable substitution, single will not.
* `variable=$( command )` - Save the output of a command into a variable (example: `myvar=$( ls /etc | wc -l )`)
* `export var1`- Make the variable var1 available to child processes.
* `read varName` - Read input from the user and store it in the variable varName.
* `/dev/stdin` - A file you can read to get the STDIN for the Bash script (example: `echo roi | cat /dev/stdin`)
    * STDIN - /dev/stdin or /proc/self/fd/0
    * STDOUT - /dev/stdout or /proc/self/fd/1
    * STDERR - /dev/stderr or /proc/self/fd/2
* `let <arithmetic expression>` - Make a variable equal to an expression. (example: `let a=5+4` echo $a # 9)
    * `+, -, /*, /` - addition, subtraction, multiply, divide
    * `var++` - Increase the variable var by 1
    * `var--` - Decrease the variable var by 1
    * `%` -Modulus (Return the remainder after division)
* `expr item1 operator item2` - like let only print out the result of the expression.
* `$(( expression ))` - Return the result of the expression. *recommended*
* `${#var}` - Return the length of the variable var.

## Statements
* `if statement`
``` bash
if [ <some test> ]
then
  <commands>
elif [ <some test> ] 
then
  <different commands>
else
  <other commands>
fi
```
* `case statement`
``` bash
case <variable> in
<pattern 1>)
  <commands>
;;
<pattern 2>)
  <other commands>
;;
esac
```

## loops
* `while loop`
``` bash
while [ <some test> ]
do
  <commands>
done
```
* `until loop`
``` bash
until [ <some test> ]
do
  <commands>
done
```
* `for loop`
``` bash
for var in <list>
do
  <commands>
done
```
* `ranges` - can provide a list for for loops (example: `for value in {1..5} do...`)
* `break` - leave the loop strait away
* `continue` - continue to the next iteration of the loop
* `select loop` - like for loop with `select` instead of `for`. Display a simple menu system for selecting items from a list.

## Test
`test` - command is the command line version of `[]` in bash scripts
* `! EXPRESSION` - The EXPRESSION is false.
* `-n STRING` - The length of STRING is greater than zero.
* `-z STRING` - The lengh of STRING is zero (ie it is empty).
* `STRING1 = STRING2` - STRING1 is equal to STRING2
* `STRING1 != STRING2` - STRING1 is not equal to STRING2
* `INTEGER1 -eq INTEGER2` - INTEGER1 is numerically equal to INTEGER2
* `INTEGER1 -gt INTEGER2` - INTEGER1 is numerically greater than INTEGER2
* `INTEGER1 -lt INTEGER2` - INTEGER1 is numerically less than INTEGER2
* `-d FILE`	- FILE exists and is a directory.
* `-e FILE`	- FILE exists.
* `-r FILE`	- FILE exists and the read permission is granted.
* `-s FILE`	- FILE exists and it's size is greater than zero (ie. it is not empty).
* `-w FILE`	- FILE exists and the write permission is granted.
* `-x FILE` - FILE exists and the execute permission is granted.
* `and` - &&
* `or` - ||

## Functions
* 
``` bash
function_name () {
  <commands>
}
# or
function function_name {
  <commands>
}
```
* arguments are passed with `$1 - $9`
* `return <value>` - Exit the function with a return status of value. 
* `local var_name=<var_value>` - Create a local variable within a function.
``` bash
# important example
print_something () {
  echo Hello $1 # we can get this with evaluating the function
  return 5 # status code
}

# in order to get result of the output of a function into a variable
lines_in_file () {
  cat $1 | wc -l
}
num_lines=$( lines_in_file $1 )
```

## AWK - text processor
* `awk '{}' FILE` - this is the template. Inside the '{}' we can give awk a lot of information
  * `'{ print }` - prints every line to the screen
  * `'{ print leanth($1),$2.$3 }` - print length of first colums (`,` means) space second column (`.` means) *no space* third column of every line
  * `'\[0-9]$\ { print }` - print every line that has the regex in between `\`. this instance it is lines that end with a number.
  * `{ if($1 ~ /test/) print }` - print line only if the first column is equeal to the regex exp (test here)
  * `awk -F":" '{}' FILE` - change the delimeter with -F (here to :)
  * `awk 'NR==1,NR==10{ printf "%-8s %3d\n, $1, $3 }...'` - print between lines 1 - 10 with specific format (not explained)
  * `awk 'BEGIN { print roi } { print }` - would start by printing roi and after printing every row
  * `awk '/test/ BEGIN { sum=0, count=0, print roi } { sum+=$1, count++ } { print } END { print count }'` - BEGIN would do once before processing every line. End would do once after all processing. We can create a variable name counter and add to it every line.


## SED - text processor
* `sed 's/A/B/${flag}' ${file}` - sed substitutes every line's *first* A by B (A can be a regex)
  * `sed -i...` - change the original file
  * `s/A/B/g` - means change globally (all A in line by B's)
  * `s/[0-9]/(&)/g` - change every number by a parenthesis version of himself (& holds the found regex)
  * `s/[0-9]*/(&)/g` - searches for *0* or more occurances of numbers 0-9.
  * `s/[0-9][0-9]*/(&)/g`- searches for *1* or more occurances of numbers 0-9. 