Roi's Vimsheet
==============

NORMAL MODE
-----------
Structure of an Editing Command
`<number><command><text object or motion>`

the . operator
--------------
the most important key IMO.

records everything since exiting NORMAL mode until you get back to NORMAL mode so you can repeat it with `.`

the ; and , operator
--------------
repeat to the right/left the forward/torward motions

number
------
The number is used to perform the command over multiple text objects or motions, e.g., backward three words, forward two paragraphs. The number is optional and can appear either before or after the command.

command (or verbs)
------------------
The command is an operation, e.g., change, delete (cut), or yank (copy). The command is also optional; but without it, you only havea motion command, not an edit command

### single characters (not . usable)
* `x` - delete letter
* `r` - replace
* `s` - switch (replace that stays in insert mode)
### usable with motions
* `d` - delete
* `c` - change
* `y` - yank
* `g~` - swap case
* `gu` - make lowercase
* `gU` - make uppercase
* `>` - shift right
* `<` - shift left
* `=` - autoindent
* `!` - filter {motion} line throught an external program

motions and text objects
------------------------
### text objects (a and i)
* `aw` - a word
* `iw` - inner word
* `as` - a sentece
* `is` – inner sentece 
* `ap` - a paragraph
* `ip` - inner paragraph
can also be used with other like `"` `'` `{` `(` `<` etc
### motions
* `0` - beginning of line
* `^` - beginning of non-whitespace
* `$` - end of line
* `h j k l` - left down up right (can also be read as letter)
* `w` - move forward by word
* `b` - move backward by word
* `gg` - first line
* `G` - last line
* `f{char}` - move forward to first character
* `t{char}` = move torwards right before the first character
* `}` - move forward by paragraph or block
* `{` - move backwards by paragraph or block

movement
--------
* `C-u` - up half page
* `C-d` - down half page
* `H` - head of the screen
* `M` - middle of the screen
* `L` - last of the screen
* `%` - find matching brace, paren, etc
* `ma` - mark a line in a file with marker "a"
* `backtick a` - after moving around, go back to the exact position of marker "a"
* `'a` - after moving around, go back to line of marker "a"
* `:marks` - view all the marks
* `''` - go to the last place you were
* `[{` - jump back to the "{" at the beginning of the current code block
* `*` - search for word under the cursor
    * `n` - search again forward
    * `N` - search again backwards
    * `#` - search backwards for word under cursor
    * `/` - search forward
    * `?` - search backward

editing
-------
* `x` - delete char under cursor
* `X` - delete char before cursor
* `A` - add to end of line
* `I` - insert at the beginning of the line
* `dd` - delete line
* `D` - delete from cursor to end of line
* `di'` - delete text inside single quotes
* `yy` - copy line
* `Y` - copy from cursor to end of line
* `cc` - change line
* `C` - change from cursor to end of line
* `cit` - change text inside html tag
* `ci'` - change text inside single quotes
* `ci{` - change text inside curly brackets.
* `ci...` - etc
* `p` - paste after cursor
* `P` = paste before cursor
* `-` add line below
* `O` - add line above
* `.` = repeat last comment
* `r` - replace character
* `R` - replace. (overwrite) (good for columns of text)
* `J` - join line (cursor can be anywhere on line)

INSERT MODE
-----------
* `<C-h>` - delete back one character (backspace)
* `<C-w>` - delete back one word
* `<C-u>` - delete back to start of line
* `<C-[>` - switch to normal mode (like `<ESC>`)
* `<C-o`> - switch to insert normal mode (single normal mode action)
* `<C-r>0` - paste from 0 register (where the yank goes)
* `<C-r><C-p>0` - paste without autoindenting
* `<C-r>=` - lets u do some math and put it
* `<C-v>{123}` - insert character by decimal mode
* `<C-v>u{1234}` - insert character by hexadecimal mode
* `<C-v>{nondigit}` - insert nondigit literally
* `<C-k>{char1}{char2}` - insert character represented by digraph :h digraphs or digraphs-table or digraphs-defualt

VISUAL MODE
-----------
* `v` - visual char mode
* `V` - visual line mode
* `C-v` - block visual mode

### in visual mode
* `"+y` to yank to system clipboard
* `o` to go to other end of visual selection 

REGISTERS
---------
* for NORMAL MODE use `"`
* for INSERT MODE use `CTRL-R`
* `""` - unnamed register (NORMAL MODE `p` takes from there
* `"_` - void register in order to delete without saving to " register
* `"0` or `<CTRL-R>0` - yank register
* `"+` or `<CTRL-R>+` - system clipboard register

OTHER
-----
* `:e ++ff=dos` - remove ^M linebreak to normal linebreak
