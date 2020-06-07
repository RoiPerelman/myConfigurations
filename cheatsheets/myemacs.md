Roi's emacs
===========

* `M-x {$cmd-name}` - execute with command name
* `C-x C-c` - quit emacs

Buffers & Files
---------------
* `C-x b` - switch to or create a buffer
* `C-x C-f` - navigate to open a file in buffer
* `C-x C-s` - save file
* `C-x k` - kill buffer

Windows
-------
* `C-x o` - Switch cursor to another window. Try this now to switch between your Clojure file and the REPL.
* `C-x 1` - Delete all other windows, leaving only the current window in the frame. This doesn’t close your buffers, and it won’t cause you to lose any work.
* `C-x 2` - Split frame above and below.
* `C-x 3` - Split frame side by side.
* `C-x 0` - Delete current window.

Navigation
----------
* `M-m` - Move to first non-whitespace character on the line.
* `C-a` - Move to beginning of line.
* `C-e` - Move to end of line.
* `M-a` - Move to beginning of paragraph.
* `M-e` - Move to end of paragraph.
* `C-f` - Move forward one character.
* `C-b` - Move backward one character.
* `M-f` - Move forward one word.
* `M-b` - Move backward one word.
* `C-n` - Move to next line.
* `C-p` - Move to previous line.
* `C-v` - Move forward one screenful
* `M-v` - Move backward one screenful
* `C-l` - clear screen and redisplay all the text, moving the text around the cursor to the center of the screen 
* `C-s` - Regex search for text in current buffer and move to it. Press C-s again to move to next match.
* `C-r` - Same as C-s, but search in reverse.
* `M-<` - Move to beginning of buffer.
* `M->` - Move to end of buffer.
* `M-g g` -	Go to line.

Kill Ring
---------
* `C-w` - Kill region
* `M-w` - Copy region to kill ring
* `C-y` - Yank
* `M-y` - Cycle through kill ring after yanking
* `M-d` - Kill word
* `C-k` - Kill line

Editing and Help
----------------
* `Tab`	- Indent line
* `C-j` - New line and indent, equivalent to enter followed by tab
* `M-/` - Hippie expand; cycles through possible expansions of the text before point
* `M-\` - Delete all spaces and tabs around point (I use this one a lot)

* `C-h k {$key-binding}` - Describe the function bound to the key binding. To get this to work, you actually perform the key sequence after typing C-h k
* `C-h f` - Describe function

Clojure Cider
-------------
* `C-c C-k` - Compile current buffer.
* `C-x C-e` - Evaluate expression immediately preceding point.
* `C-c M-n` - Switch to namespace of current buffer.
* `C-c C-d C-d` - Display documentation for symbol under point.
* `M-. and M-,` - Navigate to source code for symbol under point and return to your original buffer.
* `C-c C-d C-a` - Apropros search; find arbitrary text across function names and documentation.

Paredit
-------
* `M-x paredit-mode` - Toggle paredit mode.
* `M-(` - Surround expression after point in parentheses (paredit-wrap-round).
* `C-→` - Slurp; move closing parenthesis to the right to include next expression.
* `C-←` - Barf; move closing parenthesis to the left to exclude last expression.
* `C-M-f, C-M-b` - Move to the opening/closing parenthesis.