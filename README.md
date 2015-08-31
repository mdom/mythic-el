# Mythic for Emacs

## Introduction

mythic.el provides a major mode to play the role playing game
Mythic by Tom Pigeon. Mythic allows a group of players to improvise
an adventure. It can be used with and without a game
master. The player ask questions about the game world, which are
rsolved by rolling on a table and interpret the results by logic.

Mythic.el streamlines this process by handling the dice rolling and
list management. This documentation deliberatly won't explain the
rules of Mythic, you have to buy the books to learn more about
Mythic.

## Installation

Just download mythic.el and add the following statements to your .emacs:
```
(add-to-list 'load-path "path/to/download/folder")
(require 'mythic)
```

You can use also use the next statement, if you want to open file
with a mythic extension automatically with mythic:
```
(add-to-list 'auto-mode-alist '("\\.mythic\\'" . mythic-mode))
```

## Start a new adventure

You can start a new adventure by calling `M-x mythic`. It will ask
for a filename, that can either be an existing mythic file or a new
file. If its a new file, mythic will ask you for the setup of the
first scene and open the file. Otherwise the file is opened and
mythic displays the last scene you played.

## Odds and resisted questions

Use `C-c C-o` to ask an odds question against the current chaos
factor. A multiple choice buffer with the possible acting ranks
will pop up and the answer to your question will be display in the
echo area. Mythic also provides the shortcut `C-c C-c` for this
action, as you will be mainly using this function to advance your
adventure.

Pressing `C-c C-r` will pop up a buffer, where you can enter the
acting and difficulty rank of your resisted question. Again, the
answer will be shown in the echo area.

## Scene

If you decide a scene is played out, you can advance to the next
scene via `C-c C-a`. Mythic will ask you for the scene setup, if you
want to increase the chaos factor, and for a new setup if its
either a interrupted or modified scene.

Mythic will always narrow the buffer to the current scene. To check
what happend in earlier scenes, `M-p` and `M-n` will bring you to an
earlier or later scene.
