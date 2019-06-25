emacs.d
=======
Clone the reponsitory and link ~/.emacs.d/ (backup an existing directory first):
```
git clone https://github.com/tjeastman/emacs.d.git
ln -s emacs.d ~/.emacs.d
mkdir emacs.d/state
```

### Jedi Setup

Install the jedi server:
```shell
M-x jedi:install-server
```
This command creates a virtualenv in ~/.emacs.d/.python-environments/ and installs the Jedi server there.

### git-modes

Clone the `git-modes` project:
```
mkdir .emacs.d/contrib
cd .emacs.d/contrib
git clone https://github.com/magit/git-modes.git
```

### Keymap

#### Global

Key                    | Description
-----------------------|------------------------------------------------------------
<kbd>C-c & C-n</kbd>   | create a new snippet
<kbd>C-=</kbd>         | expand region
<kbd>C-M-j</kbd>       | end ivy completion
<kbd>C-c e n</kbd>     | goto flycheck next error
<kbd>C-c e p</kbd>     | goto flycheck previous error
<kbd>C-;</kbd>         | backward kill word
<kbd>M-o</kbd>         | goto other window
<kbd>C-:</kbd>         | avy goto char timer
<kbd>M-g g</kbd>       | avy goto line
<kbd>C-c p p</kbd>     | switch projects with projectile
<kbd>C-c p f</kbd>     | find file with projectile
<kbd>C-c p d</kbd>     | find directory with projectile
<kbd>C-c p b</kbd>     | switch project buffer with projectile
<kbd>C-c k</kbd>       | grep for string in current directory with ripgrep
<kbd>C-c ! l</kbd>     | list flycheck errors

#### Docker

Key                    | Description
-----------------------|------------------------------------------------------------
<kbd>C-c C-b</kbd>     | build Docker image

#### Python

Key                    | Description
-----------------------|------------------------------------------------------------
<kbd>C-c w</kbd>       | pyvenv workon
<kbd>C-c a</kbd>       | pyvenv activate
<kbd>C-c C-e</kbd>     | elpy multiedit symbol at point
<kbd>C-c C-t</kbd>     | run tests
<kbd>M-.</kbd>         | goto definition
<kbd>M-,</kbd>         | return from goto definition
<kbd>M-a</kbd>         | navigate backward block
<kbd>M-e</kbd>         | navigate forward block

#### JSON

Key                    | Description
-----------------------|------------------------------------------------------------
<kbd>C-c C-f</kbd>     | reformat region/buffer
