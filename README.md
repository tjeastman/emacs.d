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


### Keymap

#### Global

Keybinding             | Description
-----------------------|------------------------------------------------------------
<kbd>C-c & C-n</kbd>   | create a new snippet
<kbd>C-=</kbd>         | expand region
<kbd>C-M-j</kbd>       | end ivy completion
<kbd>C-c e n</kbd>     | goto flycheck next error
<kbd>C-c e p</kbd>     | goto flycheck previous error
<kbd>C-;</kbd>         | backward kill word
<kbd>M-o</kbd>         | goto other window

#### Python

Keybinding             | Description
-----------------------|------------------------------------------------------------
<kbd>C-c w</kbd>       | pyvenv workon
<kbd>C-c a</kbd>       | pyvenv activate
<kbd>C-c C-e</kbd>     | elpy multiedit symbol at point
<kbd>M-.</kbd>         | goto definition
