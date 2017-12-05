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
