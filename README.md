emacs.d
=======
Clone the reponsitory and link ~/.emacs.d/ (backup an existing directory first):
```
git clone https://github.com/tjeastman/emacs.d.git
ln -s emacs.d ~/.emacs.d
```

Setup Pymacs:
```
cd ~/.emacs.d/contrib/Pymacs
make
python setup.py install --user
```
