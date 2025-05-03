# install

Needs .config/emacs

```sh
git clone https://github.com/hlissner/doom-emacs $XDG_CONFIG_HOME/emacs


cp $XDG_CONFIG_HOME/emacs/static/init.example.el ./dot-config/doom/init.el
cp $XDG_CONFIG_HOME/emacs/static/config.example.el ./dot-config/doom/config.el
cp $XDG_CONFIG_HOME/emacs/static/packages.example.el ./dot-config/doom/packages.el
```


write out the paths/paths.d file with eval'd $XDG_CONFIG_HOME/emacs/bin
PATH="$HOME/.emacs.d/bin:$PATH"
