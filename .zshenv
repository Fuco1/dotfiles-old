typeset -U path
path=(~/.cabal/bin ~/bin $path)
hosts=($(hostname) aisa.fi.muni.cz dasnet.cz bart.math.muni.cz)

export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/
export EDITOR="emacs -nw"
export XDG_CONFIG_HOME=/home/matus/.config
export SHELL="/usr/bin/zsh"
export TERM="rxvt-unicode"
