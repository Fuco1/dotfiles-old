# dotfiles-old

**TLDR** new dotfiles are [here](https://github.com/Fuco1/dotfiles).

---

These were the dotfiles managed by having `.git` directly inside my
`$HOME`.  I have since migrated to using [GNU
Stow](https://www.gnu.org/software/stow/) to manage dotfiles in a
separate directory and merely symlink them to where they need to be.
This also allows you to only selectively install whatever
configuration you want, for example by doing `stow fish` you will only
get the [fish shell](https://fishshell.com/) config.
