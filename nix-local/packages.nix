# Install with nix-env -f my-packages.nix -i
# Remove/install with nix-env -f my-packages.nix -ir
with import <nixpkgs> {};
[ acpi
  avfs
  cava
  clang
  detox
  emacs-master
  exa
  font-awesome
  ghq
  gimp
  git
  glibc-locales
  gnupg
  google-chrome-beta
  htop
  hub
  i3-gaps
  i3blocks-gaps
  iosevka-master
  libotf
  mpv
  ncdu
  neovim
  networkmanager_dmenu
  nix
  nix-prefetch-git
  nodejs
  otfcc
  parallel
  pass-git-helper
  password-store
  peco-master
  playerctl
  ripgrep-master
  rlwrap
  rofi
  rtv
  sbcl
  termite-master
  tmux
  ttfautohint
  unclutter
  vlc
  xbacklight
  xcape
  youtube-dl
  zathura-with-plugins
  zsh
  zsh-completions ]
