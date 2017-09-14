# Install with nix-env -f my-packages.nix -i
# Remove/install with nix-env -f my-packages.nix -i
with import <nixpkgs> {};
[ acpi
  cava
  chicken
  detox
  emacs-master
  exa
  font-awesome
  ghq
  gimp
  glibc-locales
  google-chrome-beta
  htop
  hub
  iosevka
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
  ripgrep
  rlwrap
  rofi
  rtv
  sbcl
  tmux
  ttfautohint
  unclutter
  vim
  vlc
  xbacklight
  xcape
  youtube-dl
  zathura-with-plugins ]
