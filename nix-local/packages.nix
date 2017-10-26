# Install with nix-env -f my-packages.nix -i
# Remove/install with nix-env -f my-packages.nix -ir
with import <nixpkgs> {};
[ acpi
  avfs
  cava
  chicken
  llvmPackages.clang-unwrapped
  cmus
  detox
  emacs-master
  exa
  ghq
  gimp
  gitMinimal
  glibcLocales
  gnupg
  htop
  gitAndTools.hub
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
  nodejs-8_x
  otfcc
  pandoc
  pass-git-helper-master
  pass
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
  xorg.xbacklight
  xcape
  youtube-dl
  zathura
  zsh
  zsh-completions ]
