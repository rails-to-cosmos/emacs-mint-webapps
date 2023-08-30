{ pkgs, ... }:

{
  env.GREET = "devenv";
  packages = with pkgs; [ cask ];
  scripts.run-test.exec = "cask exec buttercup -L .";
  enterShell = ''
    cask
  '';
}
