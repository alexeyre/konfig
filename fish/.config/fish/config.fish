direnv hook fish | source
set -Ux EDITOR nvim
set -Ux HOMEBREW_NO_AUTO_UPDATE 1
alias vi nvim
set -U fish_greeting
fish_add_path /opt/homebrew/bin
if status is-interactive
fish_vi_key_bindings
set fish_cursor_default     block      blink
set fish_cursor_insert      line       blink
set fish_cursor_replace_one underscore blink
set fish_cursor_visual      block
end
