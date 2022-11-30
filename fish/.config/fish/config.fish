set -Ux EDITOR nvim
alias vi nvim
set -U fish_greeting
fish_add_path /opt/homebrew/bin
if status is-interactive
and not set -q TMUX
    exec tmux
end
fish_vi_key_bindings
set fish_cursor_default     block      blink
set fish_cursor_insert      line       blink
set fish_cursor_replace_one underscore blink
set fish_cursor_visual      block
if status is-interactive
    # Commands to run in interactive sessions can go here
end
