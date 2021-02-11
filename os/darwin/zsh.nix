{ ... }: {
	programs.zsh.envExtra = ''
		test -e /Users/alex/.config/zsh/.iterm2_shell_integration.zsh && source /Users/alex/.config/zsh/.iterm2_shell_integration.zsh || true
	'';
}
