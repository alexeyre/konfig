{config, pkgs, lib, ... }:
let 
	dotfilesGit = 
		fetchGit
			https://github.com/alex-eyre/dotfiles.git;
in
{	
	programs.emacs = {
		enable = true;
		package = pkgs.emacsGit;
	};
	services.emacs.enable = true;
	home.file.emacs = {
		source = "${dotfilesGit}/emacs/";
		target = ".config/emacs/";
		recursive = true;
	};
}
